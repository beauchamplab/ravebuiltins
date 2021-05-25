input <- getDefaultReactiveInput()
output = getDefaultReactiveOutput()

power_3d_fun = function(brain){
  showNotification(p('Generating 3d viewer...'))
  
  # brain = rave::rave_brain2(); 
  brain$load_surfaces(subject = subject, surfaces = c('pial', 'white', 'smoothwm'))
  
  dat = rave::cache(key = list(
    list(baseline_window, preload_info)
  ), val = get_summary())
  
  
  
  
  # for each electrode, we want to test the different conditions
  .FUN <- if(length(levels(dat$condition)) > 1) {

    if (length(levels(dat$condition)) == 2) {
      function(x) {
        res = get_t(power ~ condition, data=x)
        res = c(res[1] - res[2], res[3], res[4])
        res %>% set_names(c('b', 't', 'p'))
      }
    } else {
      function(x) {
        get_f(power ~ condition, data=x)
      }
    }
  } else {
    function(x) {
      get_t(x$power) %>% set_names(c('b', 't', 'p'))
    }
  }

  values = sapply(unique(dat$elec), function(e){
    sub = dat[dat$elec == e, ]
    re = .FUN(sub)
    v = re[input$viewer_3d_type]
    brain$set_electrode_value(subject, e, v)
    return(v)
  })
  
  brain$view(value_range = c(-1,1) * max(abs(values)),
             color_ramp = rave_heat_map_colors)
}

# Export functions
get_summary <- function() {
  # here we just want an estimate of the power at each trial for each electrode
  # get the labels for each trial

  ..g_index <- 1
  GROUPS = lapply(GROUPS, function(g){
    g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% unlist(g$group_conditions)]

    if(g$group_name == '') {
      g$group_name <-  LETTERS[..g_index]
      ..g_index <<- ..g_index + 1
    }

    return(g)
  })
  rm(..g_index)

  tnum_by_condition <- sapply(GROUPS, function(g) {
    list(g$Trial_num)
  }) %>% set_names(sapply(GROUPS, '[[', 'group_name'))

  all_trials <- unlist(tnum_by_condition)
   # .bl_power <- cache(
     # key = list(subject$id, preload_info$electrodes, baseline_window, preload_info),
     # val = baseline(power, baseline_window[1],  baseline_window[2], hybrid = FALSE, mem_optimize = FALSE)
  # )

  .bl_power <- baseline(power, baseline_window[1],  baseline_window[2], hybrid = FALSE, mem_optimize = FALSE)

  # subset out the trials, frequencies, and time rane
  .power <- .bl_power$subset(Frequency = Frequency %within% FREQUENCY,
                             Time = Time %within% analysis_window,
                             Trial = Trial %in% all_trials, data_only = FALSE)

  stimulus <- epoch_data$Condition[as.numeric(.power$dimnames$Trial)]

  condition <- .power$dimnames$Trial %>% as.numeric %>% sapply(function(tnum) {
    #ensure only one group is ever selected? or we could throw an error on length > 1
    sapply(tnum_by_condition, `%in%`, x=tnum) %>% which %>% extract(1)
  }) %>% names

  # rutabaga over Freq and Time
  # by_elec <- rutabaga::collapse(.power$data, keep=c(1,4)) / prod(.power$dim[2:3])
  by_elec <- .power$collapse(keep = c(1,4), method = 'mean')

  data.frame('subject_id' = subject$id,
             'elec' = rep(preload_info$electrodes, each=length(condition)),
             'trial' = rep(seq_along(condition), times=length(preload_info$electrodes)),
             'condition' = rep(condition, length(preload_info$electrodes)),
             'power' = c(by_elec)
  )
}

export_stats = function(conn=NA, lbl='stat_out', dir, ...){
  out_dir <- dir #module_tools$get_subject_dirs()$module_data_dir %&% '/condition_explorer/'

  if(!dir.exists(out_dir))    {
    dir.create(out_dir, recursive = TRUE)
  }

  if(is.na(conn)) {
    fout <- out_dir %&% lbl %&% '.RDS'
  } else {
    fout <- conn #out_dir %&% conn
  }


  # run through all the active electrodes and get the data
  # out_data <- lapply_async(electrodes, process_for_stats)

  out_data <- get_summary()

  saveRDS(out_data, file = fout)

  invisible(out_data)
}

graph_export = function(){
  tagList(
    # actionLink(ns('btn_graph_export'), 'Export Graphs'),
    downloadLink(ns('btn_graph_download'), 'Download Graphs')
  )
}

output$btn_graph_download <- downloadHandler(
  filename = function(...) {
    paste0('power_explorer_export',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.zip')
  },
  content = function(conn){
    tmp_dir = tempdir()
    
    # map the human names to the function names
    function_map <- list('Spectrogram' = 'heat_map_plot',
                         'By Trial Power' = 'by_trial_heat_map',
                         'Over Time Plot' = 'over_time_plot', 
                         'Windowed Average' = 'windowed_comparison_plot')

    to_export <- function_map[plots_to_export]
    prefix <- sprintf('%s_%s_%s_', subject$subject_code, subject$project_name, format(Sys.time(), "%b_%d_%Y_%H_%M_%S"))
        
    fnames <- function_map[plots_to_export]
    
    tmp_files <- prefix %&% str_replace_all(names(fnames), ' ', '_') %&% '.pdf'
    
    mapply(export_graphs, file.path(tmp_dir, tmp_files), fnames)
    
    wd = getwd()
    on.exit({setwd(wd)})
    
    setwd(tmp_dir)
    
    zip(conn, files = tmp_files, flags='-r2X')
  }
)

export_graphs <- function(conn=NA,
                          which_plot=c('heat_map_plot','by_trial_heat_map','over_time_plot', 'windowed_comparison_plot'), ...) {
  
  which_plot <-  match.arg(which_plot)
  
  args = isolate(reactiveValuesToList(input))
  
  electrodes_loaded = preload_info$electrodes
  # check to see if we should loop over all electrodes or just the current electrode
  if(export_what == 'Current Selection') {
    electrodes_loaded <- ELECTRODE
  }
  
  progress = rave::progress('Rendering graphs for: ' %&% str_replace_all(which_plot, '_', ' '),
                            max = length(electrodes_loaded) + 1)
  
  on.exit({progress$close()}, add=TRUE)
  progress$inc(message = 'Initializing')
  
  .export_graph = function(){
    module = rave::get_module('ravebuiltins', 'power_explorer', local = TRUE)
    
    formal_names = names(formals(module))
    args = args[formal_names]
    names(args) = formal_names
    
    # having issues here with the size of the plots being too large for the font sizes
    # we can't (easily) change the cex being used by the plots. So maybe we can 
    # just change the size of the output PDF. people can the resize
    
    # based on the number of groups we should scale the plots
    ngroups = 0
    for(ii in seq_along(args$GROUPS)) {
      if(length(args$GROUPS[[ii]]$group_conditions)>1) {
        ngroups = ngroups+1
      }
    }
    
    w_scale = h_scale = 1
    if(which_plot == 'windowed_comparison_plot') {
      w_scale = ngroups / 2.25
    }
    
    if(which_plot %in% c('by_trial_heat_map', 'heat_map_plot')) {
      w_scale = ngroups*1.25
      h_scale = ngroups*1.05
    }
    
    .w <- round(9.75*w_scale,1)
    .h <- round(6.03*h_scale,1)
    
    pdf(conn, width = .w, height = .h, useDingbats = FALSE)
    
    on.exit(dev.off())
    
    for(e in electrodes_loaded){
      progress$inc(message = sprintf('Electrode %s', e))
      args[['ELECTRODE']] = e
      result = do.call(module, args)
      result[[which_plot]]()
    }
  }
  
  .export_graph()
  
  # showNotification(p('Export graph finished.'))
  
  #TODO check the variable export_per_electrode to see if we need to loop over electrodes and export
  # or if we want use just the current_electrodes and combine them
  
  #TODO need to scale all the fonts etc so things aren't too large for export
  
}
