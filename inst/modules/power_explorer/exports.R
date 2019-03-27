power_3d_fun = function(brain){
  showNotification(p('Generating in progress, need some time...'))

  dat = rave::cache(key = list(
    list(BASELINE_WINDOW, preload_info)
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

  brain$view(value_range = c(-1,1) * max(abs(values)))
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
     # key = list(subject$id, preload_info$electrodes, BASELINE_WINDOW, preload_info),
     # val = baseline(power, BASELINE_WINDOW[1],  BASELINE_WINDOW[2], hybrid = FALSE, mem_optimize = FALSE)
  # )

  .bl_power <- baseline(power, BASELINE_WINDOW[1],  BASELINE_WINDOW[2], hybrid = FALSE, mem_optimize = FALSE)

  # subset out the trials, frequencies, and time rane
  .power <- .bl_power$subset(Frequency = Frequency %within% FREQUENCY,
                             Time = Time %within% ANALYSIS_WINDOW,
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

input <- getDefaultReactiveInput()
graph_export = function(){
  actionLink(ns('btn_graph_export'), 'Export Graphs')
}


observeEvent(input$btn_graph_export, {
  print(123)
  module = with(globalenv(), {
    module = rave::get_module('ravebuiltins', 'power_explorer', local = T)
    module
    
  })
  
  pdf('~/Desktop/hmp_e' %&% deparse_selections(preload_info$electrodes, max_lag=10) %&% '.pdf', w=6, h=4, useDingbats = FALSE)
  for(e in preload_info$electrodes){
    result = module(ELECTRODE=e)
    result$heat_map_plot()
  }
  
  dev.off()
  
})

export_graphs <- function(conn=NA, lbl='png_out', dir, ...) {
  
  module = rave::get_module('ravebuiltins', 'power_explorer', local = T)
  
  pdf('~/Desktop/hmp_e' %&% deparse_selections(preload_info$electrodes, max_lag=10) %&% '.pdf', w=6, h=4, useDingbats = FALSE)
  for(e in preload_info$electrodes){
    result = module(ELECTRODE=e)
    result$heat_map_plot()
  }
  dev.off()
  # 
  # pdf('hmp.pdf')
  # result$heat_map_plot()
  # dev.off()
  # 
  # pdf('otp.pdf')
  # result$line_plot()
  # dev.off()
  
  
  # .dir <- module_tools$get_subject_dirs()$module_data_dir %&%  '/condition_explorer/png_out/'
  # dir.create(.dir, recursive = TRUE)

  # pow <- module_tools$get_power(force = T, referenced = T)

  #TODO check the variable export_per_electrode to see if we need to loop over electrodes and export
  # or if we want use just the current_electrodes and combine them

  #TODO need to scale all the fonts etc so things aren't too large for export

  # as_pdf(.dir %&% 'line_plot_el', w = 5, h=3, {
  #   over_time_plot()
  # } )
}
