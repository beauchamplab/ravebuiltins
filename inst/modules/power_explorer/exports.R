input <- getDefaultReactiveInput()
output = getDefaultReactiveOutput()

power_3d_fun = function(need_calc, side_width, daemon_env, viewer_proxy, ...){
  
  showNotification(p('Generating 3d viewer...'))
  brain = rave::rave_brain2(subject = subject);
  
  if(is.null(brain)){
    rave::close_tab('power_explorer', 'Results on surface')
  }
  
  shiny::validate(shiny::need(!is.null(brain), message = 'No surface/volume file found!'))
  re = NULL
  
  display_side = isTRUE(isolate(input$power_3d_widget_side_display))
  
  zoom_level = shiny::isolate(viewer_proxy$main_camera$zoom)
  controllers = viewer_proxy$get_controllers()
  # bgcolor = ifelse('dark' %in% rave::get_rave_theme()$themes, '#1E1E1E', '#FFFFFF')
  
  bgcolor = input$background_plot_color_hint
  if(bgcolor == 'Gray') {
    bgcolor = '#1E1E1E'
  } else {
    bgcolor %<>% col2hex
  }
  if(any(c('#000000', '#1E1E1E', '#FFFFFF') %in% controllers[['Background Color']]) || !length(controllers[['Background Color']])){
    controllers[['Background Color']] = bgcolor
  }
  
  if(input$synch_to_3dviewer) {
    
    # set the display to mean and the threshold to t-score
    
    # controllers[['Display Data']] = 
  }
  
  
  if( need_calc ){
    or = cache(name='omnibus_results')
    
    values = data.frame(t(or))
    values$Subject = as.factor(subject$subject_code)
    values$Electrode = as.numeric(colnames(or))
    values$Time = 0
    
    brain$set_electrode_values(values)
    assign('omnibus_results', omnibus_results, globalenv())
    
    # check to see if we've udpated the dependent variable. We do this by comparing this list of Possible DVs with the 
    # actual current DV
    curr_dv =  controllers[['Display Data']]
    new_dv = rownames(or)[1]
    is_old_dv = curr_dv %in% str_subset(format_unit_of_analysis_name(get_unit_of_analysis(names=T)),
                                        new_dv, negate = TRUE)
    
    if(!length(controllers[['Display Data']]) || controllers[['Display Data']] == '[None]' || is_old_dv) {
      controllers[['Display Data']] = new_dv
      v = ceiling(max(abs(or[1,])) )
      controllers[['Display Range']] = sprintf('-%s,%s', v, v )
    }
    
    if(!length(controllers[['Threshold Data']]) || controllers[['Threshold Data']] == '[None]'){
      controllers[['Threshold Data']] = rownames(or)[2]
      v = 1+ceiling(max(abs(or[2,])) )
      controllers[['Threshold Range']] = sprintf('-%s,-2|2,%s', v, v)
      # threshold format: -10,2|2,10
    } else if(is_old_dv & controllers[['Threshold Data']] == 't') {
      # we want to update the threshold range just in case
      old_range = controllers[['Threshold Range']]
      
      if(str_detect(old_range, '|')) {
        rng = str_split(str_split(old_range, '\\|')[[1]], ',')
        new_mx = max(c(rng[[1]] %>% as.numeric %>% abs, 1+ceiling(max(abs(or[2,])) )))
        controllers[['Threshold Range']] = sprintf('-%s,%s|%s,%s', new_mx, rng[[1]][2], rng[[2]][1], new_mx)
      }
    }
    
    ### maybe we don't always want to show legend...
    controllers[['Show Legend']] = TRUE
    pals = list('dv' = cache(name='current_rave_heatmap_palette'),
                t = cache(name='current_rave_heatmap_palette'),
                p = c('yellow', 'yellow', 'white')
    )
    names(pals)[1] = new_dv
    
    re = brain$plot(symmetric = 0, palettes = pals,
                    val_ranges = list(
                      'p' = c(0,1),
                      'FDR.p.' = c(0,1),
                      'Bonf.p.' = c(0,1)
                    ),
                    side_width = side_width / 2, side_canvas = TRUE, 
    side_display = display_side, start_zoom = zoom_level, controllers = controllers,
    control_presets = 'syncviewer', timestamp = FALSE)
    
  }else{
    # optional, if you want to change the way 3D viewer looks in additional tab
    daemon_env$widget = brain$plot(side_width = side_width, side_canvas = FALSE, side_display = display_side)
    
    # Just initialization, no need to show sidebar
    re = brain$plot(side_width = side_width / 2, side_canvas = TRUE, side_display = display_side,
                    control_presets = 'syncviewer', timestamp = FALSE)
  }
  
  re
  
  # brain$view(value_range = c(-1,1) * max(abs(values)),
  #            color_ramp = rave_heat_map_colors, side_shift = c(-265, 0))
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
  
  .bl_power <- baseline(power, BASELINE_WINDOW[1], BASELINE_WINDOW[2], hybrid = FALSE, mem_optimize = FALSE)
  
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

# export_stats = function(conn=NA, lbl='stat_out', dir, ...){
#   out_dir <- dir #module_tools$get_subject_dirs()$module_data_dir %&% '/power_explorer/'
# 
#   if(!dir.exists(out_dir))    {
#     dir.create(out_dir, recursive = TRUE)
#   }
# 
#   if(is.na(conn)) {
#     fout <- out_dir %&% lbl %&% '.RDS'
#   } else {
#     fout <- conn #out_dir %&% conn
#   }
# 
# 
#   # run through all the active electrodes and get the data
#   # out_data <- lapply_async(electrodes, process_for_stats)
# 
#   out_data <- get_summary()
# 
#   saveRDS(out_data, file = fout)
# 
#   invisible(out_data)
# }


## modified from downloadButton
fix_font_color_button <- function (outputId, label = "Download", class = NULL, ...)  {
  aTag <- tags$a(id = outputId, class = paste("btn shiny-download-link", 
                                              class), href = "", target = "_blank", download = NA, 
                 icon("download"), label, ...)
}

graph_export = function(){
  tagList(
    fix_font_color_button(ns('btn_graph_download'), 'Download graphs and their data', icon=shiny::icon('download'),
                          class = 'btn-primary text-white')
  )
}

download_electrodes_csv <- function() {
  tagList(downloadLink(ns('btn_electrodes_meta_download'), 'Download copy of meta data for all electrodes'),
          tags$p('   ', style='margin-bottom:20px'))
}

output$btn_electrodes_meta_download <- downloadHandler(
  filename=function(...) {
    paste0('electrodes_meta_data_copy_',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.csv')
  },
  content = function(conn) {
    write.csv(module_tools$get_meta('electrodes'), file=conn)
  }
)

output$btn_graph_download <- downloadHandler(
  filename = function(...) {
    paste0('power_explorer_export',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.zip')
  },
  content = function(conn) {
    tmp_dir = tempdir()
    
    # map the human names to the function names
    function_map <- list('Spectrogram' = 'heat_map_plot',
                         'By Trial Power' = 'by_trial_heat_map_plot',
                         'Over Time Plot' = 'over_time_plot', 
                         'Windowed Average' = 'windowed_comparison_plot',
                         'Over Time by Electrode' = 'by_electrode_heat_map_plot',
                         'Results by Electrode' = 'across_electrode_statistics_plot')
    
    to_export <- function_map[plots_to_export]
    prefix <- sprintf('%s_%s_%s_', subject$subject_code, subject$project_name, format(Sys.time(), "%b_%d_%Y_%H_%M_%S"))
    
    fnames <- function_map[plots_to_export]
    
    tmp_files <- prefix %&% str_replace_all(names(fnames), ' ', '_') %&% '.pdf'
    
    print('writing out graphs')
    write_out_graphs(conns=file.path(tmp_dir, tmp_files), plot_functions=fnames,
                     dir=tmp_dir, prefix=prefix)    
    wd = getwd()
    on.exit({setwd(wd)}, add = TRUE)
    
    setwd(tmp_dir)
    print('making zip file')
    zip(conn, files = list.files(pattern = prefix %&% '+.+(pdf|json)'), flags='-r2X')
  }
)

write_out_graphs <- function(conns=NA, plot_functions, dir, prefix, ...) {
  args = isolate(reactiveValuesToList(input))
  
  assign('..args', args, envir = globalenv())
  # args = ..args
  # attachDefaultDataRepository()
  electrodes_loaded = preload_info$electrodes
  # check to see if we should loop over all electrodes or just the currently selected electrode(s)
  if(export_what == 'Current Selection') {
    electrodes_loaded <- requested_electrodes
  }
  
  progress = rave::progress('Rendering graphs...',
                            max = length(electrodes_loaded) + 2)
  
  on.exit({progress$close()}, add=TRUE)
  progress$inc('Initializing')
  
  rave::rave_context('rave_running_local')
  module = rave::get_module('ravebuiltins', 'power_explorer', local = TRUE)
  formal_names = names(formals(module))
  if(F) {
    args = sapply(formal_names, function(fn) {
      results$get_value(fn)
    })
  }
  args = args[formal_names]
  names(args) = formal_names
  
  # so we want to open all the PDFs initially
  # based on the number of groups we should scale the plots
  ngroups = 0
  for(ii in seq_along(args$GROUPS)) {
    if(length(args$GROUPS[[ii]]$group_conditions)>1) {
      ngroups = ngroups+1
    }
  }
  # having issues here with the size of the plots being too large for the font sizes
  # we can't (easily) change the cex being used by the plots. So maybe we can 
  # just change the size of the output PDF. people can then resize but keep the relative sizes correct
  # FIXME the sizes of these outputs aren't very pretty. we should probably just hard code a list for common sizes, say
  # ngroup <= 4... then apply a scale factor after that? We also need to take care of the font cex... could maybe set some hint
  # such that the rave_cex is set to pdf mode... We should be able to query the current graphics device to see if it is a window or 
  # a pdf?
  
  # yes!
  # > dev.cur()
  # pdf 
  # 4 
  fin = mapply(function(conn, pf) {
    w_scale = h_scale = 1
    if(pf == 'windowed_comparison_plot') {
      w_scale = ngroups / 2.25
    }
    
    if(pf %in% c('by_trial_heat_map_plot', 'heat_map_plot')) {
      w_scale = ngroups*1.25
      h_scale = ngroups*1.05
    }
    
    .w <- round(9.75*w_scale,1)
    .h <- round(6.03*h_scale,1)
    
    pdf(conn, width = .w, height = .h, useDingbats = FALSE)
  }, conns, plot_functions)
  
  #there are more graphics devices existing than those that we just created, so
  #we need to be a little more careful about how we cycle through them, see 
  # names(dev.list()) == 'pdf' below
  
  find_open_pdfs <- function() {
    dev.list()[names(dev.list()) == 'pdf']
  }
  
  on.exit({
    sapply(find_open_pdfs(), dev.off)
  }, add = TRUE)
  
  
  plot_for_el <- function(etext, write_out_data=FALSE) {
    if(length(etext) > 1) {
      etext %<>% deparse_svec
    }
    # if(shiny_is_running()) {
      progress$inc(sprintf('Rendering graphs for %s', etext))
    # }
    
    args[['ELECTRODE_TEXT']] = etext
    result = do.call(module, args)
    .results = result$results
    
    fff = function(x) {
      paste(names(x), x, sep=':')
    }
    
    mapply(function(graf, dev_num) {
      cat2(paste('plotting:', graf), level = 'INFO')
      cat2(paste('plotting:', fff(dev.set(dev_num))), level = 'INFO')
      #get the function named by graf
      get(graf, envir = asNamespace('ravebuiltins'))(.results)
      
      # match.fun(graf)(.results)
      if(write_out_data) {
        fname <- file.path(dir, paste0(prefix, graf, '.json'))
        data_var = stringr::str_replace(graf, '_plot', '_data')
        cat(jsonlite::serializeJSON(result$results$get_value(data_var, 'NODATA')),
            file=fname)
      }
    }, plot_functions, find_open_pdfs())
  }
  
  # first write into the graphs the aggregate functions
  plot_for_el(electrodes_loaded, write_out_data = TRUE)
  
  # now for the individual electrodes
  lapply(electrodes_loaded, plot_for_el)
  
  if(shiny_is_running()) {
    showNotification(p('Exports finished!'))
  }
}

# Export data options
write_out_data_ui <- function(){
  download = isTRUE(input$export_also_download)
  if( download ){
    tags$a(id = ns('export_data_and_download'), class = 'btn btn-primary shiny-download-link',
           href = '', target='_blank', download='',
           shiny::icon('download'), 'Export data for group analysis')
  }else{
    actionButtonStyled(ns('export_data_only'), 
                       label = 'Export data for group analysis', 
                       icon=shiny::icon('save'),
                       type = 'primary')
  }
}

output$export_data_and_download <- downloadHandler(
  filename = function(){
    analysis_prefix = stringr::str_replace_all(analysis_prefix, '[^\\w]+', '_')
    paste0(analysis_prefix, '.csv')
  },
  content = function(con){
    res_path = write_out_data_function()
    # R.utils::gzip(res_path, destname = con)
    file.copy(res_path, to=con)
  }
)

observeEvent(input$export_data_only, {
  write_out_data_function()
  showNotification(p('Done saving'), duration = 3, type = 'message')
})


# observeEvent(input$GROUPS, {
  
  # print('groups changed')
  
# })


save_inputs <- function(yaml_path, variables_to_export){
  if( !shiny_is_running() || !exists('getDefaultReactiveInput') ){ return(FALSE) }
  
  input <- getDefaultReactiveInput()
  cache_list = shiny::isolate(shiny::reactiveValuesToList(input))
  if(!missing(variables_to_export)) {
    cache_list =cache_list[variables_to_export]
  }
  # if( exists('local_data') && shiny::is.reactivevalues(local_data) ){
  #   local_dat = shiny::isolate(shiny::reactiveValuesToList(local_data))
  #   cl = names(cache_list); cl = cl[cl %in% names(local_dat)]
  #   cache_list[cl] = local_dat[cl]
  # }
  yaml::write_yaml(x = cache_list, fileEncoding = 'utf-8', file = yaml_path)
  return(TRUE)
}

# export data for group analysis
write_out_data_function <- function(){
  
  project_name = subject$project_name
  subject_code = subject$subject_code
  
  # et electrodes to be exported
  electrodes = parse_svec(current_active_set)
  electrodes = electrodes[electrodes %in% preload_info$electrodes]
  
  progress = progress('Exporting baselined data...', max = 3 + length(electrodes))
  on.exit({ progress$close() })
  progress$inc('Collecting data')
  
  # Get trial conditions
  conditions = input$trial_type_filter
  conditions = conditions[conditions %in% preload_info$condition]
  trials = module_tools$get_meta('trials')
  trial_number = trials$Trial[trials$Condition %in% conditions]
  
  # check if they want to include outliers
  .trial_outlier_list = input$trial_outliers_list
  if(length(.trial_outlier_list) > 0 && (!input$include_outliers_in_export)) {
    trial_number <- trial_number[!(trial_number %in% .trial_outlier_list)]
  }
  
  # Get timepoints,frequency range
  time_points = preload_info$time_points
  time_points = time_points[time_points %within% input$export_time_window]
  freq_range = preload_info$frequencies
  freq_range = freq_range[freq_range %within% input$FREQUENCY]
  
  # get baseline
  baseline_range = input$BASELINE_WINDOW
  
  # Do some checks
  
  # Check 1: if no electrode is chosen
  # Check 2: if no condition is chosen
  # Check 3: if no time is chosen
  # Check 4: if no frequency is found
  check_fails = !c(length(electrodes), length(trial_number), length(time_points), length(time_points))
  err_msg = c('None of the electrodes to be exported is loaded', 'No trial found matching selected condition', 
              'Time range is too narrow for any data points to be found', 'Frequency range is too narrow for any data points to be found')
  if(any(check_fails)){
    err_msg = err_msg[check_fails]
    showNotification(p('The following error(s) found:',br(),tags$ul(tagList(
      lapply(err_msg, tags$li)
    ))), type = 'error', id = ns('export_csv'))
    return()
  }
  
  # Baseline
  progress$inc('Generating results... (might take a while)')
  
  # Memory-friendly baseline but might be more time consuming
  power = module_tools$get_power(referenced = TRUE)
  
  # condition list
  cond_list = list(); cond_list[trials$Trial] = trials$Condition
  
  # Use async lapply to speed up the calculation as it's really per electrode analysis
  # res = rave::lapply_async(electrodes, function(e){
  #   bl = baseline(power$subset(Trial = Trial %in% trial_number,
  #                         Time = Time %within% time_points,
  #                         Frequency = Frequency %within% freq_range,
  #                         Electrode = Electrode %in% e), 
  #            from = baseline_range[1], to = baseline_range[2], hybrid = FALSE, mem_optimize = FALSE)
  #   flat = bl$collapse(keep = c(1,3))
  #   dimnames(flat) = dimnames(bl)[c(1,3)]
  #   flat = reshape2::melt(flat, value.name = 'Power') # trial time, value
  #   flat$Condition = unlist(cond_list[flat$Trial])
  #   flat$Electrode = e
  #   flat
  # }, .call_back = function(ii){
  #   progress$inc(sprintf('Electrode %d', electrodes[[ii]]))
  # # specify all variables in .globals, in this way we can avoid the whole memory mappings
  # }, .globals = c('power', 'trial_number', 'time_points', 'freq_range', 'e', 'baseline_range', 'cond_list'))
  
  unit_dims = c(1,2,4)
  if(isTRUE(input$global_baseline)) {
    unit_dims = c(2,4)
  }
  method <- list(
    '% Change Power' = 'percentage',
    '% Change Amplitude' = 'sqrt_percentage',
    'z-score Power' = 'zscore',
    'z-score Amplitude' = 'sqrt_zscore',
    'decibel' = 'decibel'
  )
  unit_of_analysis <- input$unit_of_analysis
  baseline_method = method[[unit_of_analysis]]
  unit_name = format_unit_of_analysis_name(unit_of_analysis)
  
  res = rave::lapply_async(electrodes, function(e){
    progress$inc(sprintf('Electrode %d', e))
    # Important p_sub is assigned, otherwise, it will get gc before baselined
    p_sub = power$subset(Trial = Trial %in% trial_number,
                         Frequency = Frequency %within% freq_range,
                         Electrode = Electrode %in% e)
    
    # bl = baseline(p_sub, from = baseline_range[1], to = baseline_range[2], hybrid = FALSE, mem_optimize = FALSE)
    bl = dipsaus::baseline_array(
      x = p_sub$get_data(),
      baseline_indexpoints = which(p_sub$dimnames$Time %within% baseline_range),
      along_dim = 3L,
      method = baseline_method,
      unit_dims = unit_dims
    )
    bl = ECoGTensor$new(bl, dim = dim(p_sub), dimnames = dimnames(p_sub),
                        varnames = p_sub$varnames, hybrid = FALSE)
    
    bl = bl$subset(Time = Time %within% time_points)
    flat = bl$collapse(keep = c(1,3))
    dimnames(flat) = dimnames(bl)[c(1,3)]
    flat = reshape2::melt(flat, value.name = unit_name) # trial time, value
    flat$Condition = unlist(cond_list[flat$Trial])
    flat$Electrode = e
    flat
  }, .globals = c('unit_name', 'unit_dims', 'baseline_method', 'power',
                  'trial_number', 'time_points', 'freq_range', 'e', 'baseline_range', 'cond_list'),
  .gc = FALSE) 
  
  res = do.call('rbind', res)
  res$Project = project_name
  res$Subject = subject_code
  
  # flag outliers as needed
  res$TrialIsOutlier = FALSE
  if(!is.null(.trial_outlier_list)) {
    res$TrialIsOutlier[res$Trial %in% .trial_outlier_list] = TRUE
  }
  
  
  # Write out results
  progress$inc('Writing out on server, preparing...')
  # write to server _project_data/power_explorer/file
  analysis_prefix = stringr::str_replace_all(analysis_prefix, '[^\\w]+', '_')
  now = strftime(Sys.time(), '-%Y%m%d-%H%M%S')
  
  fname = paste0(analysis_prefix, now, '.csv')
  dirname = file.path(subject$dirs$subject_dir, '..', '_project_data', 'power_explorer', 'exports')
  dir.create(dirname, showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(res, file.path(dirname, fname), append = FALSE)
  
  save_inputs(file.path(dirname, paste0(fname, '.yaml')))
  
  # Collapse Trial and save to 3D viewer
  collapsed_trial = reshape2::dcast(res, Project+Subject+Electrode+Time~Condition, mean, value.var = unit_name)
  dirname_viewer = file.path(subject$dirs$subject_dir, '..', '_project_data', '3dviewer')
  dir.create(dirname_viewer, showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(collapsed_trial, file.path(dirname_viewer, paste0(analysis_prefix, '-collapse_trial-', now, '.csv')), append = FALSE)
  
  return(normalizePath(file.path(dirname, fname)))
}
