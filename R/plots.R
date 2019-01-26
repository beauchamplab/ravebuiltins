#' time series plot
#' @export
over_time_plot <- function(results) {
  with(results, {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))
    
    time_series_plot(line_plot_data)
  })
}

#' by trial plot with statistics
#' @export
windowed_comparison_plot <- function(results){
  with(results, {
    
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))
    
    trial_scatter_plot(scatter_bar_data)
  })
}

#' basic time frequency plot
#' @export
heat_map_plot = function(results){
  with(results, {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))
    
    # here we need to pass in the decorator because dmhm is going to loop over the heatmaps
    # and take care of drawing a color bar for us
    draw_many_heat_maps(hmaps = heat_map_data, x = preload_info$time_points, 
                        y = preload_info$frequencies, log_scale = log_scale,
                        TIME_RANGE = TIME_RANGE, BASELINE = BASELINE, FREQUENCY = FREQUENCY, max_zlim = max_zlim)
  })
}

voltage_over_time_plot = function(results){
  with(results, {
    
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))
  })
  
  # here we need to pass in the decorator because dmhm is going to loop over the heatmaps
  # and take care of drawing a color bar for us
  # draw_many_heat_maps(heat_map_data, allow_log_scale = TRUE)
}


msg_out = function() {
  # put analysis information in here
  # msg
  # return(sprintf('length: %d', length(heat_map_data)))
}

async_out = function(){
  validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
  async_msg
}

# do_execute <- function(.electrode) {
#     re <- cache(
#         key = list(.electrode, subject$id, preload_info, BASELINE, FREQUENCY),
#         value = {
#             #
#         }
#     )
#
#     return(re)
# }


get_summary <- function() {
  # here we just want an estimate of the power at each trial for each electrode
  # get the labels for each trial
  
  ..g_index <- 1
  GROUPS = lapply(GROUPS, function(g){
    g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]
    
    if(g$GROUP_NAME == '') {
      g$GROUP_NAME <-  LETTERS[..g_index]
      ..g_index <<- ..g_index + 1
    }
    
    return(g)
  })
  rm(..g_index)
  
  tnum_by_condition <- sapply(GROUPS, function(g) {
    list(g$Trial_num)
  }) %>% set_names(sapply(GROUPS, '[[', 'GROUP_NAME'))
  
  all_trials <- unlist(tnum_by_condition)
  .bl_power <- cache(
    key = list(subject$id, preload_info$electrodes, BASELINE, preload_info),
    val = baseline(power, BASELINE[1],  BASELINE[2], hybrid = FALSE, mem_optimize = FALSE)
  )
  
  # subset out the trials, frequencies, and time rane
  .power <- .bl_power$subset(Frequency = Frequency %within% FREQUENCY,
                             Time = Time %within% TIME_RANGE,
                             Trial = Trial %in% all_trials, data_only = FALSE)
  
  stimulus <- epoch_data$Condition[as.numeric(.power$dimnames$Trial)]
  
  condition <- .power$dimnames$Trial %>% as.numeric %>% sapply(function(tnum) {
    #ensure only one group is ever selected? or we could throw an error on length > 1
    sapply(tnum_by_condition, `%in%`, x=tnum) %>% which %>% extract(1)
  }) %>% names
  
  # rutabaga over Freq and Time
  # by_elec <- rutabaga::collapse(.power$data, keep=c(1,4)) / prod(.power$dim[2:3])
  by_elec <- .power$collapse(keep = c(1,4), method = 'mean')
  
  data.frame('subject_id' = subject$subject_code,
             'elec' = rep(preload_info$electrodes, each=length(condition)),
             'trial' = rep(seq_along(condition), times=length(preload_info$electrodes)),
             'condition' = rep(condition, length(preload_info$electrodes)),
             'power' = c(by_elec)
  )
}


ce_main <- function(current_electrodes) {
  #baseline all available trials
  GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]; g })
  has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
  any_trials <- any(has_trials)
  
  bl_power <- cache(
    key = list(subject$id, current_electrodes, BASELINE, any_trials, preload_info),
    val = baseline(power$subset(Electrode = Electrode %in% current_electrodes),
                   BASELINE[1],  BASELINE[2], hybrid = FALSE, mem_optimize = FALSE)
  )
  
  
  
}

# rave_execute({
#   
# },{
#   if(.is_async){
#     async_msg = 'Running in the background'
#   }
# }, async = {
#   logger('Async test')
#   # rm(list = ls(all.names = T))
#   Sys.sleep(0.15)
#   nms = ls(all.names = T)
#   async_msg = paste(search(), collapse = ', ')
# },
# #####
# auto = TRUE
# )

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

export_graphs <- function(conn=NA, lbl='png_out', dir, ...) {
  .dir <- module_tools$get_subject_dirs()$module_data_dir %&%  '/condition_explorer/png_out/'
  dir.create(.dir, recursive = TRUE)
  
  pow <- module_tools$get_power(force = T, referenced = T)
  
  
  #TODO check the variable export_per_electrode to see if we need to loop over electrodes and export
  # or if we want use just the current_electrodes and combine them
  
  #TODO need to scale all the fonts etc so things aren't too large for export
  
  as_pdf(.dir %&% 'line_plot_el', w = 5, h=3, {
    over_time_plot()
  } )
  
  
}
