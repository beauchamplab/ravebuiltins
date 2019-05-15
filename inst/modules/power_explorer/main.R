# Main algorithm - rave_executes

# Initialize inputs
# rm(list = ls(all.names=T)); rstudioapi::restartSession()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'power_explorer', debug = TRUE)

if(FALSE) {
  GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                list(group_name='B', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v')))
}

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
# requested_electrodes = rutabaga::parse_svec(ELECTRODE_TEXT, sep = ':-')
# requested_electrodes %<>% get_by(`%in%`, electrodes)

# this will be NA if the only requested electrodes are not available
# electrode <- requested_electrodes[1]
# assertthat::assert_that(length(requested_electrodes) >= 1 &&
                          # all(not_NA(requested_electrodes)), msg = 'No electrode selected')

# grab the subject code so it can be used later
subject_code = subject$subject_code

# Clean group input data
group_data = lapply(seq_along(GROUPS), function(idx) {
  g = GROUPS[[idx]]

  Trial_num = epoch_data$Trial[epoch_data$Condition %in% unlist(g$group_conditions)]
  list(
    name = g$group_name,
    Trial_num = Trial_num,
    group_index = idx,
    has_trials = length(Trial_num) > 0,
    conditions = unlist(g$group_conditions)
  )

})
has_trials <- vapply(group_data, function(g){g$has_trials}, FALSE)
any_trials <- any(has_trials)

# Subset data
bl_power <- cache(
  key = list(subject$id, ELECTRODE, BASELINE_WINDOW, preload_info$time_points, combine_method,
             any_trials, preload_info$epoch_name, preload_info$reference_name),
  val = baseline(power$subset(Electrode = Electrode == ELECTRODE), 
                 from=BASELINE_WINDOW[1], to= BASELINE_WINDOW[2],
                 hybrid = FALSE, mem_optimize = FALSE)
)

# Prepare plot datasets
scatter_bar_data <- line_plot_data <- by_trial_heat_map_data <- heat_map_data <- group_data
flat_data <- data.frame()

# set transform method
.transform <- electrode_transform(combine_method)

# for transforms, the idea is to apply at each trial for each frequency
# then when things get it will already be done


#relies on .transform as defined above
if(combine_method != 'none') {
  transformed_power <- cache(
    key = list(combine_method, subject$id, ELECTRODE, BASELINE_WINDOW, preload_info$time_points,
               any_trials, preload_info$epoch_name, preload_info$reference_name),
    
    val = {
      transformed_power <- bl_power$get_data()
      
      # we should be able to apply the sqrt transform directly to the eniter tensor
      if(combine_method == 'amplitude') {
        transformed_power %<>% .transform
      } else {
        for(ti in seq_len(dim(transformed_power)[1L])) {
          transformed_power[ti,,,1] <- t(apply(transformed_power[ti,,,1], 1, .transform))
        }
      }
      
      transformed_power
    }
  )
  bl_power$set_data(transformed_power)
}


# Collapse data

## Leave it here in case you want to change it later
# (make it user specific)
collapse_method = 'mean'

# This module is no longer across electrodes, so if we are transforming,
# we likely want to do it at the trial level, not on the back end before combining across electrodes
for(ii in which(has_trials)){
  .power_all = bl_power$subset(Trial = Trial %in% group_data[[ii]]$Trial_num, data_only = FALSE, drop=FALSE)
  .power_freq = .power_all$subset(Frequency=Frequency %within% FREQUENCY, data_only = FALSE, drop=FALSE)
  
  N = dim(.power_all)[1L]
  
  
  
  trials = epoch_data %>% subset((.)$Trial %in% group_data[[ii]]$Trial_num) %>% extract2('Condition')
  
  # utils functions
  wrap_data = function(value){
    list(
      data = value,
      range = .fast_range(value),
      N = N,
      trials = trials,
      name = group_data[[ii]]$group_name
    )
  }
  
  # 1 Time x Frequency
  .power_all_clean <- .power_all$subset(Trial=! (Trial %in% trial_outliers_list))
  Nclean <- dim(.power_all_clean)[1L]
  heat_map_data[[ii]] <- append(heat_map_data[[ii]],
                                wrap_data(.power_all_clean$collapse(keep = c(3,2), method = collapse_method)))
  
  attr(heat_map_data[[ii]]$data, 'xlab') <- 'Time (s)'
  attr(heat_map_data[[ii]]$data, 'ylab') <- 'Frequency'
  attr(heat_map_data[[ii]]$data, 'zlab') <- ifelse(combine_method=='none', 'Mean % Signal Change',
                                                            'Mean '  %&% combine_method %&% ' %SC')
  
  # the x value for the hmd is time
  heat_map_data[[ii]]$x <- .power_all$dimnames$Time
  
  #the y value for the hmd is frequency
  heat_map_data[[ii]]$y <- .power_all$dimnames$Frequency
  
  # hmd is using the clean data
  heat_map_data[[ii]]$N <- Nclean
  
  # 2 Time x Trial (.power_freq)
  # by trial data. Set drop to FALSE b/c we want to keep the electrode dim even if #e ==1
  by_trial_heat_map_data[[ii]] <- append( by_trial_heat_map_data[[ii]], wrap_data(
    .power_freq$collapse(keep = c(3,1), method = collapse_method)
  ))

  # the x value for the bthmd is time
  by_trial_heat_map_data[[ii]]$x <- .power_freq$dimnames$Time

  #the y value for the bthmd is Trial
  by_trial_heat_map_data[[ii]]$y <- seq_along(.power_freq$dimnames$Trial)
  
  attr(by_trial_heat_map_data[[ii]]$data, 'xlab') <- 'Time (s)'
  attr(by_trial_heat_map_data[[ii]]$data, 'ylab') <- 'Trial'
  attr(by_trial_heat_map_data[[ii]]$data, 'zlab') <- ifelse(combine_method=='none', 'Mean % Signal Change',
                                                            'Mean '  %&% combine_method %&% ' %SC')
  
  # 3 Time only
  # coll freq and trial for line plot w/ ebar. Because we're doing error bars, we have to know whether we have 1 vs. >1 electrodes
  # if(length(requested_electrodes) == 1){
  # Single electrode, mean and mse for each time points
  line_plot_data[[ii]] = append(line_plot_data[[ii]], wrap_data(t(
    apply(
      .power_freq$collapse(keep = c(1,3), method = 'mean'),
      2, .fast_mse)
  )))
  
  attr(line_plot_data[[ii]]$data, 'xlab') <- 'Time (s)'
  attr(line_plot_data[[ii]]$data, 'ylab') <- ifelse(combine_method=='none', 'Mean % Signal Change',
                                                    'Mean '  %&% combine_method %&% ' %SC')
  
  # scatter bar data
  scatter_bar_data[[ii]] = append(scatter_bar_data[[ii]], wrap_data(
    rowMeans(.power_freq$subset(
      Time = (Time %within% ANALYSIS_WINDOW),
      data_only = TRUE
    ))
  ))
  
  attr(scatter_bar_data[[ii]]$data, 'xlab') <- 'Group'
  attr(scatter_bar_data[[ii]]$data, 'ylab') <- ifelse(combine_method=='none', 'Mean % Signal Change',
                                                      'Mean '  %&% combine_method %&% ' %SC')
  
  # we want to make a special range for the line plot data that takes into account mean +/- SE
  line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                       line_plot_data[[ii]]$data[,2]))
  
  # also add in the x variable for the time series
  line_plot_data[[ii]]$x <- .power_freq$dimnames$Time
  
  # for the scatter_bar_data we also need to get m_se within condition
  scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)
  
  flat_data %<>% rbind(data.frame('group'=ii, 'y' = scatter_bar_data[[ii]]$data))
}

# .power_freq[,, preload_info$time_points %within% ANALYSIS_WINDOW, ]$data

# for baseline you want to have only the baseline times
flat_data$group %<>% factor

# this can be used elsewhere
has_data = sum(has_trials)

# calculate some statistics

# calculate the statistics here so that we can add them to the niml_out
# if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
if(length(unique(flat_data$group)) > 1) {
  # we need to check if they have supplied all identical data sets
  # easy way is to check that the trials are the same?
  g1_trials <- unlist(GROUPS[[which(has_trials)[1]]]$group_conditions)
  if(all(
    sapply(which(has_trials)[-1],
           function(ii) {
             setequal(unlist(GROUPS[[ii]]$group_conditions),g1_trials)
           })
  )) {
    result_for_suma <- get_t(flat_data$y[flat_data$group==flat_data$group[1]])
  } else {
    result_for_suma <- get_f(y ~ group, flat_data)
  }
} else {
  result_for_suma <- flat_data$y %>% get_t
}

attr(scatter_bar_data, 'stats') <- result_for_suma

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
require(ravebuiltins)

module = ravebuiltins:::debug_module('power_explorer')

result = module(GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                              # putting in an empty group to test our coping mechanisms
                              list(group_name='YY', group_conditions=c()),
                              list(group_name='', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))),
                FREQUENCY = c(75,150), max_zlim = 0,
                sort_trials_by_type = T, combine_method = 'none')
results = result$results
# attachDefaultDataRepository()

result$heat_map_plot()
result$by_trial_heat_map()
result$over_time_plot()
result$windowed_comparison_plot()

ravebuiltins::dev_ravebuiltins(expose_functions = TRUE)
view_layout('power_explorer', sidebar_width = 3, launch.browser = T)

m = to_module(module_id)
init_app(m)

mount_demo_subject()

env = reload_this_package(expose = FALSE, clear_env = TRUE)

# Step 2: make sure rave data is attached
attachDefaultDataRepository()

# Step 3: try to run from local session
module = rave::get_module(package = 'ravebuiltins', module_id = 'power_explorer', local = T)

res = module()

# Step 4: launch modules in RAVE (production)
# Cmd+Shift+B
m = rave::detect_modules('ravebuiltins')
rave::init_app(m)

