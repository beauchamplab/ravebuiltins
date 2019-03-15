# Main algorithm - rave_executes

# Initialize inputs
ravebuiltins:::dev_ravebuiltins(T)

mount_demo_subject()

init_module(module_id = 'power_explorer', debug = TRUE)


if(FALSE) {
  GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                list(group_name='B', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v')))
}


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
requested_electrodes = rutabaga::parse_svec(ELECTRODE_TEXT, sep = ':-')
requested_electrodes %<>% get_by(`%in%`, electrodes)

# this will be NA if the only requested electrodes are not available
# electrode <- requested_electrodes[1]
assertthat::assert_that(length(requested_electrodes) >= 1 &&
                          all(not_NA(requested_electrodes)), msg = 'No electrode selected')

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
  key = list(subject$id, requested_electrodes, BASELINE_WINDOW, any_trials, preload_info),
  val = baseline(power$subset(Electrode = Electrode %in% requested_electrodes),
                 BASELINE_WINDOW[1],  BASELINE_WINDOW[2], hybrid = FALSE, mem_optimize = FALSE)
)

# Prepare plot datasets
scatter_bar_data <- line_plot_data <- by_trial_heat_map_data <- heat_map_data <- group_data
flat_data <- data.frame()

# set transform method
.transform <- electrode_transform(combine_method)

#relies on transform as defined above
do_row_transform <- function(.tens) {
  vapply(seq_len(dim(.tens)[3]), function(ei) {
    t(apply(.tens[,,ei], 1, .transform))
  }, FUN.VALUE = .tens[,,1])
}

# Collapse data

## Leave it here in case you want to change it later (make it user specific, for example!)
collapse_method = 'mean'

# Q: how should we collapse across electrodes?
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


  # Case 1: if no tranformation,
  if(identical(.transform, IDENTITY_TRANSFORM)) {

    # 1 Time x Frequency
    heat_map_data[[ii]] = append(heat_map_data[[ii]], wrap_data(
      .power_all$collapse(keep = c(3,2), method = collapse_method)
    ))

    # 2 Time x Trial (.power_freq)
    # by trial data. Set drop to FALSE b/c we want to keep the electrode dim even if #e ==1
    by_trial_heat_map_data[[ii]] <- append( by_trial_heat_map_data[[ii]], wrap_data(
      .power_freq$
        collapse(keep = c(3,1), method = collapse_method)
    ))

    # 3 Time only
    # coll freq and trial for line plot w/ ebar. Because we're doing error bars, we have to know whether we have 1 vs. >1 electrodes
    if(length(requested_electrodes) == 1){
      # Single electrode, mean and mse for each time points
      line_plot_data[[ii]] = append(line_plot_data[[ii]], wrap_data(t(
        apply(
          .power_freq$collapse(keep = c(1,3), method = 'mean'),
          2, .fast_mse)
      )))
    }else{
      # multiple electrodes, mean and mse across electrodes
      # Note by Zhengjia: I don't think this calculation is correct, the actual sample size is larger and the stat power is lowered (type-II error increases)

      line_plot_data[[ii]] = append(line_plot_data[[ii]], wrap_data(t(
        apply(
          .power_freq$collapse(keep = c(3,4), method = 'mean'),
          1, .fast_mse)
      )))
    }



    # scatter bar data
    scatter_bar_data[[ii]] = append(scatter_bar_data[[ii]], wrap_data(
      rowMeans(.power_freq$subset(
        Time = (Time %within% ANALYSIS_WINDOW),
        data_only = TRUE
      ))
    ))

  }else{
    # transform data
    hmd <- do_row_transform(
      .power_all$collapse(keep = 2:4, method = 'mean')
    )

    heat_map_data[[ii]] = append(heat_map_data[[ii]], wrap_data(rutabaga::collapse(hmd, keep=2:1, average = TRUE)))


    #collapse over frequency then row transform
    bthmd <- do_row_transform(
      .power_freq$collapse(keep = c(1,3,4), method = 'mean')
    )
    by_trial_heat_map_data[[ii]] =  append(by_trial_heat_map_data[[ii]], wrap_data(rutabaga::collapse(bthmd, keep=2:1, average = TRUE)))


    if(length(requested_electrodes) == 1){
      line_plot_data[[ii]] = append(line_plot_data[[ii]], wrap_data(t(
        apply(
          .power_freq$collapse(keep = c(1,3), method = 'mean'),
          2, .fast_mse)
      )))
    }else{

      oft <- apply(
        .power_freq$collapse(keep = c(3,4), method = 'mean'),
        2, .transform)
      line_plot_data[[ii]] <- append(line_plot_data[[ii]], wrap_data(
        t(apply(oft, 1, .fast_mse))
      ))
    }

    # collapse over freq and time so we get response per trial for scatter bar data.
    # use the bthmd that is already frequency selected and transformed per trial (across time)

    # now we want the summary across time \in ANALYSIS_WINDOW and electrode. one mean per trial
    ind.t <- preload_info$time_points %within% ANALYSIS_WINDOW
    scatter_bar_data[[ii]] <- append(scatter_bar_data[[ii]], wrap_data(rowMeans(bthmd[,ind.t,])))
  }

  # we want to make a special range for the line plot data that takes into account mean +/- SE
  line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                       line_plot_data[[ii]]$data[,2]))

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

###### @async
print(Sys.getpid())


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
require(ravebuiltins)

module = ravebuiltins:::debug_module('power_explorer')

result = module(sort_trials_by_type = TRUE, log_scale=F, GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a'))))
results = result$results

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


