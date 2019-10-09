# Main algorithm - rave_executes

# Initialize inputs
# rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject(force_reload_subject = T,
                   subject_code = 'YAB',project_name = 'congruency', electrodes=13:20, epoch='YABa')

init_module(module_id = 'power_explorer', debug = TRUE)
# attachDefaultDataRepository()
if(FALSE) {
  GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                list(group_name='B', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v')))
  FREQUENCY = c(75,150)
  ELECTRODE_TEXT = '14-19'
}

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
# 
# cache <- function(key,val, ...) {
#   return (val)
# }

# these are only needed when shiny is running (e.g., module debug mode)
if(shiny_is_running()) {
  calc_flag = shiny::isolate(local_data$calculate_flag)

  if( !auto_calculate && calc_flag >= do_calculate_btn ){
    # Not auto calculation
    session$sendCustomMessage(type = 'rave_enable_button', message = list( element_id = ns('do_calculate_btn') ))
    local_data$calculate_flag = do_calculate_btn
    return()
  }else{
      session$sendCustomMessage(type = 'rave_disable_button', message = list( element_id = ns('do_calculate_btn') ))
      local_data$calculate_flag = do_calculate_btn
  }
}
requested_electrodes = rutabaga::parse_svec(ELECTRODE_TEXT, sep=',|;', connect  = ':-')
requested_electrodes %<>% get_by(`%in%`, electrodes)

# this will be NA if the only requested electrodes are not available
# electrode <- requested_electrodes[1]
assertthat::assert_that(length(requested_electrodes) >= 1 &&
                          all(not_NA(requested_electrodes)), msg = 'No electrode selected')

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

# for performance -- check if the previous baseline exists in the cache. If so,
# grab it, if not, then recaclulate, but remove the previous one to save on space
# Could put another value in the cache -- last_baseline_key or something, so we
# can check that directly
# cache('hi', val={Sys.sleep(2); 4}, replace=F)
# getDefaultCacheEnvironment()[[digest::digest('hi')]]

# Subset data
bl_power <- cache(
  key = list(subject$id, requested_electrodes, BASELINE_WINDOW,
             preload_info$time_points, combine_method,
             any_trials, preload_info$epoch_name, preload_info$reference_name),
  val = baseline(power$subset(Electrode = Electrode %in% requested_electrodes),
                 from=BASELINE_WINDOW[1], to= BASELINE_WINDOW[2],
                 hybrid = FALSE, mem_optimize = FALSE)
)

jitter_seed <- cache(
  key = 'jitter_seed',
  val = sample(1:100, 1)
)

# Prepare plot datasets
scatter_bar_data <- line_plot_data <- by_electrode_heat_map_data <- 
  by_trial_heat_map_data <- heat_map_data <- group_data
flat_data <- data.frame()

# set transform method
.transform <- electrode_transform(combine_method)

# for transforms, the idea is to apply at each trial for each frequency
# then when things get it will already be done
#relies on .transform as defined above
if(combine_method != 'none') {
  transformed_power <- cache(
    key = list(combine_method, subject$id, ELECTRODE_TEXT, BASELINE_WINDOW, preload_info$time_points,
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
# we likely want to do it at the trial level, not on the back end before combining across electrodes

# to help with caching, we need to only recalculate here if the GROUPs have changed. 
# get_data_for_condition <- function(ii) {
#   
# }

for(ii in which(has_trials)){
  .time_stamp <- proc.time()

  ### 17ms  
  .power_all = bl_power$subset(Trial = Trial %in% group_data[[ii]]$Trial_num)
  .power_all_clean <- .power_all$subset(Trial=! (Trial %in% trial_outliers_list))
  .power_freq = .power_all$subset(Frequency=Frequency %within% FREQUENCY)
  .power_freq_clean = .power_freq$subset(Trial=! (Trial %in% trial_outliers_list))
  
  N = dim(.power_all)[1L]
  Nclean <- dim(.power_all_clean)[1L]
  
  epoch_data_subset <- epoch_data %>% subset((.)$Trial %in% group_data[[ii]]$Trial_num)
  
  # This copies over some information from group_data that is needed by particular plots
  # as well as populating data/range 
  wrap_data = function(value, ...){
    ll = list(
      data = value,
      range = .fast_range(value),
      N = N,
      trials = epoch_data_subset$Condition,
      Trial_num = group_data[[ii]]$Trial_num,
      is_clean = !(epoch_data_subset$Trial %in% trial_outliers_list),
      name = group_data[[ii]]$name,
      has_trials = group_data[[ii]]$has_trials,
      conditions = group_data[[ii]]$conditions,
      baseline_window = BASELINE_WINDOW,
      analysis_window = ANALYSIS_WINDOW,
      frequency_window = FREQUENCY
      )
    
    vals = list(...)
    
    for (k in c('ylab', 'zlab')) {
      if (isTRUE(vals[[k]] == 'auto')) {
        vals[[k]] = ifelse(combine_method == 'none',
                           'Mean % Signal Change',
                           'Mean '  %&% combine_method %&% ' %SC')
      }
    }
    
    for(k in names(vals)) {
      # check for attribute labels
      if (k %in% c('xlab', 'ylab', 'zlab')) {
          attr(ll$data, k) = vals[[k]]
      }
      # all other values just add into the data list
      else {
        ll[[k]] = vals[[k]]
      }
    }
    
    return (ll)
  }
  
  # 1. power @ frequency over time
  heat_map_data[[ii]] <- wrap_data(
    .power_all_clean$collapse(keep = c(3,2), method = collapse_method),
    xlab='Time (s)', ylab='Frequency', zlab='auto',
    x = .power_all$dimnames$Time,
    y = .power_all$dimnames$Frequency,
    # hmd is using the clean data
    N = Nclean
  )
  
  # 2. power @ trial over time
  by_trial_heat_map_data[[ii]] <- wrap_data(
    .power_freq$collapse(keep = c(3,1), method = collapse_method),
    x = .power_freq$dimnames$Time,
    y = seq_along(.power_freq$dimnames$Trial),
    xlab='Time (s)', ylab='Trial', zlab='auto'
  )
  
  # 2.5 by electrode over time
  by_electrode_heat_map_data[[ii]] <- wrap_data(
    .power_freq$collapse(keep = c(3,4), method = collapse_method),
    x=.power_freq$dimnames$Time,
    y=.power_freq$dimnames$Electrode,
    xlab='Time (s)', ylab='Electrode', zlab='auto'
  )
  
  # 3. Time only
  # coll freq and trial for line plot w/ ebar. Because we're doing error bars, we have to know whether we have 1 vs. >1 electrodes
  # Single electrode, mean and mse for each time points
  line_plot_data[[ii]] = wrap_data(t(
    apply(.power_freq_clean$collapse(keep = 3:4, method = 'mean'), 1, .fast_mse)),
    xlab='Time (s)', ylab='auto', N=dim(.power_freq_clean)[4L], x=.power_freq_clean$dimnames$Time
  )
  
  # set NA (divide by zero) error bars to 0  
  line_plot_data[[ii]]$data[is.na(line_plot_data[[ii]]$data[,2]),2] <- 0
  
  
  # we want to make a special range for the line plot data that takes into account mean +/- SE
  line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                       line_plot_data[[ii]]$data[,2]))
  
  # scatter bar data -- here we want all of the data because we are going to highlight (or not) the outliers -- same for by-trial heatmap
  # if(show_outliers_on_plots) {
  scatter_bar_data[[ii]] <- wrap_data(
    rowMeans(.power_freq$subset(Time = (Time %within% ANALYSIS_WINDOW),data_only = TRUE)),
    N=Nclean, xlab='Group', ylab='auto', x=.power_freq$dimnames$Time
  )
  
  # Although this seems to be the wrong place to do this, not sure where else we can do it
  # to enable point identification later, we need to know the x-location of each point. So the jittering
  # needs to be done here.
  .xp <- barplot(which(has_trials),plot=FALSE)
  .r <- if(sum(has_trials)>1) {
       mean(unique(diff(.xp)))*0.25  
     } else {
      0.75*(1/3)
     }
  
  xpi <- which(ii == which(has_trials))
  scatter_bar_data[[ii]]$xp <- .xp[xpi]
  set.seed(jitter_seed)
  scatter_bar_data[[ii]]$x <- .xp[xpi] + runif(length(scatter_bar_data[[ii]]$data), -.r, .r)

  
  # for the scatter_bar_data we also need to get m_se within condition w/o the outliers
  scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data[scatter_bar_data[[ii]]$is_clean])
  
  flat_data %<>% rbind(data.frame('group'=ii,
                                  'y' = with(scatter_bar_data[[ii]], data[is_clean])))
  
  # print('loop ' %&% ii) 
  # print(proc.time() - .time_stamp)
}


# for baseline you want to have only the baseline times
flat_data$group %<>% factor

# this can be used elsewhere
has_data = sum(has_trials)

# calculate some statistics across electrodes

# because we're eventually going to be doing second-level stats, we're not too worried about
# gratuitous NHST

# the first thing we want is an omnibus F statistic. Because the data are baseline corrected, we're here
# just comparing with zero. That is, a no-intercept model.

# we need the omnibus result per-electrode, do for all electrodes, not just selected

all_trial_types <- GROUPS %>% lapply(`[[`, 'group_conditions') %>% unlist %>% unique

get_data_per_electrode <- function()  {
  bl <- baseline(power$subset(Trial=Trial %in% epoch_data$Trial[epoch_data$Condition %in% all_trial_types],
                              Frequency=Frequency %within% FREQUENCY,
                              Time=Time %within% range(BASELINE_WINDOW, ANALYSIS_WINDOW) ),
                 from=BASELINE_WINDOW[1], to= BASELINE_WINDOW[2],
                 hybrid = FALSE, mem_optimize = FALSE)
  bl.analysis <- bl$subset(Time=Time %within% ANALYSIS_WINDOW)
  pow <- bl$collapse(keep = c(1,4))
  m = colMeans(pow)
  
  t = m / .fast_column_se(pow)
  p = 2*pt(abs(t), df = nrow(pow)-1, lower=F)
  
  res <- rbind(m,t,p)
  colnames(res) <- dimnames(power)$Electrode
  
  return(res)
}
get_data_per_electrode_alt <- function(){
  trial_numbers = epoch_data$Trial[epoch_data$Condition %in% all_trial_types]
  
  # Do not baseline them all, otherwise memory will explode
  res = rave::lapply_async(electrodes, function(e){
    # Subset on electrode is memory optimized, and is fast
    bl = power$subset(Electrode = Electrode == e)
    bl = baseline(bl$subset(Trial=Trial %in% trial_numbers,
                             Frequency=Frequency %within% FREQUENCY,
                             Time=Time %within% range(BASELINE_WINDOW, ANALYSIS_WINDOW) ),
                   from=BASELINE_WINDOW[1], to= BASELINE_WINDOW[2],
                   hybrid = FALSE, mem_optimize = FALSE)
    bl.analysis <- bl$subset(Time=Time %within% ANALYSIS_WINDOW)
    pow <- bl$collapse(keep = c(1,4))
    m = colMeans(pow)
    
    t = m / .fast_column_se(pow)
    p = 2*pt(abs(t), df = nrow(pow)-1, lower=F)
    
    res <- rbind(m,t,p)
    colnames(res) <- e
    res
  }, .globals = c('electrodes', 'e', 'trial_numbers', 'FREQUENCY', 'ANALYSIS_WINDOW', 'BASELINE_WINDOW',
                  '.fast_column_se'), .gc = FALSE)
  do.call('cbind', res)
}

omnibus_results <- cache(
  key = list(subject$id, BASELINE_WINDOW, FREQUENCY,all_trial_types,
             ANALYSIS_WINDOW, combine_method, preload_info$epoch_name,
             preload_info$reference_name, trial_outliers_list),
  val = get_data_per_electrode_alt()
)

# calculate the statistics here so that we can add them to plot output -- eventually this goes away?
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
  result_for_suma <- get_t(flat_data$y)
}

attr(scatter_bar_data, 'stats') <- result_for_suma


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject(force_reload_subject = T)
module = ravebuiltins:::debug_module('power_explorer')

result = module(ELECTRODE_TEXT = '14',
  GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                              # putting in an empty group to test our coping mechanisms
                              list(group_name='YY', group_conditions=c()),
                              list(group_name='ZZ', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))),
                background_plot_color_hint='white', BASELINE_WINDOW = c(-1,-.1), plot_time_range = c(-1,1.5),
                FREQUENCY = c(70,150), max_zlim = 0, show_outliers_on_plots = TRUE,
                sort_trials_by_type = T, combine_method = 'none')
results = result$results
# results$get_value('omnibus_results')
result$across_electrodes_corrected_pvalue()

# attachDefaultDataRepository()
# get_summary()

result$heat_map_plot()
result$windowed_comparison_plot()
result$by_trial_heat_map()
result$over_time_plot()
result$by_electrode_heat_map()

ravebuiltins::dev_ravebuiltins(expose_functions = TRUE)


# dev layout has red theme
dev_layout <- function(module_id, sidebar_width = 5, launch.browser = rstudio_viewer){
  # Always reload the package to the newest status and preview
  env = reload_this_package()
  
  m = env$to_module(module_id = module_id, sidebar_width = sidebar_width)
  rave::init_app(m, launch.browser = launch.browser, disable_sidebar = T, simplify_header = T, theme='red')
}

# view_layout('power_explorer', sidebar_width = 3, launch.browser = T)
dev_layout('power_explorer', sidebar_width = 3, launch.browser = T)

# m = to_module(module_id)
# init_app(m)

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
rave::start_rave()

