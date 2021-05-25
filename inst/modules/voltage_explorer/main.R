# Initialize inputs
ravebuiltins:::dev_ravebuiltins(T)

mount_demo_subject()

init_module(module_id = 'voltage_explorer', debug = TRUE)

if(F) {
  GROUPS = list(list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                list(group_name='B', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v')))

  ERP_SAMPLE_RATE = 50
  BASELINE_TYPE = 'dB'
  do_baseline = TRUE
}


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
    # requested_electrodes = parse_svec(ELECTRODE_TEXT %>% str_replace_all(':', '-'))
    # requested_electrodes %<>% get_by(`%in%`, electrodes)

    # this will be NA if the only requested electrodes are not available
    # electrode <- requested_electrodes[1]
    # assertthat::assert_that(length(requested_electrodes) >= 1 && all(not_NA(requested_electrodes)),msg = 'No electrode selected')

    # downsample as requested
    s = voltage$dimnames$Time[1]
    f = voltage$dimnames$Time[length(voltage$dimnames$Time)]
    requested_samples <- seq(s, f, by=1/ERP_SAMPLE_RATE)
    eps <- diff(requested_samples)

    keepers <- voltage$dimnames$Time[..get_nearest(requested_samples, voltage$dimnames$Time)]
    voltage_sub <- voltage$subset(Time = Time %in% keepers, Electrode = Electrode %in% ELECTRODE)
    # match_baseline_unit <- function(u) {
    #   if(u == 'dB') {
    #     return ('dB')
    #   }
    #     return ('%')
    # }


    # system.time({
      # vs <- voltage_sub$collapse(keep=2)
      # par(mfrow=c(1,3))
      # .fft = fftw::FFT(vs)
      # plot(vs, type='l')
      # plot(Re(fftw::IFFT(fftw::FFT(vs), scale = F)), type='l')
      # .fft[c(1:25, 35:301)] <- 0
      # plot(Re(fftw::IFFT(.fft, scale = T)) %>% abs %>% smooth.spline(df=10) %>% extract2('y'), type='l')
    # })

    # Brian says that baselining is reasonable. For welch plots, subtract off the baseline welch
    # Do % change (x-mean(b)) /mean(b) or pure subtractive baseline (x-mean(b))
    # if(do_baseline) {
    #   bl_power <- cache(
    #     key = list(preload_info, requested_electrodes, baseline_window, 'voltage', BASELINE_TYPE, keepers),
    #     val = baseline(voltage_sub, unit = match_baseline_unit(BASELINE_TYPE),
    #                    baseline_window[1],  baseline_window[2], hybrid = FALSE, mem_optimize = FALSE)
    #   )
    # }

    analysis_voltage <- voltage_sub$subset(Time = Time %within% analysis_window)
    baseline_voltage <- voltage_sub$subset(Time = Time %within% baseline_window)

    group_data = lapply(seq_along(GROUPS), function(idx){
        g = GROUPS[[idx]]

        Trial_num = epoch_data$Trial[epoch_data$Condition %in% unlist(g$group_conditions)]
        .all_data <- voltage_sub$subset(Trial = Trial %in% Trial_num)
        .analysis_data <- analysis_voltage$subset(Trial = Trial %in% Trial_num)
        .baseline_data <- baseline_voltage$subset(Trial = Trial %in% Trial_num)
        
        list(
            name = g$group_name,
            Trial_num = Trial_num,
            group_index = idx,
            has_trials = length(Trial_num) > 0,
            conditions = unlist(g$group_conditions),
            all_data = .all_data,
            analysis_data = .analysis_data,
            baseline_data = .baseline_data,
            range = range(.all_data$get_data()),
            N=nrow(.all_data$get_data())
        )
    })

    has_trials <- vapply(group_data, function(g){g$has_trials}, FALSE)
    has_data <- any(has_trials)
    line_plot_data <- group_data
    # lpd <- line_plot_data[[1]]
    line_plot_data <- lapply(line_plot_data, function(lpd) {
      .d <- lpd$all_data$collapse(keep=1:2)
      lpd$x <- lpd$all_data$dimnames$Time
      
      lpd$all_data <- NULL
      lpd$analysis_data <- NULL
      lpd$baseline_data <- NULL
      lpd$data <- t(apply(.d, 2, .fast_mse))
      lpd$range <- range(plus_minus(lpd$data[,1], lpd$data[,2]))
      return(lpd)
    })

    group_data %<>% lapply(function(d) {
      d$analysis_data <- d$analysis_data$collapse(keep=1:2)
      # d$analysis_data_mean <- rowMeans(d$analysis_data)

      d$baseline_data <- d$baseline_data$collapse(keep=1:2)
      # d$baseline_data_mean <- rowMeans(d$baseline_data)
      d$data <- d$all_data$collapse(keep=1:2) %>% t
      
      d$x <- d$all_data$dimnames$Time
      d$y <- d$all_data$dimnames$Trial
      attr(d$data, 'xlab') = 'Time'
      attr(d$data, 'ylab') = 'Trial'
      
      attr(d$baseline_data, 'xlab') = 'Time'
      attr(d$baseline_data, 'ylab') = 'Trial'
      attr(d$analysis_data, 'xlab') = 'Time'
      attr(d$analysis_data, 'ylab') = 'Trial'
      
      attr(d$data, 'zlab') = 'Mean Voltage'
      
      return(d)
    })
    
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
ravebuiltins:::dev_ravebuiltins(T)
rave:::mount_demo_subject(force_reload_subject = T)
view_layout('voltage_explorer')
    
# Debug
require(ravebuiltins)
dev_toolbox = dev_ravebuiltins(T)
reload_this_package(expose = T, clear_env = F)
mount_demo_subject()

module_id = 'voltage_explorer'

module = ravebuiltins:::debug_module(module_id)
result = module(GROUPS=list(
  list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')))
)

results = result$results
result$by_trial_erp_map()
result$erp_over_time_plot()
result$by_condition_welch()

view_layout('voltage_explorer',
            sidebar_width = 3, launch.browser = T)

m = to_module(module_id)
init_app(m)

ravebuiltins::dev_ravebuiltins(expose_functions = TRUE)
mount_demo_subject()
env = reload_this_package(expose = FALSE, clear_env = TRUE)

# Step 2: make sure rave data is attached
attachDefaultDataRepository()

# Step 3: try to run from local session
module = rave::get_module(package = 'ravebuiltins', module_id = 'voltage_explorer', local = T)

res = module()

