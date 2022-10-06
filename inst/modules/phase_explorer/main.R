# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'phase_explorer', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

# people might choose a time point or a frequency that doesn't exist in the data. Warn them that we are changing this value, but it will not
# be reflected in the UI element

# if(! (FREQUENCY %in% frequencies)) {
#     new_frequency <- frequencies[..get_nearest(FREQUENCY, frequencies)]
# 
#     showNotification(p('Chosen frequency: ', strong(FREQUENCY),
#                        "doesn't exist. Switching to: ", strong(new_frequency)), type = 'warning', id='BAD_FREQ')
#     FREQUENCY <- new_frequency
#     
#     updateSliderInput(session, 'FREQUENCY', value = FREQUENCY)
#     
# }

if(! (analysis_window %in% time_points)) {
    new_aw <- time_points[..get_nearest(analysis_window, time_points)]

    showNotification(p('Chosen analysis point: ', strong(analysis_window),
                       "doesn't exist. Switching to: ", strong(new_aw)), type = 'warning', id='BAD_TP')
    analysis_window <- new_aw
}

dat = phase$subset(Frequency = Frequency %within% FREQUENCY,
                 Electrode = Electrode == ELECTRODE)
if(max_phase_freq <= 0) {
    max_phase_freq = max(phase$dimnames$Frequency)
}

full_data = phase$subset(Electrode = Electrode == ELECTRODE, Frequency = Frequency %within% c(0,max_phase_freq))
print(dim(dat$swap_file))
time = dat$dimnames$Time
frequency = dat$dimnames$Frequency

# print(time)
# print('---')
# print(FREQUENCY)
# print('---')
# print(frequency)
# print('---')
# print(ELECTRODE)
# print('---')

# Make phase data complex numbers
raw_dat <- phase$subset(
    Frequency = Frequency %within% FREQUENCY,
    Electrode = Electrode == ELECTRODE
)
print('------')
val = dat$get_data()
dat$set_data(exp(1i * val))
val = full_data$get_data()
full_data$set_data(exp(1i * val))

# for each groups, calculate inter-trial coherence for each frequencies
lapply(1:length(GROUPS), function(ii) {
    g = GROUPS[[ii]]
    trial_number = epoch_tbl$Trial[epoch_tbl$Condition %in% unlist(g$group_conditions)]
    n_trials = length(trial_number)
    if(!n_trials){
        return(list(
            has_trials = F
        ))
    }

    sub = dat$subset(Trial = Trial %in% trial_number)
    group_phase = raw_dat$subset(Trial = Trial %in% trial_number)
    group_power = NULL#power$subset(Trial = Trial %in% trial_number)

    full_sub <- full_data$subset(Trial = Trial %in% trial_number)

    intc_cplx = sub$collapse(keep = c(3, 2))
    intc_real = Mod(intc_cplx)

    full_intc_real = Mod(full_sub$collapse(keep=c(3,2)))

    list(
        which_group = ii,
        has_trials = T,
        n_trials = n_trials,
        group_name = g$group_name,
        data = intc_real,
        full_data = full_intc_real,
        all_frequencies = full_data$dimnames$Frequency,
        range = range(intc_real),
        full_range = range(full_intc_real),
        name =  sprintf('%s %d Trials', g$group_name, n_trials),
        phase = group_phase,
        pow = group_power
    )
}) ->
    calc_results

n_groups = sum(0, vapply(calc_results, function(x){x$has_trials}, FUN.VALUE = FALSE))

actual_lim <- NULL
if(n_groups > 0){
    actual_lim = lapply(calc_results, function(x){
        x$full_range
    })

    actual_lim = range(unlist(actual_lim))
    actual_lim[abs(actual_lim) < 0.01] = 0
}else{
    actual_lim = c(0,1)
}


has_data <- any(sapply(calc_results, `[[`, 'has_trials'))


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)

# devtools::document()
rave_tools = ravebuiltins::dev_ravebuiltins(expose_functions = T)

# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'phase_explorer'
module = ravebuiltins:::debug_module('phase_explorer')

result = module(analysis_window = 0)
result$phase_histogram()
result$itpc_plot_heatmap()
result$itpc_time_plot()
result$phase_plot()

results = result$results

rave_tools$view_layout('phase_explorer', sidebar_width = 3, launch.browser = T)


