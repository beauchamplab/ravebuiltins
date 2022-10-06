
heat_map_spike_correlation_plot <- function(results, ...) {
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    draw_many_heat_maps(hmaps = results$get_value('spike_correlation_by_frequency'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = 1)
    # ,
    #                     PANEL.LAST = spectrogram_heatmap_decorator(plot_options = po),
    #                     PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator,0)
    # )
    
    # 
    # image(rbind(lc2[21:2,], lag_cors), 
    #       col=colorRampPalette(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff",
    #                              "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061") %>% rev)(101), zlim=c(-1,1),
    #       x=-20:21 * .01, y=heat_map_data[[ii]]$y)
}


by_trial_heat_map_spike_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    by_trial_spike_data <- results$get_value('by_trial_spiking_data')
    
    #base decorator
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    po$draw_decorator_labels = FALSE
    decorator <- by_trial_heat_map_decorator(plot_options = po, title_options = list(
        allow_sample_size=FALSE, allow_freq=FALSE,allow_cond=FALSE
    ), )
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    sort_trials_by_type <- results$get_value('sort_trials_by_type', 'Trial Number')
    if(sort_trials_by_type != 'Trial Number') {
        for(ii in which(results$get_value('has_trials'))) {
            by_trial_spike_data[[ii]] %<>% reorder_trials_by_event(event_name = sort_trials_by_type)
        }
        # add a decorator that can draw the trial labels
        if(sort_trials_by_type == 'Condition') {
            decorator %<>% add_decorator(trial_type_boundaries_hm_decorator)
        } else  {
            decorator %<>% add_decorator(by_trial_analysis_window_decorator(event_name= sort_trials_by_type,
                                                                            show_label = po$draw_decorator_labels))
        }
    }
    
    show_outliers <- results$get_value('show_outliers_on_plots', FALSE)
    if(show_outliers) {
        # print('showing outliers')
        decorator %<>% add_decorator(heatmap_outlier_highlighter_decorator)
    } else {
        # print('not showing outliers, removing them, start with: ' %&% nrow(by_trial_heat_map_data[[1]]$data))
        by_trial_spike_data %<>% remove_outliers_from_by_trial_data
    }
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    need_wide = ('Condition' == sort_trials_by_type)
    
    draw_many_heat_maps(by_trial_spike_data,
                        max_zlim = 1,
                        wide = need_wide,
                        PANEL.LAST=decorator,
                        show_color_bar = F,
                        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
                        axes=c(TRUE, !need_wide))
}


over_time_spike_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    time_series_plot(plot_data = results$get_value('over_time_spike_data'),
                     plot_time_range = results$get_value('plot_time_range'),
                     PANEL.FIRST = time_series_decorator(plot_options=po, do_not_shade = 'baseline_window')
    )
}



generate_spike_matrix <- function(trials, times, seed=NULL) {
    ntrial = length(trials)
    ntime = length(times)
    
    set.seed(seed)
    #baseline firing level
    spike_mat <- replicate(ntrial, rbinom(ntime, 1, prob = runif(sum(ntime), .05, .25)))
    
    # lots of spikes initially
    event_window = times %within% c(-.1,0.1)
    is_v_only = endsWith(trials, '_v')
    
    spike_mat[event_window,!is_v_only] <- spike_mat[event_window,!is_v_only] +
        replicate(sum(!is_v_only), rbinom(sum(event_window), 1, prob = runif(sum(event_window), .15, .3)))
    
    # post event spikes tail off a bit
    event_window = times %within% c(0.11,0.4)
    spike_mat[event_window,!is_v_only] <- spike_mat[event_window,!is_v_only] +
        replicate(sum(!is_v_only), rbinom(sum(event_window), 1, prob = runif(sum(event_window), .1, .2)))
    
    # post event recovery
    event_window = times %within% c(0.41,0.5)
    
    # spikes below baseline
    # dipsaus::cat2('below baseline')
    spike_mat[event_window,!is_v_only] <- replicate(sum(!is_v_only), rbinom(sum(event_window), 1, prob = .05))
    
    #blur the start time?
    event_window = times %within% c(-0.2,0.2)
    spike_mat[event_window,!is_v_only] = spike_mat[event_window,!is_v_only] + replicate(sum(!is_v_only), rbinom(sum(event_window), 1,
                                                       prob = runif(sum(event_window), 0, .35)))
    # dipsaus::cat2('smooth spike trains')
    spike_mat = apply(spike_mat, 2, runmed, k=5)
    
    spike_mat[spike_mat>0] = 1
    
    return (spike_mat)
}


