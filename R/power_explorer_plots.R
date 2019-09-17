#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    time_series_plot(plot_data = results$get_value('line_plot_data'),
                     PANEL.FIRST = time_series_decorator(results = results))
}

get_foreground_color <- function() {
    switch(par('bg'),
           'black' = 'white',
           'white' = 'black',
           '#1E1E1E' = 'gray70',
           'gray' = '#A5A5A5'
    ) 
}

#works by side effect to change the palette used by the current graphics device
set_palette_helper <- function(results, ...) {
    .bg <- results$get_value('background_plot_color_hint', 'white')
    
    # setting the background color here triggers a cascade of color changes
    if(.bg == 'Gray') {
        par('bg'='#1E1E1E')
    } else {
        par('bg'=.bg)
    }
    
    pal <- get_palette(results$get_value('color_palette'))
    
    if(results$get_value('invert_colors_in_palette', FALSE)) {
        .alpha <- col2rgb(pal, alpha=TRUE)[4,]
        pal = apply(col2rgb(pal), 2, function(rgb) 255-rgb)
        pal %<>% rgb(alpha=.alpha, maxColorValue = 255)
    }
    
    if(results$get_value('reverse_colors_in_palette', FALSE)) {
        pal %<>% rev
    }
    
    set_palette(pal)
    
    invisible()
}


#' @title By Trial Plot With Statistics
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
windowed_comparison_plot <- function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    trial_scatter_plot(
        group_data = results$get_value('scatter_bar_data'),
        show_outliers = results$get_value('show_outliers_on_plots')
    )
}

#' @title Basic Time Frequency Plot
#'
#' @param results results returned by module
#' @param ... other parameters passed to module output
#'
#' @examples
#' \dontrun{
#' rave_prepare(...)
#' fn = ravebuiltins:::get_module('power_explorer')
#' res = fn()
#' heat_map_plot(res$result)
#' }
#' @export
heat_map_plot <- function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    
    draw_many_heat_maps(hmaps = results$get_value('heat_map_data'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = results$get_value('max_zlim'),
                        PANEL.LAST = spectrogram_heatmap_decorator(results=results)
    )
}


by_electrode_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    by_electrode_heat_map_data <- results$get_value('by_electrode_heat_map_data')
    
    draw_many_heat_maps(by_electrode_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        PANEL.LAST=by_electrode_heat_map_decorator(results=results))
    
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
# expects by_trial_heat_map_data to exist
by_trial_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    
    by_trial_heat_map_data <- results$get_value('by_trial_heat_map_data')
    
    #base decorator
    decorator <- by_trial_heat_map_decorator(results=results)
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    sort_trials_by_type <- results$get_value('sort_trials_by_type', FALSE)
    if(sort_trials_by_type) {
        for(ii in which(results$get_value('has_trials'))) {
            by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_type
        }
        # add a decorator that can draw the trial labels
        decorator %<>% add_decorator(trial_type_boundaries_hm_decorator)
    }

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        wide = sort_trials_by_type,
                        PANEL.LAST=decorator,
                        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
                        axes=c(TRUE, !sort_trials_by_type))
}
