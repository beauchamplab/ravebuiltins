#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))

    time_series_plot(plot_data = results$get_value('line_plot_data'),
                     PANEL.FIRST = time_series_decorator(results = results))
}

#' @title By Trial Plot With Statistics
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
windowed_comparison_plot <- function(results, ...){
    with(results, {
        validate(need((exists('has_data') && (has_data)), "No Condition Specified"))
        trial_scatter_plot(scatter_bar_data)
    })
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

    draw_many_heat_maps(hmaps = results$get_value('heat_map_data'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = results$get_value('max_zlim'),
                        PANEL.LAST = spectrogram_heatmap_decorator(results=results)
    )
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
# expects by_trial_heat_map_data to exist
by_trial_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    #base decorator
    decorator <- trial_hm_decorator(baseline=results$get_value('BASELINE_WINDOW'))
    
    by_trial_heat_map_data <- results$get_value('by_trial_heat_map_data')
    time_points <- results$get_value('time_points')
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    # do we need to sort the trials into trial type ordering? (instead of just ascending by trial #)
    sort_trials_by_type <- results$get_value('sort_trials_by_type', FALSE)
    if(sort_trials_by_type) {
        for(ii in which(results$get_value('has_trials'))) {
            by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_type
        }
        # # change the y axis and draw label boundaries
        # decorator <- add_decorator(trial_type_boundaries_hm_decorator,
        #                            function(map,x,y,...) {
        #                                trial_hm_decorator(map,x,y,yax=FALSE, baseline=results$get_value('BASELINE_WINDOW'))
        #                            })
    }

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        wide = sort_trials_by_type,
                        PANEL.LAST=by_trial_heat_map_decorator(results=results))
    # (trial_labels=sort_trials_by_type)
}

# show power over time with MSE by condition
time_series_plot <- function(plot_data, PANEL.FIRST=NULL, PANEL.LAST=NULL) {
    
    xlim <- pretty(get_list_elements(plot_data, 'x'))
    ylim <- pretty(get_data_range(plot_data), min.n=2, n=4)

    plot_clean(xlim, ylim)
    
    if(isTRUE(is.function(PANEL.FIRST))) PANEL.FIRST(plot_data)

    # draw the axes AFTER the first paneling, should this go in PANEL.FIRST?
    # it's weird because the PANEL.FIRST is responsible for labeling the axes, so why not for drawing them?
    # the counter is that we need the xlim and ylim to make the plot. So it's easy to just draw the labels here
    rave_axis(1, xlim)
    rave_axis(2, ylim)
    abline(h=0, col='gray70')
    
    # draw each time series
    for(ii in seq_along(plot_data)) {
        with(plot_data[[ii]], {
            if(has_trials) {
                ebar_polygon(x, data[,1], data[,2], add_line = TRUE, col=get_color(ii))
            }
        })
    }

    # if someone wants to add "top-level" decorations, now is the time
    if(is.function(PANEL.LAST)) PANEL.LAST(plot_data)

    invisible(plot_data)
}


#
# group_data is a list
# this is a list to allow different N in each group
# NB: the reason we use barplot here is that it takes care of the width between groups for us, even though by default we don't actually show the bars
trial_scatter_plot = function(group_data, ylim, bar.cols=NA, bar.borders=NA, cols, ebar.cols='gray30', ebar.lwds=3, jitr_x,
                              pchs=19, pt.alpha=175, xlab='Group', ylab='Mean % Signal Change', ebar.lend=2, ...) {

    nms <- group_data %>% get_list_elements('name')
    #
    # #yes, sometimes people use the same name for different groups, or don't give names. Let's create new names
    gnames <- paste0(LETTERS[seq_along(nms)], nms)

    #
    ns <- group_data %>% get_list_elements('N')

    yax <- do_if(missing(ylim), {
        pretty(get_data_range(group_data), high.u.bias = 100, n=4, min.n=3)
    }, ylim)

    #there are edge cases where length(mses) != length(names), take care of this with `ind` below
    bp_names <- paste0(nms, ' (N=' %&% ns %&%')')

    # this creates space for empty groups -- is this expected behavior? It is good to preserve the color
    # mapping, but I'd rather not have the empty, space... so we need to preserve the colors but not the empty space
    ind <- which(unlist(lapply(group_data, '[[', 'has_trials')))
    mses <- sapply(ind, function(ii) group_data[[ii]]$mse)

    x <- rave_barplot(mses[1,],
                      ylim=.fast_range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
                      ylab=ylab, names.arg=bp_names[ind], xlab=xlab, axes=F, ...)

    rave_axis(2, at=yax)

    if(min(yax) < 0) abline(h=0, col='lightgray')

    # grabbing an attribute from the group data
    if(not_null(attr(group_data, 'stats')))
        rave_title(as.title(pretty(attr(group_data, 'stats'))))

    #emphasize the means
    lsize <- (1/3)*mean(unique(diff(x)))
    # this means there is only 1 group. If there is one group, the barplot seems to get placed at 0.7, with
    # the usr range being 0.16 - 1.24.
    if(is.na(lsize)) lsize <- 1/3

    if(missing(jitr_x)) jitr_x <- 0.75*lsize

    if(missing(cols)) cols <- get_color(seq_along(group_data))

    # Ensure all parameters are sufficiently long. This feels extravagant, but needed because we're indexing into these variables
    # and we don't want to reach beyond the end
    par_rep <- function(y) rep_len(y, length(group_data))

    cols %<>% par_rep
    pchs %<>% par_rep
    bar.cols %<>% par_rep
    ebar.cols %<>% par_rep
    bar.borders %<>% par_rep

    # x may not be the same length as group_data because we're skipping empty groups
    # we still want everything else to be based on group number
    xi <- 1
    for(ii in seq_along(group_data)) {
        if(group_data[[ii]]$has_trials) {

            lines(x[xi] + c(-lsize, lsize), rep(mses[1, xi], 2), lwd=3, lend=ebar.lend, col=ebar.cols[ii])

            add_points(x[xi], group_data[[ii]]$data,
                       col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)

            ebars.y(x[xi], mses[1,xi], mses[2,xi],
                    lwd=ebar.lwds, col=ebar.cols[ii], code=0, lend=ebar.lend)
            xi <- xi+1
        }
    }

    # in case people need to further decorate
    invisible(x)
}
