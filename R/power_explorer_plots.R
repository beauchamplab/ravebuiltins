#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results) {

    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))

        time_series_plot(results$get_value('line_plot_data'),
                         x=results$get_value('time_points'),
                         frequencies=results$get_value('FREQUENCY'),
                         SHADER=window_highlighter(windows=list(results$get_value('BASELINE_WINDOW'), results$get_value('ANALYSIS_WINDOW')), window_names = c('baseline', 'analysis'))
        )
}

#' @title By Trial Plot With Statistics
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
windowed_comparison_plot <- function(results){
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

    # here we need to pass in the decorator because dmhm is going to loop over the heatmaps
    # and take care of drawing a color bar for us
    draw_many_heat_maps(hmaps = results$get_value('heat_map_data'),
                        x = results$get_value('time_points'),
                        y = results$get_value('frequencies'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = results$get_value('max_zlim'),
                        DECORATOR=tf_hm_decorator(TIME_RANGE = results$get_value('ANALYSIS_WINDOW'),
                                                  FREQUENCY = results$get_value('FREQUENCY'),
                                                  BASELINE = results$get_value('BASELINE_WINDOW'))
    )
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
# expects by_trial_heat_map_data to exist
by_trial_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # # need to sort the data

    decorator <- trial_hm_decorator(baseline=results$get_value('BASELINE_WINDOW'))
    # do we need to sort the trials into trial type ordering? (instead of just ascending by trial #)
    by_trial_heat_map_data <- results$get_value('by_trial_heat_map_data')
    time_points <- results$get_value('time_points')
    sort_trials_by_type <- results$get_value('sort_trials_by_type', FALSE)
    if(sort_trials_by_type) {
        for(ii in which(results$get_value('has_trials'))) {
            by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_type
        }
        # change the y axis and draw label boundaries
        decorator <- add_decorator(trial_type_boundaries_hm_decorator,
                                   function(map,x,y,...) trial_hm_decorator(map,x,y,yax=FALSE, baseline=results$get_value('BASELINE_WINDOW')))

    }

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data, wide = sort_trials_by_type,
                        x = results$get_value('time_points'), y=function(m) seq_len(dim(m)[2L]),
                        ylab='Trials', DECORATOR=decorator, allow_log_scale=FALSE, max_zlim = results$get_value('max_zlim'))
}

# show power over time with MSE by condition
time_series_plot <- function(plot_data, x, xlab='Time (s)', ylab='% Signal Change', DECORATOR=ts_labels_only, SHADER,
                             title, frequencies, draw_labels=TRUE) {
    ylim <- pretty(get_data_range(plot_data), min.n=2, n=4)


    ### This should be done by a decorator. time_series_plot shouldn't be taking in a variable for frequencies.
    if(missing(title)) {
        # the unlist here will strip out the NULLS for us
        ns <- unlist(lapply(plot_data, getElement, 'N'))
        title <- 'Freq ' %&% paste0(frequencies, collapse=':') %&% ' || Ns ' %&% paste0(ns, collapse=', ')
    }

    plot_clean(x, ylim, xlab=xlab, ylab=ylab, main='')
    rave_title(title)

    # draw polys and labels for baseline and analysis ranges
    SHADER(ylim, draw_labels)

    # draw each time series
    for(ii in seq_along(plot_data)) {
        with(plot_data[[ii]], {
            if(has_trials) {
                ebar_polygon(x, data[,1], data[,2], add_line = TRUE, col=get_color(ii))
            }
        })
    }

    # if someone wants to add decorations, now is the time
    # we could consider adding this inside the above for loop, not sure which
    # is preferable
    if(is.function(DECORATOR)) DECORATOR(plot_data)

    rave_axis(1, pretty(x))
    rave_axis(2, ylim)

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
