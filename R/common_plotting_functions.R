
#' @author John Magnotti
#' @title Draw several heatmaps in a row and (optionally) a color bar
#' @param hmaps data to draw heatmaps
#' @param log_scale draw y in log scale?
#' @param show_color_bar show color legend to the right? Future: Will will check to see if this parameter is a function. If so, we
#' can call it to allow arbitrary legends in the right-most (half) panel
#' @param max_zlim zlim that trims z value
#' @param wide boolean. should we use a wider margin on the left? defaults to false
#' @param useRaster,... passed to image()
#' @param PANEL.FIRST a function that is called after each plot window has been created, but before any rendering is done. In truth, this is currently called AFTER the call to image(),
#' so if you draw within the plotting region it will overwrite the heatmap. To fix this requires editing draw_img(...) to allow for a function to be called after creation but before rendering.
#' Don't depend on this call order, use PANEL.LAST if you want to draw things on top of the heatmap
#' @param PANEL.LAST a function that is called after the rendering of each heat map. It is not called after the rendering of the color bar. 
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar.
#' By default it is setup for time/freq, but by swapping labels and decorators you can do anything.
#' @seealso layout_heat_maps
#' @seealso draw_img
draw_many_heat_maps <- function(hmaps, max_zlim=0, log_scale=FALSE,
                                show_color_bar=TRUE, useRaster=TRUE, wide=FALSE,
                                PANEL.FIRST=NULL, PANEL.LAST=NULL, axes=c(TRUE, TRUE), ...) {

    k <- sum(hmaps %>% get_list_elements('has_trials'))
    orig.pars <- layout_heat_maps(k)
    on.exit({
        par(orig.pars)
    })

    # this is to add some extra spacing on the LEFT margin to allow, e.g., longer axis titles
    # we could also set this adaptively based on the max(nchar(...)) for the appropriate labels from hmap[[ii]] condition names
    if(wide) {
        par(mar = c(5.1, 7, 2, 2))
    }

    # actual data range, as opposed to the max zlim which controls the plottable range
    actual_lim = get_data_range(hmaps)

    if(max_zlim==0) {
        max_zlim <- max(abs(actual_lim))
    }

    log_scale <- if(isTRUE(log_scale)) {
        'y'
    } else {
        ''
    }
    
    lapply(hmaps, function(map){
        if(map$has_trials){
            # we are linearizing the x and y spaces so that we can use the fast raster
            x <- seq_along(map$x)
            y <- seq_along(map$y)
            
            # because image plot centers the data on the y-variable, it can introduce 0s which fail when log='y'
            # so we shift y by a small amount so that the minimum is > 0
            dy <- 0
            if(log_scale == 'y') {
                dy <- (y[2]-y[1])/2 + min(y)
                #FIXME I think this may be making the edge boxes too small cf. the else block where we pad 0.5
                plot_clean(x,y+dy, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, log='y')
            } else {
                pad = c(-0.5, 0.5)
                plot_clean(xlim=range(x) + pad,
                           ylim=range(y) + pad)#, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, log='y')
            }
            
            if(is.function(PANEL.FIRST)) {
                PANEL.FIRST(map)
            }
            
            # make sure the decorators are aware that we are linearizing the y scale here
            make_image(map$data, x=x, y=y, log=ifelse(log_scale, 'y', ''), zlim=c(-1,1)*max_zlim)
            
            xticks <- ..get_nearest(pretty(map$x), map$x)
            yticks <- ..get_nearest(pretty(map$y), map$y)

            axes %<>% rep_len(2)
            if(axes[1])
                rave_axis(1, at=xticks, labels=map$x[xticks], tcl=0, lwd=0)
            if(axes[2])
                rave_axis(2, at=yticks, labels=map$y[yticks], tcl=0, lwd=0)

            if(is.function(PANEL.LAST)) {
                #PANEL.LAST needs to know about the coordinate transform we made
                PANEL.LAST(map,
                           Xmap=function(x) ..get_nearest_i(x, map$x),
                           Ymap=function(y) ..get_nearest_i(y, map$y))
            }
        }
    })
    
    if(show_color_bar){
        par(mar=c(5.1, 4.5, 2, 2),
            mai = c(0.6732, 0.5412, 0.5412, 0.2772))
        
        .ylab = ''
        ii = 1
        while(ii <= length(hmaps)) {
            if(hmaps[[ii]]$has_trials) {
                .ylab <- attr(hmaps[[ii]]$data, 'zlab')
                ii <- 1e10
            }
            ii = ii + 1
        }
        rave_color_bar(max_zlim, actual_lim, ylab=.ylab)
    }

    invisible(hmaps)
}

# show power over time with MSE by condition
time_series_plot <- function(plot_data, PANEL.FIRST=NULL, PANEL.LAST=NULL) {
    
    xlim <- pretty(get_list_elements(plot_data, 'x') %>% unlist)
    ylim <- pretty(get_data_range(plot_data) %>% unlist, min.n=2, n=4)
    
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
                ebar_polygon(x, data[,1], data[,2], add_line = TRUE, col=ii)
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
    
    
    .col <- if(par('bg')=='black') {
        'white'
    } else {
        'black'
    }
    
    x <- rave_barplot(mses[1,],
                      ylim=.fast_range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
                      names.arg=bp_names[ind], col.axis=.col, axes=F, ...)
    
    # putting this here, but move this out to a PANEL.FIRST
    axis_label_decorator(group_data)
    
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
    
    # if(missing(cols)) cols <- get_color(seq_along(group_data))
    if(missing(cols)) cols <- grDevices::palette()
    
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

# the Xmap and Ymap here are functions that allow for transformation of the plot_data $x and $y into
# the coordinate system of the plot
spectrogram_heatmap_decorator <- function(plot_data, results, Xmap=force, Ymap=force, btype='line', atype='box', 
                                          title_options=list(allow_freq=FALSE), ...) {
    
    shd <- function(plot_data, Xmap=Xmap, Ymap=Ymap) {
        title_options$plot_data = plot_data
        title_options$results=results
        
        do.call(title_decorator, args=title_options)
        
        axis_label_decorator(plot_data)
        
        windows <- list(
            'Baseline'=list(
                window = Xmap(results$get_value('BASELINE_WINDOW')),
                type=btype
            ),
            'Analysis'=list(
                window = if(atype=='box') {
                    list(x=Xmap(results$get_value('ANALYSIS_WINDOW')),
                         y=Ymap(results$get_value('FREQUENCY')))
                } else {
                    Xmap(results$get_value('ANALYSIS_WINDOW'))
                },
                type=atype
            )
        )

        lapply(names(windows), function(nm) {
            if(paste(nm, 'Window') %in% results$get_value('PLOT_TITLE')) {
                with(windows[[nm]],
                     window_decorator(
                         window=window, type=type,
                         text=ifelse(results$get_value('draw_decorator_labels'), nm, '')
                     )
                )
            }
        })
        invisible(plot_data)
    }
    
    if(missing(plot_data)) {
        return (shd)
    }
    
    shd(plot_data, Xmap, Ymap)
}

# here we just call the spectrogram decorator with some special setup options
by_trial_heat_map_decorator <- function(plot_data=NULL, results, Xmap=force, Ymap=force, ...) {
    args <- list(
        results=results, Xmap=Xmap, Ymap=Ymap, atype='line', btype='line',
        title_options = list(allow_sample_size=FALSE),
        ...
    )
    
    if(is.null(plot_data)) {
        return(do.call(spectrogram_heatmap_decorator, args = args))
    }
    
    args$plot_data=plot_data
    do.call(spectrogram_heatmap_decorator, args = args)
}






#' @author John Magnotti
#' @title RAVE custom image plotter
#' @param zmat z-matrix
#' @param x,y z and y axis
#' @param xlab, ylab label for x and y
#' @param zlim value to trim zmat
#' @param log which axis will be in log scale
#' @param useRaster,... passed to image()
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators.
#' We are just plotting image(mat) Rather Than t(mat) as you might expect. The Rave_calculators know this so we can save a few transposes along the way.
make_image <- function(mat, x, y, zlim, col, log='', useRaster=TRUE, clip_to_zlim=TRUE, add=TRUE) {
    #xlab='Time (s)', ylab='Frequency (Hz)', zlim, log='', useRaster=TRUE, PANEL.FIRST=NULL, PANEL.LAST=NULL, ...) {
    # zmat %<>% clip_x(lim=zlim)
    
    if(missing(zlim)) {
        zlim <- c(-1,1)*max(abs(mat))
    } else {
        # if zlim is missing, then the zlim will be set symmetrically based on the range
        # of the data (in the 'if' block above), so we only have to worry about clipping if the range is passed in
        if(clip_to_zlim) {
            mat %<>% clip_x(zlim)
        }
    }
    
    col %?<-% if (par('bg')=='black') {
        rave_heat_map_dark_colors
    } else {
        rave_heat_map_colors
    }
    
    image(x=x, y=y, z=mat, zlim=zlim, col=col, useRaster=useRaster, log=log,
          add=add, axes=F, xlab='', ylab='', main='')

    # return the clipped zmat
    invisible(mat)
}
# for compatibility
# draw_img <- make_image

# setup so that heatmaps look nice and you have enough space for the color bar
# ratio: heatmap to color bar width ratio
# k is the number of heatmaps, excluding the color bar
layout_heat_maps <- function(k, ratio=4) {
    opars <- par(no.readonly = TRUE)
    layout(matrix(1:(k+1), nrow=1), widths=c(rep(ratio, k), lcm(5)) )
    par(mar=c(5.1, 4.5, 2, 2))
    invisible(opars)
}

##RUTABAGA
median_ticks <- function(k, .floor=1) c(.floor, ceiling(k/2), k)

`conditional_sep<-` <- function(str, value = '', sep=' ') {
    if(isTRUE(nchar(str) > 0)) {
        str = paste0(str, sep)
    }
    str = paste0(str, value)
}

##RUTABAGA
# helper function to write out plots as PDF
as_pdf = function(fname, w, h, expr, TEST=FALSE, bg='transparent') {
    if(! TEST) {
        on.exit(dev.off())

        fname <- fix_pdf_name(fname)
        pdf(fname, width=w, height=h, useDingbats=FALSE, bg=bg)
        res = eval(expr)
    } else {
        res = eval(expr)
    }
    return (invisible(res))
}


##RUTABAGA
fix_pdf_name <- function(fname) {
    if(!grepl("\\.pdf$", fname)) {
        fname = paste0(fname, ".pdf")
    }
    return(fname)
}


str_rng <- function(rng) sprintf('[%s]', paste0(rng, collapse=':'))

rave_color_bar <- function(zlim, actual_lim, clrs, ylab='Mean % Signal Change',
                           mar=c(5.1, 5.1, 2, 2)) {
    
    clrs %?<-% if(par('bg') == 'black') {
        rave_heat_map_dark_colors
    } else {
        rave_heat_map_colors
    }
    
    cbar <- matrix(seq(-zlim, zlim, length=length(clrs))) %>% t
    par(mar=mar)
    image(cbar,
          col=clrs, axes=F, ylab=ylab, main='',
          cex.main=rave_cex.main*.8, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis)

    # rave_main(str_rng(actual_lim %>% round))
    rave_axis(2, at=0:2/2, labels = c(-zlim, 0, zlim) %>% round, tcl=0.3)
    box()

    invisible(zlim)
}

##RUTABAGA
midpoint <- function(x) {
    sapply(seq_along(x)[-1], function(ii) {
        (x[ii] - x[ii-1])/2 + x[ii-1]
    })
}

# this is really only used by the by_trial heat map, but that gets used in multiple modules, so it's here....
reorder_trials_by_type <- function(bthmd) {
    # we want to sort but preserve the order that the conditions were added to the group, but this doesn't do that
    ind <- sapply(bthmd$conditions, function(ttype) which(ttype==bthmd$trials), simplify = FALSE)

    .xlab <- attr(bthmd$data, 'xlab')
    .zlab <- attr(bthmd$data, 'zlab')
    
    bthmd$data <- bthmd$data[,unlist(ind)]
    
    # set the axis labels
    attr(bthmd$data, 'xlab') <- .xlab
    attr(bthmd$data, 'ylab') <- ''
    attr(bthmd$data, 'zlab') <- .zlab
    
    bthmd$lines <- cumsum(c(sapply(ind, length)))
    bthmd$ttypes <- names(ind)
    bthmd$trials <-  bthmd$trials[unlist(ind)]

    return(bthmd)
}

#sometimes we don't need the last item
# if you give me < 1 I will return the full vector with a warning
# this is helpful as it (I think) avoids error checking and
# should be reasonable for most events
#RUTABAGA
remove_tail = function(x, k=1) {
    if(k<1) {
        warning('Tried removing less than 1 element, k = ', k)
        return(x)
    }
    #k>=1
    lx <- length(x)
    stopifnot(k<lx)
    #seems like it's faster to rewrite as selecting from the beginning
    #rather than using negative indexing
    # x[-(length(x):(length(x) - (k-1)))]
    x[1:(lx-k)]
}




# # # # Plot Decorators

#the idea here is to allow decorators to be stacked/piped more easily while still deferring execution until
# a more opportune time. f1 %>% f2 won't pass along the ... parameters (although it will pass along x), so this format is not used here
add_decorator <- function(f1, f2, ...) {
    force(f1); force(f2)
    return (function(.IN., ...) {
        f1(.IN., ...)
        f2(.IN., ...)
    })
}


# all decorators should return a function that does the actual decorating. All of the ancillary options related to decorating should
# be set at creation time, and then all the (generated) decorator function needs is the data.



# here we are creating a new decorator that uses component-esque pattern to draw
# lines after the initial decoration is done
trial_type_boundaries_hm_decorator <- function(map, ...) {
    with(map, {
        if(length(map$lines)>1) {
            abline(h=remove_tail(map$lines) + 0.5, lwd=2, col=rave_colors$TRIAL_TYPE_SEPARATOR)
            # draw the trial type labels
            yat <- c(map$lines[1]/2, midpoint(map$lines))
        } else {
            yat <- median(y)
        }
        rave_axis(2, tcl=0, lwd=0, at=yat, labels=map$ttypes)
    })

    invisible(map)
}

window_highlighter <- function(ylim, draw_labels=TRUE, window, window_name) {
    do_wh <- function(ylim, draw_labels) {
            clr <- rave_colors[[toupper(window_name %&% '_window')]]
            clr %?<-% 'gray50'
            
            do_poly(window, range(ylim), col=clr)
            
            if(draw_labels) {
                text(min(window), max(ylim), window_name, col=clr, adj=c(0,1))
            }
    }

    if(missing(ylim)) {
        return(do_wh)
    }

    do_wh(ylim, draw_labels)
}

# helper function for drawing vertical borders
vertical_borders <- function(x, y, lbl, clr, ..., alpha=150, lwd=4, draw_labels=TRUE) {
    abline(v=range(x), col=getAlphaRGB(clr, alpha), lwd=lwd, ...)

    if(draw_labels)
        text(median(x), max(y), '<- ' %&% lbl %&% ' ->', col=clr)
}

# helper to show multiple different time windows
# this is written as a function generator so you can effectively cache the TIMES list
create_multi_window_shader <- function(TIMES, clrs) {

    if(missing(clrs)) clrs <- rep('gray30', length(TIMES))
    if(length(clrs) < length(TIMES)) {
        clrs <- c(clrs, rep(clrs, length.out=length(TIMES)-length(clrs)))
    }

    return (function(ylim, draw_labels, ...) {
        for(ti in seq_along(TIMES)){
            x <- unlist(TIMES[[ti]]$RANGE)
            if(diff(x) > 0) {
                vertical_borders(
                    x, ylim, TIMES[[ti]]$TIME_NAME,
                    clrs[ti], lty=1 + (ti-1)%%6
                )
            }
        }
        vertical_borders(BASELINE, ylim,
                         'baseline', rave_colors$BASELINE_WINDOW)
        abline(h=0, col='gray70')
    })
}


# window_lines <- function(ylim, ...) {
#     txts <- c('baseline', 'analysis')
#     mapply(vertical_borders,
#            list(BASELINE, TIME_RANGE),
#            list(ylim, ylim),
#            txts,
#            rave_colors[toupper(txts %&% '_window')]
#     )
#     abline(h=0, col='gray70')
# 
# }

# 
axis_label_decorator <- function(plot_data, col) {
    # here we are assuming that everything in plot_data 
    # is of the same x/y type
    
    # we  need to check if we've been give a list of things to plot (as is common for line plots),
    # or a single thing (as is common for the case for heatmaps)
    # test if there are plot variables at the highest level, if so, then we have k=1
    pd <- plot_data
    if(is.null(pd[['has_trials']])) {
        ii = which(get_list_elements(pd, 'has_trials'))[1]
        pd <- pd[[ii]]
    } 
    
    col %?<-% if(par('bg') == 'black') {
        'white'
    } else {
        'black'
    }
    title(xlab=attr(pd$data, 'xlab'),
          ylab=attr(pd$data, 'ylab'),
          cex.lab=rave_cex.lab, col.lab=col)
}


# by default we use PLOT_TITLE variable in results to see what to put in the title string
# callers can override this behavior by specifically dis-allowing certain options
# currently you can't force something to be TRUE if a user doesn't allow it, but we can think about 
# this. If that's the case, all the allow_* would be NULL by default, and setting them to TRUE would override 
# user preference. This seems rude at best, but for certain plots maybe they really require something to 
# be put in the title?
title_decorator <- function(plot_data, results,
                            allow_sid=TRUE, allow_enum=TRUE, allow_freq=TRUE, allow_cond=TRUE, allow_sample_size=TRUE, ...) {
    title_string = ''
    .plot_options <- results$get_value('PLOT_TITLE')
    
        # wraps do_on_inclusion to make ths following lines easier to understand
    add_if_selected <- function(id, expr) {
        do_on_inclusion(id, expr, .plot_options)
    }
    
    
    if(allow_cond)
        add_if_selected('Condition', {
            .name <- plot_data[['name']]
            if(nchar(.name) > 0) {
                .name <- '' %&% .name
            }
            title_string = .name
        })
    
    # we could write this as a simply m/sapply if the variable names had a clear relationship to one another
    if(allow_sid)
        add_if_selected('Subject ID', {
            conditional_sep(title_string) = results$get_value('subject_code')
        })
    
    if(allow_enum)
        add_if_selected('Electrode #', {
            conditional_sep(title_string) = 'E' %&% results$get_value('ELECTRODE')
        })
    
    if(allow_freq)
        add_if_selected('Frequency Range', {
            conditional_sep(title_string) = 'Freq ' %&% paste0(results$get_value('FREQUENCY'), collapse=':')
        })
    
    if(allow_sample_size) 
        add_if_selected('Sample Size', {
            if(!is.null(plot_data[['N']])) 
                conditional_sep(title_string) = 'N=' %&% plot_data$N
        })
    
    # rave_title is an "additive" instead of replacement call, so rendering an empty string won't hurt anything,
    # but let's save a few needless function calls
    if(nchar(title_string) > 0) {
        rave_title(title_string)
    }
    
    invisible()
}

# helper to reduce redundancies in searching then evaluating
do_on_inclusion <- function(needle, expr, haystack) {
    if(needle %in% haystack) {
        eval(expr)
    }
}


#
# helper that calls out to sub-decorators based on user-selected options
#
time_series_decorator <- function(plot_data, results, ...) {
    .plot_options <- results$get_value('PLOT_TITLE')
    
    do_tsd <- function(plot_data) {
        # plot title
        title_decorator(plot_data, results, allow_sample_size=FALSE, allow_cond = FALSE)
        
        # axis labels
        axis_label_decorator(plot_data)
        
        sapply(c('Baseline', 'Analysis'), function(nm) {
            if(paste(nm, 'Window') %in% .plot_options) {
                full_name <- toupper(nm) %&% '_WINDOW'
                if(!results$get_value('draw_decorator_labels'))
                    nm <- FALSE
                
                window_decorator(results$get_value(full_name),
                                 type='shaded', shade.col = rave_colors[[full_name]], text = nm)
            }
        })
        
        # legend options, translate the names into fields available in each element of plot_data
        legend_include = c('name', 'N')[c('Condition', 'Sample Size') %in% .plot_options]
        legend_decorator(plot_data, include = legend_include)
    }

    if(missing(plot_data)) {
        return (do_tsd)        
    } 
    
    do_tsd(plot_data)
}

# this decorator takes care of checking if has_data==TRUE and only shows labels for which there is data
# I guess this could be an option to include N==0 labels...
legend_decorator <- function(plot_data, include=c('name', 'N'), location='topleft') {

    valid.names <- c('name', 'N')
    
    if(length(include) < 1 || !any(include %in% valid.names)) {
        return (invisible(plot_data))
    }
    
    ii <- which(plot_data %>% get_list_elements('has_trials'))
    nms <- plot_data %>% get_list_elements('name', drop_nulls = FALSE) %>% extract(ii)
    ns <- plot_data %>% get_list_elements('N', drop_nulls = FALSE) %>% extract(ii)
    
    if('name' %in% include) {
        legend_text = nms
    } else if ('N' %in% include) {
        legend_text = paste0('n=',ns)
    }
    
    # handle the case where both are included. a little duplication, but clearer
    if (all(valid.names %in% include)) {
        legend_text = paste0(nms, ' (n=', ns, ')')
    }

    legend(location, legend=legend_text, ncol=ceiling(length(ii)/3),
           inset=c(.025,.075), bty='n',
           text.col=ii, cex=rave_cex.lab)

    invisible(plot_data)
}

window_decorator <- function(window, type=c('line', 'box', 'shaded'),
                             line.col, shade.col='gray60',
                             text=FALSE, text.col, lwd, lty) {
    type <- match.arg(type)
    text.x <- window[1]
    text.y <- par('usr')[4] * .9
    
    
    line.col %?<-% if(par('bg') == 'black') {
        'white'
    } else {
        'black'
    }
    
    switch(type, 
           line = {
               lwd %?<-% 1
               lty %?<-% 2
               text.col %?<-% line.col
               
               abline(v=unlist(window), lwd=lwd, lty=lty, col=line.col)
           },
           box = {
               lwd %?<-% 2
               lty %?<-% 2
               text.col %?<-% line.col
               
               if(any(is.null(window$x), is.null(window$y)) ) {
                   warning("window must be a list with x and y components to draw a box")
                   text=FALSE
               } else {
                   with(window, rect(x[1], y[1], x[2], y[2], lwd=lwd, lty=lty, border=line.col, col=NA))
                   
                   text.x <- window$x[1]
                   
                   # the multipler on the box here needs to be based on the size of the plot to reduce
                   # the likelihood of over-printing. Basically we plot just above the analysis window (2.5% of the plottting range), or else 
                   # at 90% of the figure region, whichever is lower
                   yfac <- diff(par('usr')[4] * c(.975,1))
                   text.y <- min(par('usr')[4]*.9, window$y[2] + yfac)
               }
           },
           shaded = {
               text.col %?<-% shade.col
               
               x = window
               y = par('usr')[3:4]
               text.y = par('usr')[4]*0.95
               if(is.list(window)) {
                   x = window$x
                   y = window$y
                   text.x <- window$x[1]
                   
                   yfac <- diff(par('usr')[4] * c(.975,1))
                   text.y <- min(par('usr')[4]*.95, window$y[2] + yfac)
               }
               do_poly(x, y, col=shade.col)
           })
    
    if(!isFALSE(text)) {
        text(text.x, text.y, labels = text, pos=4, col=text.col, cex=rave_cex.lab)
    }
}

#
# x = sort(rnorm(10))
# ..get_nearest(pretty(x), x)
# RUTABAGA
..get_nearest_i <- function(from,to) {
    sapply(from, function(.x) which.min(abs(.x-to)))
}
..get_nearest <- ..get_nearest_i

..get_nearest_val <- function(from,to) {
    to[..get_nearest_i(from,to)]
}

# RUTABAGA
`%near%` <- function(x, y, eps=1e-4) {
    abs(x-y) < eps
}

heat_map_axes <- function(x, y, xlab, ylab, xax=TRUE, yax=TRUE, yntick=6) {
    if(xax) {
        .px <- pretty(x)
        if(missing(xlab)) {
            xlab <- .px
        } else {
            xlab <- xlab[..get_nearest(.px, x)]
        }

        rave_axis(1, at=.px, labels = xlab, tcl=0, lwd=0)
    }
    if(yax) {
        .qy <- quantile(y, 0:(yntick-1)/(yntick-1)) %>% round
        if(missing(ylab)) {
            ylab <- .qy
        } else {
            ylab <- ylab[..get_nearest(.qy, y)]
        }

        rave_axis(2, at=.qy, labels = ylab, tcl=0, lwd=0)
    }
}


#
trial_hm_decorator <- function(hmap, baseline, x, y, xax=TRUE, yax=TRUE, analysis=FALSE, ...) {

    do_thmd <- function(hmap, x, y, ...) {
        list2env(list(...), environment())
        if(!isFALSE(baseline)) {
            abline(v=baseline, lty=3, lwd=2)
        }
        if(!isFALSE(analysis)) {
            abline(v=analysis, lty=1, lwd=2)
        }
        heat_map_axes(x,y, xax=xax, yax=yax)
    }

    if(any(c(missing(hmap), missing(x), missing(y)))) {
        return (do_thmd)
    }

    do_thmd(hmap, x, y, ...)
}

# decorate a heatmap
tf_hm_decorator <- function(hmap, results, ...)
    # x, y, xlab=x, ylab=y, ..., label.col='black', draw_time_baseline=TRUE, xax=TRUE, yax=TRUE,
    #                         TIME_RANGE = NULL, FREQUENCY = NULL, BASELINE = NULL)
{

    do_tfhmd <- function(hmap, x, y, ...) {
        list2env(list(...), environment())

        heat_map_axes(x,y, xax=xax, yax=yax)

        if(draw_time_baseline) {
            # These variables are in ...
            xy <- cbind(TIME_RANGE, FREQUENCY)

            #TODO refactor with rutabaga::do_poly
            polygon(c(xy[,1], rev(xy[,1])) , rep(xy[,2], each=2), lty=2, lwd=3, border=label.col)

            #draw baseline region
            abline(v=BASELINE, lty=3, lwd=2, col=label.col)

            # label baseline region
            text(median(BASELINE), quantile(y, .7), 'baseline', col=label.col, cex=rave_cex.lab, pos=3)
            arrows(BASELINE[1], quantile(y, .7), BASELINE[2], col=label.col, length=.1, code=3)
        }
    }

    if(any(c(missing(hmap), missing(x), missing(y)))) {
        return (do_tfhmd)
    }

    do_tfhmd(hmap=hmap, x=x,y=y)
}

#' Create a easy layout for multiple plots sharing the same x,y and legend
#' @author Zhengjia Wang, John Magnotti
#' @description Provide easy ways to set plot layouts
#' @param K number of plots to be made
#' @param nrows number of rows for the plot, default 1
#' @param legend expression for generating legend, see "?legend"
#' @param legend_size legend width/height, default is lcm(3)
#' @param legend_side 1 - bottom, 2 - left, 3 - top, 4 - right. Default is 4
#' @param s_margin margins within each plots see "?par" for "mar"
#' @param b_margin margins for the whole plot see "?par" for "oma"
#' @param l_margin legend margin
easy_layout <- function(K, nrows = 1, legend,
                        legend_size = lcm(3), legend_side = 4,
                        s_margin = par('mar'), b_margin = par('oma'),
                        l_margin = local({
                            mar = s_margin;
                            mar[legend_side] = 0;
                            mar[(legend_side + 2) %% 4] = 0.5;
                            mar
                        })){
#TODO RUTABAGA

    # calculate nrow and ncols
    ncols = ceiling(K / nrows)
    K = nrows * ncols
    mat = matrix(seq_len(K) + 1, nrow = nrows, byrow = T)


    switch (as.character(legend_side),
            '1' = {
                mat = rbind(mat, 1)
                layout(mat, heights = c(rep(1, nrows), legend_size))
            },
            '2' = {
                mat = cbind(1, mat)
                layout(mat, widths = c(legend_size, rep(1, ncols)))
            },
            '3' = {
                mat = rbind(1, mat)
                layout(mat, heights = c(legend_size, rep(1, nrows)))
            },
            {
                mat = cbind(mat, 1)
                layout(mat, widths = c(rep(1, ncols), legend_size))
            }
    )
    oma = par('oma')
    mar = par('mar')
    re = list(
        oma = oma,
        mar = mar
    )

    par(oma = b_margin)

    # draw legend first!
    parent_env = parent.frame()
    expr = eval(substitute(substitute(legend)), parent_env)

    par(mar = l_margin)
    eval(expr, envir = new.env(parent = parent_env))

    par(mar = s_margin)
}

#TODO RUTABAGA
# Try to guess decimal points of the number and returns string
pretty_num <- function(x, digits = 3, roundup = 5, decimal.mark = '.', ...){
    ss = base::prettyNum(x, ...)
    s = unlist(strsplit(ss, ''))
    sel = s == decimal.mark
    if(sum(sel)){
        e = which(sel) + digits
        l = length(s)
        if(l > e && s[e+1] >= roundup){
            s[e] = as.character(as.integer(s[e]) + 1L)
        }
        end = min(e, l)
        paste(s[1:end], collapse = '')
    }else{
        ss
    }
}




#TODO
# have a parameter called "center" that allows all histograms to be centered
# enabling easier comparison of the concentraion/dispersion kappa
hist.circular <- function(x, ymax, nticks=3, digits=1, breaks=20, col='black', ...) {
    x.deg <- x %>% deg %>% circular(units='degrees')

    x.hist <- hist(as.numeric(x.deg), plot=FALSE, breaks=breaks)

    # table(round(4*x)/4) %>% pscl

    # now that we have the angles, make a polygon
    x.hist$prob <- x.hist$counts %>% pscl

    .k <- (.99*breaks)/2

    xS <- x.hist$prob * cos(rad(x.hist$mids-.k))
    xF <- x.hist$prob * cos(rad(x.hist$mids+.k))

    yS <- x.hist$prob * sin(rad(x.hist$mids-.k))
    yF <- x.hist$prob * sin(rad(x.hist$mids+.k))

    # Draw polar axes, three evenly spaced lines

    if(missing(ymax)) {
        ymax <- max(pretty(x.hist$prob))
    }
    tcks <- 0.01*round(100*seq(0, ymax, length=nticks+1)[-1])
    ylim <- c(-1,1)*ymax

    plot_clean(ylim, ylim, asp=1, ...)
    #the white borders here ensure the lines don't smear together
    for(ii in seq_along(xS)) {
        polygon(c(0, xS[ii], xF[ii], 0), c(0, yS[ii], yF[ii], 0), col = col, border='white')
    }
    # seq(max(ylim) / 3, max(ylim))
    # .k <- 500#as.integer(10*.r * 24)
    sapply(tcks, function(.r) {
        .theta <- rad(seq(-180, 180, length.out=360))
        lines(.r * cos(.theta), .r * sin(.theta), col='gray50' %>% getAlphaRGB(150),
              lwd=1, lty=1)#, cex=.5, pch=21, col='white', type='l')
    })

    #TODO this should be done via an (optional) decorator
    text(0, tcks[tcks>0.001], tcks[tcks > 0.001], cex=1.2, col='gray50', pos=1)

    invisible(x.hist)
}



# # # Colors
#' @export
get_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
    # Some of these are from:
    # https://colorhunt.co/
    .palettes <- list(
        'OrBlGrRdBrPr' = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown", 
                           "purple3"),
        'Dark IV' = c('#11144c', '#3a9679', '#fabc60', '#e16262'),
        'Pastel IV' = c('#7fe7cc', '#dfe38e', '#efca8c', '#f17e7e'),
        'Twilight IV' = c('#e7eaf6', '#a2a8d3', '#38598b', '#113f67'),
        'Blues then Orange IV' = c('#070d59', '#1f3c88', '#5893d4', '#f7b633'),
        'Bright IV' = c('#ff62a5', '#ffe5ae', '#6b76ff', '#dee0d9')
    )
    
    if(missing(pname)) {
        if(get_palette_names)
            return (names(.palettes))
        
        return (.palettes)
    }
    
    pal <- .palettes[[pname]]
    if(is.null(pal)) {
        warning("Invalid palette requested: ", pname, ". Returning random palette")
        pal <- .palettes[[sample(seq_along(.palettes), 1)]]
    }
    
    return (pal)
}

get_heatmap_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
    # Some of these are from:
    # https://colorhunt.co/
    rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')
    rave_color_ramp_dark_palette <- colorRampPalette(c('#13547a', 'black', '#ff758c'), interpolate='linear', space='Lab')
    
    
    ..dark_blue_to_red <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff", 
                                "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))
    ..light_blue_to_light_red <- c(..dark_blue_to_red[5:1], 'black', ..dark_blue_to_red[11:7])
    
    
    rave_color_ramp_palette <- colorRampPalette(..dark_blue_to_red, interpolate='linear', space='Lab')
    rave_heat_map_colors <- rave_color_ramp_palette(1001)
    
    rave_color_ramp_dark_palette <- colorRampPalette(..light_blue_to_light_red, interpolate='linear', space='Lab')
    
    rave_heat_map_dark_colors <- rave_color_ramp_dark_palette(1001)
    
    # put this here for legacy, but we need to exterminate these references
    crp <- rave_heat_map_colors
    
    if(missing(pname)) {
        if(get_palette_names)
            return (names(.palettes))
        
        return (.palettes)
    }
    
    pal <- .palettes[[pname]]
    if(is.null(pal)) {
        warning("Invalid palette requested: ", pname, ". Returning random palette")
        pal <- .palettes[[sample(seq_along(.palettes), 1)]]
    }
    
    return (pal)
}






set_palette <- function(pname) {
    if(length(pname) == 1) {
        pname %<>% get_palette
    }
    
    grDevices::palette(pname)
}



