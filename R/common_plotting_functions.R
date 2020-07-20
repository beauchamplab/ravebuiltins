
#' @author John Magnotti
#' @title Draw several heatmaps in a row and (optionally) a color bar
#' @param hmaps data to draw heatmaps
#' @param percentile_range whether to draw in percentile
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
#' @param PANEL.COLOR_BAR a function to adjust colorbar width
#' @param axes vector of logicals, whether to draw axis
#' @param plot_time_range x range, similar to \code{xlim}
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar.
#' By default it is setup for time/freq, but by swapping labels and decorators you can do anything.
#' @seealso layout_heat_maps
#' @seealso draw_img
draw_many_heat_maps <- function(hmaps, max_zlim=0, percentile_range=FALSE, log_scale=FALSE,
                                show_color_bar=TRUE, useRaster=TRUE, wide=FALSE,
                                PANEL.FIRST=NULL, PANEL.LAST=NULL, PANEL.COLOR_BAR=NULL, axes=c(TRUE, TRUE), plot_time_range=NULL, ...) {
    k <- sum(hmaps %>% get_list_elements('has_trials'))
    orig.pars <- layout_heat_maps(k)
    # I'm getting error about pin here... let's just rely on the graphics device being reset?
    # on.exit({
        # par(orig.pars)
    # })
    
    if(is.na(max_zlim)) {
        max_zlim = 0
    }
    
    # this is to add some extra spacing on the LEFT margin to allow, e.g., longer axis titles
    # we could also set this adaptively based on the max(nchar(...)) for the appropriate labels from hmap[[ii]] condition names
    if(wide) {
        #NB: B, L, T, R
        # trying to be smart about the size of the margin to accomodate the angular text. R doesn't auto adjust :(
        max_char_count = max(sapply(hmaps, function(h) ifelse(h$has_trials, max(nchar(h$conditions)), 1)))
        
        par(mar = c(par('mar')[1],
                    5.1 + max(0,(max_char_count - 5)*0.95),
                    2, 2))
    }

    # actual data range, as opposed to the max zlim which controls the plottable range
    actual_lim = get_data_range(hmaps)

    if(max_zlim <= 0) {
        max_zlim <- max(abs(actual_lim), na.rm=TRUE)
    } else if(percentile_range) {
        if(max_zlim >= 100) {
            max_zlim = (max_zlim / 100)*max(abs(actual_lim), na.rm=TRUE)
        } else {
            max_zlim <- quantile(unlist(lapply(hmaps, getElement, 'data')),
                                 probs = max_zlim / 100,
                                 na.rm = TRUE)
        }
    }

    log_scale <- if(isTRUE(log_scale)) {
        'y'
    } else {
        ''
    }
    
    lapply(hmaps, function(map){
        # map = hmaps[[1]]
        if(map$has_trials){
            # check the plottable range, to make sure we're only plotting what the user has requested
            plot_time_range %?<-% range(map$x)

            if (! all(map$x %within% plot_time_range) ) {
                # print('fixing x range')
                .attr = attributes(map$data)
                ind <- map$x %within% plot_time_range
                map$x <- map$x[ind]
                
                ## This is dropping attributes :(
                map$data <- map$data[ind,,drop=FALSE]

                # update to the new dim                
                .attr$dim = attr(map$data, 'dim')
                
                attributes(map$data) <- .attr                
            }
            
            # we are linearizing the x and y spaces so that we can use the fast raster... is this worth it?
            x <- seq_along(map$x)
            y <- seq_along(map$y)
            
            # because image plot centers the data on the y-variable, it can introduce 0s which fail when log='y'
            # so we shift y by a small amount so that the minimum is > 0
            dy <- 0
            if(log_scale == 'y') {
                dy <- (y[2]-y[1])/2 + min(y)
                #FIXME I think this may be making the edge boxes too small cf. the else block where we pad 0.5
                rutabaga::plot_clean(x,y+dy, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis*get_cex_for_multifigure(), log='y')
            } else {
                pad = c(-0.5, 0.5)
                rutabaga::plot_clean(xlim=range(x) + pad,
                           ylim=range(y) + pad)
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
        .mar <- c(par('mar')[1], 3.5, 2, 1)
        if(is.function(PANEL.COLOR_BAR)) {
            .mar[3] = 4
        }
        
        # par(mar=c(5.1, 4.5, 4, 2),
        #     mai = c(0.6732, 0.5412, 1, 0.2772))
        
        .ylab = ''
        ii = 1
        while(ii <= length(hmaps)) {
            if(hmaps[[ii]]$has_trials) {
                .ylab <- attr(hmaps[[ii]]$data, 'zlab')
                ii <- 1e10
            }
            ii = ii + 1
        }
        
        rave_color_bar(max_zlim, actual_lim, ylab=.ylab, mar=.mar)
        
        if(is.function(PANEL.COLOR_BAR)) {
            PANEL.COLOR_BAR(hmaps)
        }
    }
    
    invisible(hmaps)
}

build_group_names <- function(groups) {
    gnames = sapply(groups, `[[`, 'group_name')
    gnames[gnames == ""] = paste0('rave_group_', LETTERS[which(gnames=='')])
    return(gnames)
}

build_group_contrast_labels <- function(group_names) {
    apply(utils::combn(length(group_names),2), 2, 
          function(x) paste(group_names[x],collapse='.vs.'))
}

# show power over time with MSE by condition
time_series_plot <- function(plot_data, PANEL.FIRST=NULL, PANEL.LAST=NULL, plot_time_range=NULL,
                             do_update_ylim=TRUE, axes=TRUE) {
    # check the plottable range, to make sure we're only plotting what the user has requested
    plot_time_range %?<-% get_data_range(plot_data, 'x')
    
    for(ii in seq_along(plot_data)) {
        if (! all(plot_data[[ii]]$x %within% plot_time_range) ) {
            attrs = attributes(plot_data[[ii]]$data)
            
            ind <- plot_data[[ii]]$x %within% plot_time_range
            plot_data[[ii]]$x <- plot_data[[ii]]$x[ind]
            plot_data[[ii]]$data <- plot_data[[ii]]$data[ind,,drop=FALSE]
            
            attrs$dim = attributes(plot_data[[ii]]$data)$dim
            attributes(plot_data[[ii]]$data) = attrs
            
            # we need to update the range of the plots
            if(do_update_ylim) {
                plot_data[[ii]]$range <- .fast_range(plus_minus(plot_data[[ii]]$data))
            }
        }
    }
    
    xlim <- pretty(plot_time_range)#get_list_elements(plot_data, 'x') %>% unlist)
    ylim <- pretty(get_data_range(plot_data) %>% unlist, min.n=2, n=4)
    
    # dipsaus::cat2('MAR: ', paste0(par('mar'),collapse = ' '), level = 'INFO')
    rutabaga::plot_clean(xlim, ylim)
    
    if(isTRUE(is.function(PANEL.FIRST))) PANEL.FIRST(plot_data)
    
    # draw the axes AFTER the first paneling, should this go in PANEL.FIRST?
    # it's weird because the PANEL.FIRST is responsible for labeling the axes, so why not for drawing them?
    # the counter is that we need the xlim and ylim to make the plot. So it's easy to just draw the labels here
    axes %<>% rep_len(2)
    if(axes[1]) rave_axis(1, xlim)
    if(axes[2]) rave_axis(2, ylim)
    
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
trial_scatter_plot = function(group_data, ylim, bar.cols=NA, bar.borders=NA, cols, ebar.cols='gray30', ebar.lwd=3, jitr_x,
                              pchs=16, pt.alpha=175, xlab='Group', ylab='Mean % Signal Change', ebar.lend=2, show_outliers=TRUE, PANEL.LAST=NULL, ...) {
    
    nms <- group_data %>% get_list_elements('name')
    
    # #yes, sometimes people use the same name for different groups, or don't give names. Let's create new names
    gnames <- paste0(LETTERS[seq_along(nms)], nms)
    
    #
    ns <- group_data %>% get_list_elements('N')
    
    if(show_outliers) {
        yax <- do_if(missing(ylim), {
            pretty(get_data_range(group_data), high.u.bias = 100, n=4, min.n=3)
        }, ylim)
    } else {
        yax <- do_if(missing(ylim), {
            pretty(sapply(group_data, function(.d) {
                range(.d %$% data[is_clean])
                }), high.u.bias = 100, n=4, min.n=3)
        }, ylim)
    }
    
    #there are edge cases where length(mses) != length(names), take care of this with `ind` below
    bp_names <- paste0(nms, ' (N=' %&% ns %&%')')
    
    # this creates space for empty groups -- is this expected behavior? It is good to preserve the color
    # mapping, but I'd rather not have the empty space... so we need to preserve the colors but not the empty space
    ind <- which(unlist(lapply(group_data, '[[', 'has_trials')))
    mses <- sapply(ind, function(ii) group_data[[ii]]$mse)
    
    # 
    axis_col = get_foreground_color()
    
    # x <- rave_barplot(mses[1,],col='white', ylim=.fast_range(yax), axes=F)
    .fg <- par('fg'=axis_col, 'col.lab' = axis_col, col.axis=axis_col)
    x <- rave_barplot(mses[1,],
                      ylim=.fast_range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
                      names.arg=bp_names[ind], axes=FALSE, ...)
    par('fg'=.fg)
    
    axis_label_decorator(group_data, label_alignment=FALSE)
    
    rave_axis(2, at=yax)
    
    if(min(yax) < 0) abline(h=0, col='lightgray')

    # grabbing an attribute from the group data
    if(not_null(attr(group_data, 'stats'))) {
        # rave_title(as.title(pretty(
            # legend('topleft', bty='n', legend=attr(group_data, 'stats'))
            # )))
    } else {
        # 
    }
    
    #emphasize the means
    lsize <- (1/3)*mean(unique(diff(x)))
    
    # this means there is only 1 group. If there is one group, the barplot seems to get placed at 0.7, with
    # the usr range being 0.16 - 1.24.
    if(is.na(lsize)) lsize <- 1/3
    
    # jittering is done in main.R now so that it can persist across refreshes
    # if(missing(jitr_x)) jitr_x <- 0.75*lsize
    
    # if(missing(cols)) cols <- get_color(seq_along(group_data))
    if(missing(cols)) cols <- grDevices::palette()
    
    # Ensure all parameters are sufficiently long. This feels extravagant, but needed because we're indexing into these variables
    # and we don't want to reach beyond the end
    par_rep <- function(y) rep_len(y, length(group_data))
    cols %<>% par_rep
    pchs %<>% par_rep
    ebar.cols %<>% par_rep
    # bar.cols %<>% par_rep
    # bar.borders %<>% par_rep
    
    # x may not be the same length as group_data because we're skipping empty groups
    # we still want everything else to be based on group number
    xi <- 1
    for(ii in seq_along(group_data)) {
        if(group_data[[ii]]$has_trials) {
            
            lines(group_data[[ii]]$xp + c(-lsize, lsize), rep(mses[1, xi], 2),
                  lwd=ebar.lwd, lend=ebar.lend, col=ebar.cols[ii])
            
            # add_points(x[xi], group_data[[ii]]$data,
            #            col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)
            
            
            if(show_outliers) {
                group_data[[ii]] %$% {
                    points(x, data,
                           col=ifelse(is_clean, getAlphaRGB(cols[ii], pt.alpha), 'gray30'),
                           pch=ifelse(is_clean, pchs[ii], 1))
                }
            } else {
                group_data[[ii]] %$% {
                    points(x[is_clean], data[is_clean],
                           col=getAlphaRGB(cols[ii], pt.alpha),
                           pch=pchs[ii])
                }
                
                
            }
            
            # cat('How many clean datapoints?' %&% sum(group_data[[ii]]$is_clean) %&% '\n')
            ebars.y(x[xi], mses[1,xi], mses[2,xi],
                    lwd=ebar.lwd, col=ebar.cols[ii], code=0, lend=ebar.lend)
            
            xi <- xi+1
        }
    }
    
    if(is.function(PANEL.LAST)) {
        PANEL.LAST(group_data)
    }
    
    # in case people need to further decorate
    invisible(group_data)
}

trial_scatter_plot_decortator <- function(plot_data, plot_title_options, ...) {
    tspd <- function(plot_data, ...) {
        title_decorator(plot_data, plot_title_options=plot_title_options,
                        allow_cond = FALSE, allow_sample_size = FALSE)
    }
    
    if(missing(plot_data)) {
        return (tspd)
    }
    
    tspd(plot_data)
}



draw.box <- function(x0,y0,x1,y1, ...) {
    segments(x0, y0, x1=x1, ...)
    segments(x1, y0, y1=y1, ...)
    segments(x1, y1, x1=x0, ...)
    segments(x0, y1, y1=y0, ...)
}


# the Xmap and Ymap here are functions that allow for transformation of the plot_data $x and $y into
# the coordinate system of the plot
spectrogram_heatmap_decorator <- function(plot_data, plot_options, Xmap=force, Ymap=force, btype='line', atype='box', 
                                          title_options=list(allow_freq=FALSE), ...) {
    
    shd <- function(plot_data, Xmap=Xmap, Ymap=Ymap) {
        .args = list('plot_data' = plot_data, 'plot_title_options' = plot_options$plot_title_options)
        
        if(length(title_options) > 0) {
            .args[names(title_options)] = title_options
        }
        do.call(title_decorator, args=.args)
        
        axis_label_decorator(plot_data, Xmap = Xmap, Ymap = Ymap)
        
        # check if the analysis and baseline windows are fully contained within the plotting window. If not, switch baseline type 
        # to just be a label. This can, of course, be turned off using the regular options.
        if(!all(plot_data$baseline_window %within% plot_data$x)) {
            btype = 'label'
        }
        
        if(!all(plot_data$analysis_window %within% plot_data$x)) {
            atype = 'label'
        }
        
        # would be nice to have a TRIAL_ONSET or something here, rather than a string...
        if(plot_data$trial_alignment != 'Trial Onset') {
            btype = 'n'
        }
        
        windows <- list(
            'Baseline'=list(
                window = if(btype == 'label') {
                    plot_data$baseline_window    
                }else {
                    Xmap(plot_data$baseline_window)
                },
                type=btype
            ),
            'Analysis'=list(
                window = if(atype=='box') {
                    list(x=Xmap(plot_data$analysis_window),
                         y=Ymap(plot_data$frequency_window))
                } else if(atype == 'label') {
                    plot_data$analysis_window    
                }else {
                    Xmap(plot_data$analysis_window)
                },
                type=atype
            )
        )

        lapply(names(windows), function(nm) {
            if(paste(nm, 'Window') %in% plot_options$plot_title_options & windows[[nm]]$type != 'n') {
                with(windows[[nm]],
                     window_decorator(
                         window=window, type=type,
                         text=ifelse(plot_options$draw_decorator_labels, nm, ''),
                         label_placement_offset = ifelse(all('label' == c(btype,atype), nm == 'Analysis'), 0.8, 0.9)
                     )
                )
            }
        })
        # if(results$get_value('draw_decorator_labels')) {
        #     rave_axis(1, at=Xmap(0), labels = plot_data$trial_alignment, mgpx=c(1,2,1), lwd=0, tcl=0)
        # }
        
        invisible(plot_data)
    }
    
    if(missing(plot_data)) {
        return (shd)
    }
    
    shd(plot_data, Xmap, Ymap)
}

# here we just call the spectrogram decorator with some special setup options
by_trial_heat_map_decorator <- function(plot_data=NULL, plot_options, Xmap=force, Ymap=force, ...) {
    args <- list(
        plot_options=plot_options, Xmap=Xmap, Ymap=Ymap, atype='line', btype='line',
        title_options = list(allow_sample_size=FALSE),
        ...
    )
    
    
    # this is not great to be hard-coding Trial Number and Condition here...
    if(!(plot_options$sort_trials_by_type %in% c('Trial Number', 'Condition'))) {
        args$atype = 'n'
    }
    
    if(is.null(plot_data)) {
        return(do.call(spectrogram_heatmap_decorator, args = args))
    }
    
    args$plot_data=plot_data
    do.call(spectrogram_heatmap_decorator, args = args)
}


by_electrode_heat_map_decorator <- function(plot_data=NULL, plot_options, Xmap=force, Ymap=force, ...) {
    args <- list(
        plot_options=plot_options, Xmap=Xmap, Ymap=Ymap, atype='line', btype='line',
        title_options = list(allow_enum=FALSE),
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
#' @param mat z-matrix
#' @param x,y z and y axis
#' @param col vector of colors, color palette
#' @param zlim value to trim zmat
#' @param log which axis will be in log scale
#' @param useRaster passed to image()
#' @param clip_to_zlim whether to clip mat
#' @param add logical, whether to overlay current plot to an existing image
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators.
#' We are just plotting image(mat) Rather Than t(mat) as you might expect. The Rave_calculators know this so we can save a few transposes along the way.
make_image <- function(mat, x, y, zlim, col, log='', useRaster=TRUE, clip_to_zlim=TRUE, add=TRUE) {
    rave_context()
    
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
    
    col %?<-% get_currently_active_heatmap()
    
    if(!('matrix' %in% class(mat))) {
        warning('mat is not a matrix... check it out: make_image_mat')
        # assign('make_image_mat', mat, globalenv())
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
    
    cbar_wid = 3.5
    if('pdf' %in% names(dev.cur())) {
        cbar_wid = 3
        # cat2t('setting skinny cbar')
    }
    
    layout(matrix(1:(k+1), nrow=1), widths=c(rep(ratio, k), lcm(cbar_wid)) )
    par(mar=c(par('mar')[1], par('mar')[2], 2, 2))
    invisible(opars)
}

##RUTABAGA
median_ticks <- function(k, .floor=1) c(.floor, ceiling(k/2), k)

`conditional_sep<-` <- function(str, value = '') {
    sep=' '
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

str_rng <- function(rng) {
    sprintf('[%s]', paste0(rng, collapse=':'))
}


rave_color_bar <- function(zlim, actual_lim, clrs, ylab='Mean % Signal Change', ylab.line=1.5,
                           mar=c(5.1, 5.1, 2, 2), ...) {
    rave_context()

    clrs %?<-% get_currently_active_heatmap()
    cbar <- matrix(seq(-zlim, zlim, length=length(clrs))) %>% t
    par(mar=mar)
    image(cbar,
          col=clrs, axes=F, ylab='', main='',
          col.lab = get_foreground_color())

    
    # check if any other graphics params were requested, direct them to the proper function
    more = list(...)
    
    ral.args = list(ylab=ylab, line=ylab.line)
    if('cex.lab' %in% more) ral.args$cex.lab = more$cex.lab
    
    do.call(rave_axis_labels, ral.args)
    
    ra.args = list(side=2, at=0:1, labels=pretty_round(c(-zlim,zlim)), tcl=0)
    if('cex.axis' %in% more) ra.args$cex.axis = more$cex.axis
    
    do.call(rave_axis, ra.args)

    ra.args[c('at', 'labels', 'tcl')] = list(0.5, 0, 0.3)
    do.call(rave_axis, ra.args)
    
    box()

    invisible(zlim)
}

##RUTABAGA
midpoint <- function(x) {
    sapply(seq_along(x)[-1], function(ii) {
        (x[ii] - x[ii-1])/2 + x[ii-1]
    })
}

# this is really only used by the by_trial heat map,
# but that gets used in multiple modules, so it's here....
reorder_trials_by_type <- function(bthmd) {
    
    stop('No longer maintained, use: reorder_trials_by_event')
    
    # we want to sort but preserve the order that the conditions were added to the group
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
    bthmd$Trial_num <-  bthmd$Trial_num[unlist(ind)]
    bthmd$is_clean <-  bthmd$is_clean[unlist(ind)]

    return(bthmd)
}

reorder_trials_by_event <- function(bthmd, event_name) {
    
    
    if(event_name != 'Condition') {
        new_order = order(bthmd$events[[event_name]])
    } else {
        # We want to sort, but if we're using Condition, we want to preserve the order that the conditions were added to the group.
        # What if we factor the data, then sort by the 1e7*factor_number + trial_number
        as_fact = factor(bthmd$events[['Condition']], levels = bthmd$conditions)
        new_order = order(1e7*as.integer(as_fact) + bthmd$Trial_num)
        
        # if we're sorting by condition (or any categorical variable) 
        # we can show labels
        ind <- sapply(bthmd$conditions, function(ttype) sum(ttype==bthmd$trials), simplify = FALSE)
        bthmd$lines <- cumsum(ind)
        bthmd$ttypes <- names(ind)
    }
    
    .xlab <- attr(bthmd$data, 'xlab')
    .zlab <- attr(bthmd$data, 'zlab')
    
    # note that data is the transpose of what might be expected...
    # this saves a transpose during plotting and doesn't cause issues if you're careful
    bthmd$data <- bthmd$data[, new_order]
    bthmd$events <- bthmd$events[new_order, ]
    
    # set the axis labels
    attr(bthmd$data, 'xlab') <- .xlab
    if(event_name != 'Condition') {
        attr(bthmd$data, 'ylab') <- 'Trial # (sorted by ' %&% event_name %&% ')'
    }
    attr(bthmd$data, 'zlab') <- .zlab
    
    
    bthmd$trials <-  bthmd$trials[new_order]
    bthmd$Trial_num <-  bthmd$Trial_num[new_order]
    bthmd$is_clean <-  bthmd$is_clean[new_order]
    
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
        # rather than drawing the labels AT the lines, we should draw them intermediate
        rave_axis(2, tcl=0, lwd=0, at=yat+0.5, labels=map$ttypes)
    })

    invisible(map)
}

by_trial_analysis_window_decorator <- function(map, event_name, show_label=TRUE, Xmap=force, Ymap=force, font=2, show_0=TRUE, ...) {
    force(event_name)
    
    btawd <- function(map, Xmap, Ymap, ...) {
        points(Xmap(map$events[[event_name]]), y = Ymap(seq_len(ncol(map$data))),
               type='p', bg=adjustcolor('black', 1), pch=22, cex=0.25)
        
        
        if(show_label) {
            text(Xmap(quantile(map$events[[event_name]], 0.75)),
                 y=Ymap(0.75*ncol(map$data)), label = event_name, cex=rave_cex.lab, pos=2,
                 font=font)
        }
        
        if(show_0) {
            abline(v=Xmap(0), lty=2, col=rave_colors$TRIAL_TYPE_SEPARATOR)
            
        }
        
    }
    if(missing(map)) {
        return(btawd)        
    }
    
    btawd(map, Xmap, Ymap, ...)
}

# here we're overriding the rutabaga do_poly because we can't update rutabaga without getting dipsaus...
do_poly <- function(x, y, col, alpha=50/255, border=NA, ...) {
    if(alpha>1) alpha/255
    
    polygon(c(x,rev(x)), rep(y, each=2),
            col=adjustcolor(col, alpha), border=border, ...)
}

heatmap_outlier_highlighter_decorator <- function(map, Xmap=force, Ymap=force, ...) {
    with(map, {
        sapply(which(!is_clean), function(ti) {
            # because the data may be sorted by trial type
            # we need to make sure ti (trial index) is actually where it should be
            # this should handle non-squentiall trials
            # ti = which(map$Trial_num == ti)
            
            ## We are now sorting the is_clean vector as well, so it should line up now without having
            # to do this extra check
            
            do_poly(Xmap(range(x)), y=ti %+-% 0.5, border=NA, lty=1, alpha=0.3,
                    col=rave_colors$TRIAL_TYPE_SEPARATOR, lwd=2)
            # 
            # do_poly(Xmap(range(x)), y=ti %+-% 0.5, border='yellow', lty=2, alpha=0.0,
            #         col=rave_colors$TRIAL_TYPE_SEPARATOR, lwd=2)
                
            mtext(text = bquote('' %=>% "" ), at = ti, side=2, line=-2, cex=2, las=1, col='goldenrod2', adj = c(0,0))
            mtext(text = bquote("" %<=% ''), at = ti, side=4, line=-4, cex=2, las=1, col='goldenrod2')
        })
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
axis_label_decorator <- function(plot_data, col, Xmap=force, Ymap=force, label_alignment=TRUE, label_alignment.line = 2, ...) {
    # here we are assuming that everything in plot_data 
    # is of the same x/y type
    
    # we  need to check if we've been give a list of things to plot (as is common for line plots),
    # or a single thing (as is common for heatmaps)
    # test if there are plot variables at the highest level, if so, then we are in the latter condition
    pd <- plot_data
    if(is.null(pd[['has_trials']])) {
        ii = which(get_list_elements(pd, 'has_trials'))[1]
        pd <- pd[[ii]]
    } 
    
    # if(!is.null(pd$trial_alignment) && label_alignment) {
    #     rave_axis(1, at=Xmap(0), labels = pd$trial_alignment,
    #               mgpx=c(1,label_alignment.line,1), lwd=0, tcl=0, ...)
    # }
    # 
    rave_axis_labels(xlab=attr(pd$data, 'xlab'), ylab=attr(pd$data, 'ylab'), ...)
}

round_to_nearest <- function(x, val=10) {
    val*round(x/val)
}

get_foreground_color <- function() {
    switch(par('bg'),
           'white' = 'black',
           'black' = 'white',
           '#1E1E1E' = 'gray70',
           'gray' = '#A5A5A5', 
           'black'
    ) 
}

invert_palette <- function(pal) {
    inv = c(255, 255, 255, 255) - col2rgb(pal, alpha=TRUE)
    rgb(t(inv), alpha=255, maxColorValue = 255)    
}

# build a results-object like list
build_results_object <- function(l) {
    list(
        get_value = function(nm, ifNotFound=NULL) {
            if(nm %in% names(l)) {
                return (l[[nm]])
            }
            
            ifNotFound
        }
    )
}

#works by side effect to change the palette used by the current graphics device
# and set the RAVE theme to light or dark
set_palette_helper <- function(results, plot_options, ...) {
    rave_context()
    
    results %?<-% build_results_object(plot_options)
    
    .bg <- results$get_value('background_plot_color_hint', 'white')
    # session = shiny::getDefaultReactiveDomain()
    if(tolower(.bg) %in%  c('white')) {
        theme = set_rave_theme('light')
    }else{
        theme = set_rave_theme('dark')
    }
    
    # setting the background color here triggers a cascade of color changes
    if(tolower(.bg) == 'gray') {
        par('bg'=rave_colors$DARK_GRAY)
    } else {
        par('bg'=.bg)
    }
    
    pal <- get_palette(results$get_value('color_palette'))
    
    if(results$get_value('invert_colors_in_palette', FALSE)) {
        pal %<>% invert_palette
    }
    
    if(results$get_value('reverse_colors_in_palette', FALSE)) {
        pal %<>% rev
    }
    
    set_palette(pal)
    
    par(col=get_foreground_color())
    
    invisible()
}


shiny_is_running <- function() {
    return(shiny::isRunning())
    
    # cls <- class(getDefaultReactiveDomain())
    # any(cls %in% c('ShinySession', 'session_proxy'))
}


# by default we use plot_title_options variable in results to see what to put in the title string
# callers can override this behavior by specifically dis-allowing certain options
# currently you can't force something to be TRUE if a user doesn't allow it, but we can think about 
# this. If that's the case, all the allow_* would be NULL by default, and setting them to TRUE would override 
# user preference. This seems rude at best, but for certain plots maybe they really require something to 
# be put in the title?
title_decorator <- function(plot_data, plot_title_options,
                            allow_sid=TRUE, allow_enum=TRUE, allow_freq=TRUE,
                            allow_cond=TRUE, allow_sample_size=TRUE, ...) {
    title_string = ''
    
    # if(missing(plot_title_options)) {
    #     plot_title_options = build_plot_options()$plot_title_options
    # }
    
    # if we have multiple data, just take the first
    # the way to guess this is to check for the existence of a variable that we should have...
    if(is.null(plot_data[['name']])) {
        plot_data = plot_data[[1]]
    }
    
        # wraps do_on_inclusion to make ths following lines easier to understand
    add_if_selected <- function(id, expr) {
        do_on_inclusion(id, expr, plot_title_options)
    }
    
    if(allow_cond)
        add_if_selected('Condition', {
            .name <- plot_data[['name']]
            if(isTRUE(nchar(.name) > 0)) {
                .name <- '' %&% .name
            }
            title_string = .name
        })
    
    # we could write this as a simple m/sapply if the variable names had a clear relationship to one another
    if(allow_sid)
        add_if_selected('Subject ID', {
            conditional_sep(title_string) = plot_data$subject_code
        })
    
    if(allow_enum)
        add_if_selected('Electrode #', {
                el <- dipsaus::deparse_svec(plot_data$electrodes, max_lag=1)
            # print('EL: ' %&% el)
            conditional_sep(title_string) = 'E' %&% el
        })
    
    if(allow_freq)
        add_if_selected('Frequency Range', {
            conditional_sep(title_string) = 'Freq ' %&% paste0(plot_data$frequency_window, collapse=':')
        })
    
    if(allow_sample_size) 
        add_if_selected('Sample Size', {
            if(!is.null(plot_data$N))
                conditional_sep(title_string) = 'N=' %&% plot_data$N
        })
    
    # rave_title is an "additive" instead of replacement call, so rendering an empty string won't hurt anything,
    # but let's save a few needless function calls
    if(nchar(title_string) > 0) {
        rave_title(title_string)
    }
    
    invisible()
}

set_font_scaling <- function(plot_options, FONT_SCALING = c('shiny', 'Rutabaga', 'R')) {
    FONT_SCALING = match.arg(FONT_SCALING)
    
    font_opts = switch(FONT_SCALING,
        # here mostly making things bigger
        'shiny' = list(
            cex.main = 1.5,
            cex.axis = 1.3,
            # putting this to 1.4 because 1.5 causes some clipping of the axis(2) label,
            # we could also try to increase the left margin to compensate
            cex.lab = 1.4,
            # ticks are still outward, but shorter than normal
            tcl = -0.3
        ),    
        list(
            cex.main = 1.2,
            cex.axis = 1,
            cex.lab = 1,
            tcl = -0.5
        )
    )
    
    if (FONT_SCALING == 'Rutabaga') {
        # same as R, but shorter ticks are a must!
        font_opts$tcl = -0.3
    }
    
    plot_options[names(font_opts)] = font_opts
 
    return(plot_options)
}

build_plot_options <- function(..., FONT_SCALING=c('shiny', 'Rutabaga', 'R')) {
    # this works
    options <- fastmap::fastmap()
    
    #this does not work
    # options <- dipsaus::fastmap2()
    # options$mset(a=2, b=3)
    
    options$mset(
        plot_time_range = c(-Inf,Inf),
        draw_decorator_labels = FALSE,
        plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 
                               'Sample Size', 'Baseline Window', 'Analysis Window'),
        
        background_plot_color_hint = 'white',
        
        color_palette = 'Beautiful Field',
        invert_colors_in_palette = FALSE,
        reverse_colors_in_palette = FALSE,
        
        heatmap_color_palette = get_heatmap_palette(get_palette_names = TRUE)[1],
        heatmap_number_color_values = 101,
        invert_colors_in_heatmap_palette = FALSE,
        reverse_colors_in_heatmap_palette = FALSE,
        
        show_outliers_on_plots = TRUE,
        
        log_scale = FALSE,
        max_zlim = 0,
        percentile_range = TRUE,
        sort_trials_by_type = 'Trial Number'
    )
    
    
    options %<>% set_font_scaling(FONT_SCALING)
    
    
    # any named arguments will override the defaults
    v = list(...)
    # options[names(v)] = v
    if(length(v))    options$mset(.list=v)

    return(options)
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
time_series_decorator <- function(plot_data, plot_options, ...) {
    .plot_options <- plot_options$plot_title_options 
    ddl = plot_options$draw_decorator_labels
    
    do_tsd <- function(plot_data, label_hint=label_hint) {
        # plot title
        title_decorator(plot_data, plot_title_options = .plot_options,
                        allow_sample_size=FALSE, allow_cond = FALSE)
        
        # axis labels
        axis_label_decorator(plot_data)
        
        windows = c('Analysis')
        if(plot_data[[1]]$trial_alignment == 'Trial Onset') {
            windows %<>% c("Baseline")
        }
        
        sapply(windows, function(nm) {
            if(paste(nm, 'Window') %in% .plot_options ) {
                full_name <- tolower(nm) %&% '_window'
                if(!ddl) {
                    nm <- FALSE
                }
                window_decorator(plot_data[[1]][[full_name]],
                                 type='shaded', shade.col = rave_colors[[full_name]], text = nm,
                                 label_placement_offset = ifelse(
                                     nm == 'Baseline', 0.9, 0.8)
                )
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
# I guess there could be an option to include N==0 labels...
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
    
    .cex = rave_cex.lab*get_cex_for_multifigure()
    if(plotting_to_file()) {
        .cex = 1*get_cex_for_multifigure()
    }

    legend(location, legend=legend_text, ncol=ceiling(length(ii)/3),
           inset=c(.025,.075), bty='n',
           text.col=ii, cex=.cex)
    
    invisible()
}


pretty_round <- function(x) {
    max_x <- max(abs(x))
    dig = 0
    if(max_x < 1) {
        dig = abs(floor(log10(max_x)))
    } 
    round(x, dig)
}

color_bar_title_decorator <- function(m, cex = rave_cex.lab * 0.8) {
    rave_title(paste0('Range\n[', 
                      paste0(pretty_round(get_data_range(m)), collapse = ':'),
                      ']'),
               font = 1,
               cex = cex)
}



format_unit_of_analysis_name <- function(unit_of_analysis) {
    str_replace_all(unit_of_analysis, c(' '='_', '%'='Pct', '-'='_'))
}

# the color_variable will be recycled to length of strings to provide a (possibly the same) color for each string
add_strings_to_plot_title <- function(strings, color_variable, width_factor=1.5, ...) {
    if(!("character" %in% class(strings))) {
        # warn('Casting strings to character to get character count')
        strings %<>% as.character
    }
    
    nchars = cumsum(round(width_factor*nchar(strings)))
    nchars = nchars - nchars[1]
    
    color_variable %<>% rep(length.out = length(strings))
    
    mapply(function(nm, spacer, col, tots = max(nchars)) {
        .main = paste0(rep(' ', spacer), collapse='') %&% nm %&% paste0(rep(" ", tots-spacer), collapse='')
        title(main = .main, font=3, family='mono',
              col.main = col, cex.main = get_cex_for_multifigure()*rave_cex.main, ...)
        
    }, strings, nchars, color_variable)
}



get_unit_of_analysis <- function(requested_unit, names=FALSE) {
    ll = list(
        '% Change Power' = 'percentage',
        '% Change Amplitude' = 'sqrt_percentage',
        'z-score Power' = 'zscore',
        'z-score Amplitude' = 'sqrt_zscore',
        'decibel' = 'decibel'
    )
    
    if(missing(requested_unit)) {
        if(names) return (names(ll))
        
        return (ll)
    }
    
    if(!any(requested_unit == names(ll))) {
        warning("requested unit of analysis not available: ", requested_unit, '. Returning % Change Power')
        return(ll[['% Change Power']])
    }
    
    return(ll[[requested_unit]])
}



window_decorator <- function(window, type=c('line', 'box', 'shaded', 'label'),
                             line.col, shade.col='gray60', label_placement_offset=0.9,
                             text=FALSE, text.col, lwd, lty) {
    type <- match.arg(type)
    text.x = unlist(window)[1]
    text.y <- par('usr')[4] * label_placement_offset
    
    line.col %?<-% get_foreground_color()
    
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
           label = {
               text.col %?<-% shade.col
               if(is.character(text) && nchar(text) > 1) {
                   text = paste0(text, ':')
               } else {
                   text = ''
               }
               text = bquote(.(text) ~ .(window[1]) %->% .(window[2]))
           },
           shaded = {
               text.col %?<-% shade.col
               text.x = window[[1]]
               x = window
               y = par('usr')[3:4]

               amt = diff(par('usr')[3:4])*.10
               text.y = par('usr')[4] - amt

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
        cex = rave_cex.lab*get_cex_for_multifigure()
        if(plotting_to_file()) {
            cex = 1
        }
        text(text.x, text.y, labels = text, pos=4, col=text.col,
             cex=cex)
    }
}

#
# x = sort(rnorm(10))
# ..get_nearest(pretty(x), x)
# move to RUTABAGA
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
            text(median(BASELINE), quantile(y, .7), 'baseline', col=label.col, cex=rave_cex.lab*get_cex_for_multifigure(), pos=3)
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
                        l_margin){
#TODO RUTABAGA
    if(missing( l_margin )){
        l_margin = local({
            mar = s_margin;
            mar[legend_side] = 0;
            mar[(legend_side + 2) %% 4] = 0.5;
            mar
        })
    }

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

#' Function to get builtin color palettes
#' @param pname palette name
#' @param get_palette_names whether to get palette names
#' @param get_palettes ignored
#' @export
get_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
    # from:
    # http://colorbrewer2.org/
    .palettes <- list(
        'Beautiful Field' = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown",  "purple3"),
        'Accent' = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666'),
        'Dark2' = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666'),
        'Paired' = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
        'Pastel1' = c('#fbb4ae','#b3cde3','#ccebc5','#decbe4','#fed9a6','#ffffcc','#e5d8bd','#fddaec'),
        'Pastel2' = c('#b3e2cd','#fdcdac','#cbd5e8','#f4cae4','#e6f5c9','#fff2ae','#f1e2cc','#cccccc'),
        'Set1' = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'),
        'Set2' = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3'),
        'Set3' = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')
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

cache_heatmap_palette <- function(pname, pal) {
    # usually people will call the set_heatmap_palette_helper, this function is provided in case we
    # need to subvert the usual route
    # dipsaus::cat2('Caching heatmap: ', pname, level='INFO')
    
    cache(key='current_rave_heatmap_palette_name', val=pname, 'current_rave_heatmap_palette_name', replace=TRUE)
    cache(key='current_rave_heatmap_palette', val=pal, 'current_rave_heatmap_palette', replace=TRUE)
}

#'
#'@export
expand_heatmap <- function(pal, results, ncolors, space='Lab', ...) {
    # interpolate_type=c('linear', 'spline'), color_space = c('Lab', 'rgb'),
    if(!missing(results)) {
        ncolors <- results$get_value('heatmap_number_color_values', 101)
        pal %?<-% results$get_value('heatmap_color_palette')
    }
    
    colorRampPalette(pal, space=space, ...)(ncolors)
}

# here we take care of the dark mode business, as well as reversing, inverting, and inside-out for the color scale
# Dark mode does an inside out by default ?
# please call set_palette_helper BEFORE calling this function so that we don't have to worry about setting
# the background plot color again
set_heatmap_palette_helper <- function(results, plot_options, ...) {
    results %?<-% build_results_object(plot_options)
    
    requested_palette_name = paste(results$get_value('heatmap_color_palette'),
                                   par('bg'),
                                 results$get_value('heatmap_number_color_values'),
                                 results$get_value('invert_colors_in_heatmap_palette'),
                                 results$get_value('reverse_colors_in_heatmap_palette'),
                                 sep = '_')

    ## FIXME reverting to old cache method 
    cached_palette_name = cache(key='current_rave_heatmap_palette_name', val='none', 
                                name='current_rave_heatmap_palette_name')
    
    if(cached_palette_name != requested_palette_name) {
        if(par('bg') %in% c('#1E1E1E', 'black')) {
            pal = get_dark_mode_heatmap_palette(get_heatmap_palette(results$get_value('heatmap_color_palette')))
        } else {
            pal = get_heatmap_palette(results$get_value('heatmap_color_palette'))
        }
        
        if(results$get_value('invert_colors_in_heatmap_palette', FALSE)) {
            pal %<>% invert_palette
        }
        
        if(results$get_value('reverse_colors_in_heatmap_palette', FALSE)) {
            pal %<>% rev
        }
        
        invisible(cache_heatmap_palette(requested_palette_name, expand_heatmap(pal, results = results)))
        
        # cache(key=requested_palette_name,
        #       val=expand_heatmap(pal, results = results),
        #       name='current_rave_heatmap_palette')
    }
    
}

get_currently_active_heatmap <- function() {
    rave_context()
    cache(key = 'current_rave_heatmap_palette', 
          val = {
              cat2('No heatmap is active, using default', level = 'WARNING')
              expand_heatmap(get_heatmap_palette('BlueWhiteRed'), ncolors = 101)
          },
          name = 'current_rave_heatmap_palette')
}

get_dark_mode_heatmap_palette <- function(pal, mid_color = par('bg')) {
    if(length(pal)==1) {
        pal %<>% get_heatmap_palette
    }
    
    # these palettes should all have an odd number of colors...
    m = floor(median(seq_len(length(pal))))
    
    c(pal[(m-1):1], mid_color, pal[length(pal):(m+1)])
}


#'
#'@export
#'
get_heatmap_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
    # Some of these are from:
    # http://colorbrewer2.org
    
    .heatmap_palettes <- list(
        BlueWhiteRed = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff", 
                             "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061") %>% rev,
        Spectral = c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf',
                     '#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2') %>% rev,
        BrownWhiteGreen = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5',
                       '#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),
        PinkWhiteGreen =c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
                          '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
        PurpleWhiteGreen = c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
                             '#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b'),
        OrangeWhitePurple = c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7',
                              '#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
        BlackWhiteRed = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#ffffff',
                           '#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a') %>% rev,
        BlueYellowRed = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
                          '#e0f3f8','#abd9e9','#74add1','#4575b4','#313695') %>% rev,
        GreenYellowRed = c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#ffffbf',
                           '#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837') %>% rev
    )
    if(missing(pname)) {
        if(get_palette_names)
            return (names(.heatmap_palettes))
        
        return (.heatmap_palettes)
    }
    
    pal <- .heatmap_palettes[[pname]]
    if(is.null(pal)) {
        cat2("Invalid palette requested: ", pname, ". Returning random palette",
             level="WARNING")
        pname = sample(names(.heatmap_palettes), 1)
        pal <- .heatmap_palettes[[pname]]
    }
    attr(pal, 'name') = pname
    
    return (pal)
}

set_palette <- function(pname) {
    if (is.null(pname)) {
      pname = get_palette(get_palette_names = TRUE)[1]  
    } else if(length(pname) == 1) {
        pname %<>% get_palette
    } 
    
    grDevices::palette(pname)
}

fix_name_for_js <- function(nm) {
    str_replace_all(nm, c(
        '\\(' = '.',
        '\\)' = '.',
        '\\ ' = '.',
        '-' = '.'
    ))
}
