
#' @author John Magnotti
#' @title Draws several heatmaps in a row and (optionally) a color bar
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar.
#' By default it is setup for time/freq, but by swapping labels and decorators you can do anything
draw_many_heat_maps <- function(hmaps, x, y, xlab='Time', ylab='Frequency', log_scale=FALSE,
                                show_color_bar=TRUE, DECORATOR, max_zlim = 0, useRaster=TRUE, wide=FALSE, ...) {

    k <- sum(hmaps %>% get_list_elements('has_trials'))
    orig.pars <- layout_heat_maps(k)
    on.exit({
        par(orig.pars)
    })

    if(wide) {
        par(mar = c(5.1, 7, 2, 2))
    }

    # actual data range, as opposed to the max zlim which controls the plottable range
    actual_lim = get_data_range(hmaps)

    if(max_zlim==0) {
        max_zlim <- max(abs(actual_lim))
    }

    log_scale <- if(isTRUE(log_scale)){
        'y'
    } else {
        ''
    }

    lapply(hmaps, function(map){
        if(map$has_trials){
            # if y is a function, then use it to build the ys
            # this feels a little ridiculous, as we don't know the type of y, is there a cleaner way?
            # the problem is that draw_img() needs 'y' in order to put the image at the right location, so it is more than
            # just a decoration issue
            .y <- do_if(is.function(y), y(map$data), y)

            # draw_img(map$data, x = x, y = seq_along(.y), xlab=xlab, ylab=ylab,
            draw_img(map$data, x = x, y = .y, xlab=xlab, ylab=ylab,
                     zlim = c(-max_zlim, max_zlim),
                     main = map$name, log=log_scale, useRaster=useRaster)

            DECORATOR(map, x=x, y=.y, ylab=.y)
        }
    })

    if(show_color_bar){
        par(mar=c(5.1, 4.5, 2, 2),
            mai = c(0.6732, 0.5412, 0.5412, 0.2772))
        rave_color_bar(max_zlim, actual_lim)
    }

    invisible(hmaps)
}


#' @author John Magnotti
#' @title RAVE custom image plot
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators. We are just plotting image(zmat) Rather Than t(zmat) as you might expect. The Rave_calculators know this so we can save a few transposes along the way.
draw_img <- function(zmat, x, y, xlab='Time (s)', ylab='Frequency (Hz)',
                     zlim, log='', useRaster=TRUE, ...) {

    zmat %<>% clip_x(lim=zlim)
    # zlim <- range(zmat) %>% abs %>% max
    # zlim <- c(-zlim, zlim)
    #
    # .hmd <- results$get_value('heat_map_data')
    # zmat <- .hmd[[1]]$data
    # x <- results$get_value('time_points')
    # y <- results$get_value('frequencies')
    # log <- 'y'

    # because image plot centers the data on the y-variable, it can introduce 0s which fail when log='y'
    dy <- 0
    if(log == 'y') {
        dy <- (y[2]-y[1])/2 + min(y)
        plot_clean(x,y+dy,xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, log='y')
    }

    # was there a graph main title passed in?
    .main <- list(...)[['main']]
    .main %?<-% ''

    #TODO fix the name of crp here, make it a function
    image(x=x, y=y+dy, z=zmat, zlim=zlim, col=crp, useRaster = useRaster,
          log=log, add=(log=='y'), axes=F, xlab='', ylab='', main=.main)

    # return the clipped zmat
    invisible(zmat)
}


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

rave_color_bar <- function(zlim, actual_lim, clrs=rave_heat_map_colors, ylab='Mean % Signal Change',
                           mar=c(5.1, 5.1, 2, 2)) {
    cbar <- matrix(seq(-zlim, zlim, length=length(rave_heat_map_colors))) %>% t
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

    bthmd$data <- bthmd$data[,unlist(ind)]
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
        ruta_axis(2, tcl=0, lwd=0, at=yat, labels=map$ttypes)
    })

    invisible(map)
}

window_highlighter <- function(ylim, draw_labels=TRUE, windows, window_names) {

    do_wh <- function(ylim, draw_labels) {
        mapply(function(x, y, txt) {
            clr <- rave_colors[[toupper(txt %&% '_window')]]
            clr %?<-% 'gray50'

            do_poly(x, range(y), col=clr)
            if(draw_labels)
                text(min(x), max(y), txt, col=clr, adj=c(0,1))
        },
        windows, list(ylim, ylim), window_names)
        abline(h=0, col='gray70')
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


window_lines <- function(ylim, ...) {
    txts <- c('baseline', 'analysis')
    mapply(vertical_borders,
           list(BASELINE, TIME_RANGE),
           list(ylim, ylim),
           txts,
           rave_colors[toupper(txts %&% '_window')]
    )
    abline(h=0, col='gray70')
}

ts_labels_only <- function(plot_data) {
    ii <- which(plot_data %>% get_list_elements('has_trials'))
    nms <- plot_data %>% get_list_elements('name') %>% extract(ii)

    legend('topleft', legend=nms, ncol=ceiling(length(ii)/3),
           inset=c(.025,.075), bty='n',
           text.col=get_color(ii), cex=rave_cex.lab)

    invisible(plot_data)
}


# RUTABAGA
..get_nearest <- function(x,y) {
    sapply(x, function(.x) which.min(abs(.x-y)))
}

# RUTABAGA
`%near%` <- function(x, y, eps=1e-4) {
    abs(x-y) < eps
}


#
# x = sort(rnorm(10))
# ..get_nearest(pretty(x), x)


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
tf_hm_decorator <- function(hmap, x, y, xlab=x, ylab=y, ..., label.col='black', draw_time_baseline=TRUE, xax=TRUE, yax=TRUE,
                            TIME_RANGE = NULL, FREQUENCY = NULL, BASELINE = NULL) {

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

#' Try to guess decimal points of the number and returns string
#TODO RUTABAGA
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
