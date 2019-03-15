phase_rave_color_bar <- function(zlim, actual_lim, clrs=rave_heat_map_colors, ylab='Mean % Signal Change',
                           mar=c(5.1, 5.1, 2, 2), sym = T, ticks = NULL, digits = 1, ...) {
    max_zlim = max(abs(zlim))
    if(sym || length(zlim) == 1){
        draw_zlim = c(-1,1) * max_zlim
    }else{
        draw_zlim = zlim
    }


    cbar <- matrix(seq(-max_zlim, max_zlim, length=length(rave_heat_map_colors))) %>% t

    # this is from -1 to 1
    x = cbar / max_zlim
    sel = x %within% draw_zlim
    par(mar=mar)
    image(cbar[, sel, drop = F],
          col=clrs[sel], axes=F, ylab=ylab, main='',
          cex.main=rave_cex.main, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis, ...)

    rave_title(sprintf(
        '[%s]',
        paste(sapply(actual_lim, pretty_num, digits = digits), collapse = ':')
    ))

    labels = c(draw_zlim, -max_zlim, max_zlim, 0, ticks)
    at = labels / max_zlim
    unique = !duplicated(at)

    start = min(x[sel])
    end = max(x[sel]) - start
    rave_axis(2, at= (at[unique] - start) / end, labels = sprintf(
        sprintf('%%.%df', digits), labels[unique]), tcl=0.3)
    box()

    invisible(zlim)
}

sine_phase_over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    plot_data <- results$get_value('calc_results')

    mar = c(5.1, 5.1, 2, 2)
    lmar = c(5.1, 5.1, 2, 2)

    n_groups = results$get_value('n_groups')
    actual_lim = c(-1,1)

    easy_layout(n_groups, nrows = (n_groups>=4) + 1,s_margin = mar, legend = plot.new(),
                legend_size = ifelse(n_groups > 1, lcm(3.5), lcm(4.5)))

    lapply(plot_data, function(res) {
        time = res$phase$dimnames$Time
        trials = seq_along(res$phase$dimnames$Trial)

        .res <- res$phase$collapse(keep=c(1,3)) %>% t
        .res = t(sin(.res)) + seq_len(ncol(.res)) * 2
        yat <- seq(1, max(trials), length.out = 4) %>% round

        plot_clean(time, .res)
        rave_axis(1, pretty(time), tcl=0)
        rave_axis(2, 2*yat, labels=yat)
        abline(h=2*yat, lwd=0.5, col='gray80')

        #TODO this should be done in a decorator
        abline(v=results$get_value('ANALYSIS_WINDOW'), lty=2, lwd=2)

        apply(.res, 1, lines, x=time, col='gray30')
    })
}

phase_histogram <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    plot_data <- results$get_value('calc_results')

    .k <- results$get_value('n_groups')
    .rows = ceiling(.k / 3)


    `%get_match%` <- function(x, xi, is_match=`%within%`) {
        x[x %>% is_match(xi)]
    }

    time_range <- results$get_value('ANALYSIS_WINDOW')
    freq <- results$get_value('FREQUENCY')


    par(mfrow=c(.rows, .k/.rows), mar=c(1,1,3,1))

    # need to figure out ymax...
    mapply(function(res, clr) {
        .f <- res$phase$dimnames$Frequency
        .t <- round(res$phase$dimnames$Time[..get_nearest(time_range, res$phase$dimnames$Time)],3)

        .main <- res$group_name %&% ' | Phase @ ' %&% .f %&% ' Hz @ Time=' %&% .t
        .res <- res$phase$subset(Time = round(Time,5) %near% .t, Frequency = Frequency == .f)

        .res$get_data(drop = TRUE) %>% as.numeric %>% as.circular(units='rad', type='angles', template='none', modulo='asis', zero=0, rotation='counter') %>%
            hist(col=clr, main=.main, nticks=3)

    }, plot_data, rave_colors$GROUP[1+seq_along(plot_data)])
}

phase_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    plot_data <- results$get_value('calc_results')

    mar = c(5.1, 5.1, 2, 2)
    lmar = c(5.1, 5.1, 2, 2)

    n_groups = results$get_value('n_groups')
    actual_lim = c(0,pi)

    # png('phase_over_time', w=8, h=3, {
    easy_layout(n_groups, nrows = (n_groups>=4) + 1,s_margin = mar, legend = {
        phase_rave_color_bar(zlim = c(0, pi), actual_lim = actual_lim, clrs=rave_heat_map_colors %>% rev,
                       ylab = '|Phase|', sym = F, ticks = actual_lim, mar = lmar)
    }, legend_size = ifelse(n_groups > 1, lcm(3.5), lcm(4.5)))


    lapply(plot_data, function(res) {
        time = res$phase$dimnames$Time
        trials = seq_along(res$phase$dimnames$Trial)

        .res <- res$phase$collapse(keep=c(1,3)) %>% t

        image(z=abs(.res), zlim=c(-pi,pi), col=rave_heat_map_colors %>% rev,
              x=time, xlab='Time',
              y=trials, ylab='Trial',
              main = res$group_name %&% ' Abs Phase ' %&% deparse_selections(res$phase$dimnames$Frequency, max_lag = 10) %&% ' Hz',
              bty='n', las=1, axes=F)

        rave_axis(1, pretty(time), tcl=0)
        rave_axis(2, seq(1, max(trials), length.out = 4) %>% round, tcl=0)
    })
}

itpc_plot_heatmap = function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    plot_data <- results$get_value('calc_results')

    mar = c(5.1, 5.1, 2, 2)
    lmar = c(5.1, 5.1, 2, 2)

    n_groups = results$get_value('n_groups')
    actual_lim = results$get_value('actual_lim')
    time = results$get_value('time')

    #TODO make this first check if there is data here
    frequency = plot_data[[1]]$all_frequencies

    max_zlim <- results$get_value('max_zlim')
    if(max_zlim == 0) {
        max_zlim <- max(actual_lim)
    }

    easy_layout(n_groups, nrows = (n_groups>=4) + 1,s_margin = mar, legend = {
        phase_rave_color_bar(zlim = c(0, max(0.001, max_zlim)), actual_lim = actual_lim,
                       ylab = 'Inter-Trial Coherence', sym = F, ticks = median_ticks(max_zlim, .floor=0), mar = lmar)
    }, legend_size = ifelse(n_groups > 1, lcm(3.5), lcm(4.5)))

    # Plots
    # plot_data %>% sapply(function(x) x$data %>% class)
    lapply(plot_data, function(x){
        x$has_trials %?<-% FALSE
        if(x$has_trials){
            draw_img(zmat = x$full_data, x = time, y = frequency, xlab='Time (s)', ylab='Frequency (Hz)', zlim = c(-max_zlim,max_zlim),
                     main = x$name, useRaster = F)

            heat_map_axes(time,frequency)
            abline(v = results$get_value('ANALYSIS_WINDOW'), lwd = 3, lty = 2)
            abline(h = results$get_value('FREQUENCY'), lwd = 3, lty = 2)
        }
    })
}

itpc_time_plot = function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    plot_data <- results$get_value('calc_results')
    merge_plots <- results$get_value('MERGE_PLOTS')

    n_groups = results$get_value('n_groups')
    actual_lim = results$get_value('actual_lim')
    time = results$get_value('time')
    frequency = results$get_value('frequency')

    # Layout
    cols = lapply(seq_along(plot_data), function(ii){
        x = plot_data[[ii]]
        if(x$has_trials){
            list(
                group_name = ifelse(is.blank(x$group_name), sprintf('Group %d', ii), x$group_name),
                color = get_color(ii)
            )
        }else{
            NULL
        }
    })

    cols = dropNulls(cols)
    mar = c(5.1, 5.1, 2, 2)
    par(mar = mar)

    if(merge_plots){
        # use one big plot
        plot_clean(time, c(0,1), xlab = 'Time (s)', ylab = 'Inter-Trial Coherence')

        rave_axis(1, at=pretty(time))
        rave_axis(2, at=pretty(seq(0, 1, length.out = 11)))
        abline(v = results$get_value('ANALYSIS_WINDOW'), lwd = 3, lty = 2)
        # legend at 0, 0.9
        col = sapply(cols, '[[', 'color')
        legend(x = 0, y = 1, legend = sapply(cols, '[[', 'group_name'), lty = 1,
               col = col, cex = 1, bty = 'n', bg = c('#FFFFFFAA'),
               ncol = 2, text.col = col)
    }else{
        nrows = (n_groups>=4) + 1
        ncols = ceiling(n_groups / nrows)
        layout(
            matrix(seq_len(nrows * ncols), nrow = nrows, byrow = T)
        )
    }

    par(mar = mar)
    lapply(seq_along(plot_data), function(ii){
        x = plot_data[[ii]]
        if(!x$has_trials){
            return(NULL)
        }
        # apply(x$data, 1, function(x){
        #     c(mean(x), sd(x))
        # }) ->
        #     a

        a <- rbind(c(x$data), 0)

        if(!merge_plots){
            plot_clean(time, c(0,1), xlab = 'Time (s)', ylab = 'Inter-Trial Coherence')
            rave_axis(1, at=pretty(time), tcl=0, lwd=1)
            rave_axis(2, at=pretty(seq(0, 1, length.out = 11)), tcl=0, lwd=1)
            abline(v = results$get_value('ANALYSIS_WINDOW'), lwd = 3, lty = 2)

            legend(x = 0, y = 1, legend = ifelse(is.blank(x$group_name), sprintf('Group %d', ii),
                                                 x$group_name),
                   lty = 1,
                   col = get_color(ii), cex = 1, bty = 'n', bg = c('#FFFFFFAA'),
                   ncol = 2, text.col = get_color(ii))
        }
        rutabaga::ebar_polygon(time, a[1, ], a[2, ],
                     add_line = TRUE, col=get_color(ii))
    })
}
