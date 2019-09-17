#' @title Voltage Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
erp_over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)

    .time <- results$get_value('group_data')[[which.min(results$get_value('has_trials'))]]$all_data$dimnames$Time
    .windows <- list(results$get_value('ANALYSIS_WINDOW'),
                     results$get_value('BASELINE_WINDOW'))
    .wnames <- c('analysis', 'baseline')

    time_series_plot(plot_data = results$get_value('line_plot_data'),
                     PANEL.FIRST = time_series_decorator(results=results)
    )
}

#' @title By Trial Plot for ERP data
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
by_trial_erp_map <- function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)

    group_data <- results$get_value('group_data')
    has_trials <- results$get_value('has_trials')

    time_points <- results$get_value('voltage_sub')$dimnames$Time

    .baseline <- results$get_value('BASELINE_WINDOW')
    .analysis <- results$get_value('ANALYSIS_WINDOW')

    .max_zlim <- results$get_value('max_zlim')

    draw_many_heat_maps(group_data,
                        log_scale='',
                        max_zlim = .max_zlim,
                        PANEL.LAST = spectrogram_heatmap_decorator(results=results))
}


#' @title Welch periodogram per condition
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
by_condition_welch <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)

    .analysis <- results$get_value('ANALYSIS_WINDOW')
    .frequencies <- results$get_value('frequencies')
    .sample_rate <- results$get_value('ERP_SAMPLE_RATE')
    .group_data <- results$get_value('group_data')

    .has_trials <- results$get_value('has_trials')

    .xlim <- c(1,round(max(.frequencies),-2))

    # We need to make sure the number of fft is the same
    # max(min(nfft, length(x)), window)

    build_welch <- function(dat, nfft) {
        .window <- min(64, length(dat)-1)

        #.frequencies is defined above
        res = rave::pwelch(dat,
                     fs=2*max(.frequencies),
                     nfft = 256, noverlap = 8, window = .window, plot=F)

        ind <- res$freq %within% .xlim
        res$spec <- 10*log10(res$spec[ind])
        res$freq <- res$freq[ind]
        res
    }

    .pws <- lapply(which(.has_trials), function(ii) {
        # res.analysis_mean <- build_welch(group_data[[ii]]$analysis_data_mean)
        analysis.trials <- apply(.group_data[[ii]]$analysis_data, 1, build_welch)
        baseline.trials <- apply(.group_data[[ii]]$baseline_data, 1, build_welch)

        analysis_minus_baseline = mapply(function(a,b){
            if(!identical(a$freq, b$freq)) {
                return (NA)
                # print(paste('Al vs. Bl: ', length(a$spec), length(b$spec)))
            }

            a$spec - b$spec
        }, analysis.trials, baseline.trials)
        analysis_minus_baseline.mse <- analysis_minus_baseline %>% apply(1, .fast_mse) %>% t

        analysis.mse <- analysis.trials %>% sapply(extract2,'spec') %>% apply(1, .fast_mse) %>% t
        baseline.mse <- baseline.trials %>% sapply(extract2,'spec') %>% apply(1, .fast_mse) %>% t

        .range <- range(plus_minus(analysis.mse[,1], analysis.mse[,2]),
                        plus_minus(baseline.mse[,1], baseline.mse[,2]))

        list('analysis' = list(x=analysis.trials[[1]]$freq, y=analysis.mse),
             'baseline' = list(x=baseline.trials[[1]]$freq, y=baseline.mse),
             'diff' = list(x=analysis.trials[[1]]$freq, y=analysis_minus_baseline.mse),
             'range' = .range
        )
    })

    .ylim <- c(0, pretty(sapply(.pws, extract2, 'range'), n = 4)) %>% unique

    .row <- 1 + (length(.pws)-1) %/% 3
    .col <- 1 + (length(.pws)-1) %% 3
    .xax <- pretty(.frequencies)
    .xax[1] <- min(.frequencies)
    .xax[length(.xax)] <- max(.frequencies)
    # .xax <- c(1, 10^(unique(round(log10(.frequencies)))), max(.frequencies)) %>% unique

    par(mfrow=c(.row, .col))
    # have a parameter that controls whether we log the data or not
    for(ii in seq_along(.pws)) {
        plot_clean(.xlim, c(0,.ylim), log='', ylab='dB', asp=1)
        legend('topright', bty='n', inset=c(0.1, 0.1),
               text.col = c(grDevices::palette()[ii], 'black'), c(.group_data[[ii]]$name, 'baseline'),
               cex = rave_cex.lab)
        with(.pws[[ii]], {
            ebar_polygon(baseline$x, baseline$y[,1], baseline$y[,2], type='o', pch=16)
            ebar_polygon(analysis$x, analysis$y[,1], analysis$y[,2], type='o', pch=16, col=grDevices::palette()[ii])
        })
        rave_axis(2, at=.ylim)
        rave_axis(1, at=.xax)
    }

    # also show the difference plot? Like the differnece between conditions, or the difference from baseline?
    # plot(.pw$freq[ind], (10 * (log10(.pw$spec)-log10(.pw1$spec)))[ind], type='l')

    # for the welch periodogram, grab the power only within the analysis range
    # voltage_welch[[ii]]$welch <- pwelch(vsub[,voltage.time_ind], module_tools$get_sample_rate(original = TRUE), plot=FALSE)
    # }) #####

    # add a baseline welch

    # voltage_baseline <- vapply(volt$data[,baseline.ind,1]
}

