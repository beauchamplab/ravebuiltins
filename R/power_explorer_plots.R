#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    otd <- results$get_value('over_time_data')
    
    otd_f1 <- lapply(otd, `[[`, 'F1')
    
    if(! ('F2' %in% names(otd[[1]]))) {
        args <- setup_over_time_plot(otd_f1, results)
        
    } else{
        otd_f2 <- lapply(otd, `[[`, 'F2')
        
        # if we're plotting F2 & F1, then we need to augment the legend and the plot title
        args <- setup_over_time_plot(append(otd_f1,otd_f2), results,
            plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Sample Size'))
    }
    
    do.call(time_series_plot, args)
    
    # otd_f1
    # 
    # time_series_plot(otd_f1, plot_time_range = results$get_value('plot_time_range'))
    # 
    # 
    # minimal <- list(
    #     data = otd_f1[[1]]$data,
    #     
    #     x = otd_f1[[1]]$x,
    #     
    #     range = range(plus_minus(otd_f1[[1]]$data)),
    #     
    #     has_trials = TRUE
    # )
    # 
    # time_series_plot(list(minimal), PANEL.FIRST = time_series_decorator())
    # 
    #     
    # time_series_plot(plot_data = list(
    # 
    # ),
    #     PANEL.FIRST = time_series_decorator())
    # 
    
}

over_time_plot2 <- function(results, ...) {
    otd <- results$get_value('over_time_data')
    
    otd_f2 <- lapply(otd, `[[`, 'F2')
    shiny::validate(shiny::need(!all(sapply(otd_f2, is.null)), 'No second frequency available'))
    
    
    par(mfrow=c(1,2))
    lapply(list(lapply(otd, `[[`, 'F1'), otd_f2), function(f) {
        do.call(time_series_plot, 
            setup_over_time_plot(f, results)
        )
    })
}
setup_over_time_plot <- function(otd, results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    extra_plot_vars <- list(...)
    if(length(extra_plot_vars) > 0) {
        
        if (!is.null(extra_plot_vars$plot_title_options)) {
            po$plot_title_options = po$plot_title_options[po$plot_title_options %in% extra_plot_vars$plot_title_options]
            # [names(extra_plot_vars)] = extra_plot_vars
            
        }
    }
    
    decorator <- NULL
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator <- stimulation_window_decorator
    }
    
    return(
        list(
            plot_data = otd,
            plot_time_range = results$get_value('plot_time_range'),
            PANEL.FIRST = time_series_decorator(plot_options=po),
            PANEL.LAST = decorator
        )
    )
    
}

#' Draws an orange, dashed horizontal line at cut. Checks for not null and 
#' length > 0
#'
#' @param cut the location(s) of the lines
#'
#' @return the value of cut (invisibly)
#'
#' @export
draw_cut_point <- function(cut=NULL) {
    if(!is.null(cut) & length(cut)>0) {
        abline(h=cut, lwd=1, col='orangered', lty=2)
    }
    invisible(cut)
}

across_electrode_statistics_plot <- function(results, ...) {
    validate(need(results$get_value('has_data', FALSE), message="No Condition Specified"))
    
    validate(need(results$get_value('omnibus_results', FALSE), message="No results available"))
    
    
    
    wrap_density <- function(expr) {
        og_mar = par('mar')
        on.exit(par(mar=og_mar), add = TRUE)
        with(eval(expr), {
            par(mar=c(og_mar[1], 0, og_mar[3], 1))
            plot_sideways_density(y, ylim, cut)
        })
    }
    
    
    f1_settings <- results$get_value('f1_analysis_settings')
    f2_settings <- results$get_value('f2_analysis_settings')
    f2_enabled <- isTRUE(f2_settings$enabled)
    
    # dipsaus::cat2("Starting AESP")
    # assign('omnibus_results', results$omnibus_results, envir=globalenv())
    
    render_stats <- function(mat, settings, label_side = 3) {
        lapply(c('mean', 't'), function(sv) {
            aesph <- across_electrode_statistics_plot_helper(results, results_matrix = mat, sv,
                passing_electrodes=determine_passing_electrodes(results, results_matrix = mat))
            
            if(sv =='mean') {
                lbl <- with(settings, sprintf('%sHz %ss after %s',
                    str_collapse(frequency_window, ':'), 
                    str_collapse(analysis_window, '-'), event_of_interest))
                
                # offset <- diff(graphics::grconvertX(par('usr')[c(4,2)], to='line')) / 2
                
                mtext(lbl, outer=TRUE, side=label_side, adj=0, padj=0.75, las=1)
            }
            
            wrap_density(aesph)
        })
        
        
        # p-values a little trickier because we need to transform the result
        wrap_density(
            across_electrode_statistics_plot_helper(
                results, results_matrix = mat, 'p', 
                TRANSFORM = function(p) {-log10(p)},
                PANEL.LAST = function(yat, ...){
                    for(tick in yat) {
                        rave_axis(2, at=tick, labels=bquote(10**.(-tick)), tcl=0,
                            cex.axis = rave_cex.axis*.5*get_cex_for_multifigure())
                    }
                }
            )
        )
    }
    
    if(isTRUE(results$get_value('show_result_densities'))) {
        if(f2_enabled) {
            layout(matrix(1:12, nrow=2, byrow=TRUE), widths = rep(c(3.5,1), 6))
        } else {
            layout(matrix(1:6, nrow=1), widths = rep(c(3.5,1), 3))
        }
    } else {
        if(isTRUE(results$get_value('f2_analysis_settings')$enabled)) {
            layout(matrix(1:6, nrow=2, byrow=TRUE), widths = 1)
        } else {
            layout(matrix(1:3, nrow=1), widths = 1)
        }
        # no densities here, so just NOOP the function to avoid if/else later
        wrap_density = force
    }
    
    # a bit extra left margin
    par(mar=c(5.1, 4.1+2, 4.1, 2.1), oma=c(0, .75, 1, 0))
    
    # if we have two frequencies, split the result object and render both
    or <- results$get_value('omnibus_results')
    
    # the key is to set f1 to be !F2, as that grabs the rows that are common to both (e.g., Selected Electrodes)
    f1 <- or[!startsWith(rownames(or),'F2'),]
    
    
    # information in top left
    render_stats(f1, f1_settings)
    
    
    if(f2_enabled) {
        f2 <- or[!startsWith(rownames(or),'F1'),]
        render_stats(f2, f2_settings, label_side =2)
        par('usr')
        
        # mtext('F1\n' %&% , at = par('usr')[c(1,4)])
    } 
}

plot_sideways_density <- function(x, xlim, cut=NULL) {
    den = density(x, from=min(xlim), to=max(xlim))
    den$tmp = den$y
    den$y = den$x
    den$x = den$tmp
    plot(den, axes=F, xlab='', ylab='', main='')
    rug(x, side=2, col=get_foreground_color(), lwd = 1, ticksize = 0.05)
    
    draw_cut_point(cut)
}

make_stat_filter <- function(fname) {
    ll = list(
        't' = force,
        'abs(t)' = abs,
        'mean' = force,
        'abs(mean)' = abs,
        'p' = force,
        'FDR(p)' = function(p) p.adjust(p, method='fdr'),
        'Bonf(p)' = function(p) p.adjust(p, method='bonferroni')
    )
    
    fname = match.arg(fname, names(ll))
    
    ll[[fname]]
}

across_electrode_statistics_plot_helper <- function(results, results_matrix,
    stat_var, TRANSFORM=force, PANEL.LAST=NULL, 
    show_yaxis_labels = !is.function(PANEL.LAST), passing_electrodes, ...) {
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No data available"))
    
    available_stats = c('mean', 't', 'p')
    stat_var <- match.arg(stat_var, available_stats)
    stat_ind = which(stat_var == available_stats)
    
    set_palette_helper(results)
    res = get_active_result(results, results_matrix = results_matrix)[which(stat_var == available_stats),]
    
    # we need to see if there is an a priori transform being requested
    # awkwardly this is called a filter...
    filter_name = results$get_value(stat_var %&% '_filter')
    FILT = make_stat_filter(filter_name)
    res %<>% FILT
    cut_val = FILT(as.numeric(results$get_value(stat_var %&% '_operand')))
    
    # now apply the user supplied transform
    if(is.function(TRANSFORM)) {
        res %<>% TRANSFORM
        # don't forget to transform the cut value!
        cut_val %<>% TRANSFORM
    }
    
    # if there is a cutpoint, we should force it to be shown on the plot
    # by building it into our ylim calculation
    
    ylim = pretty(c(cut_val, res), n=4)# %>% pretty_round
    # if(min(ylim) %within% c(1e-3, 1-1e-3)) {
    #     ylim = c(0, ylim)
    # }
    plot_clean(seq_along(res), c(ylim, res))
    
    across_electrodes_xaxis(vector_to_row_matrix(res))
    rave_axis_labels(ylab=filter_name, mgp=c(3.5, 0,0))
    draw_passing_points(res, results, passing_electrodes)
    
    yat = ylim#axTicks(2)# %>% pretty(n=4) %>% unique
    #the ylab seems to be getting cutoff more often than not
    rave_axis(2, at=yat, labels = show_yaxis_labels)
    
    wrtsoe = results$get_value('which_result_to_show_on_electrodes')
    unit_of_analysis = results$get_value('unit_of_analysis')
    
    rave_title(paste(results$get_value(stat_var %&% '_filter'), '\n',
        get_result_name(wrtsoe), unit_of_analysis),
        cex = rave_cex.main*.75)
    
    draw_cut_point(cut_val)
    
    if(is.function(PANEL.LAST)) {
        PANEL.LAST(yat=yat, ...)
    }
    
    invisible(list(
        y=res,
        ylim=ylim,
        cut=cut_val
    ))
}

# several functions will need to use this
determine_passing_electrodes <- function(results, results_matrix, ...) {
    ### we need to update this to select the appropriate value!
    # res <- results$get_value('omnibus_results')
    res <- get_active_result(results, results_matrix = results_matrix)
    
    # if(!is.matrix(res)) {
    #     res = matrix(res, nrow=1, dimnames = list(NULL, names(res)))
    # }
    
    v <- c('mean', 'p', 't')
    filters <- sapply(v %&% '_filter', function(e) results$get_value(e))
    operators <- sapply(v %&% '_operator', function(e) results$get_value(e))
    operands <- sapply(v %&% '_operand', function(e) results$get_value(e))
    
    pass_the_test <- rep(TRUE, ncol(res))#length(results$get_value('electrodes')))
    
    # operands[1] = '50'
    for(ii in seq_along(filters)) {
        # first check if there is a valid operand
        if(all(operands[ii] != "", not_NA(as.numeric(operands[ii]))) ) {
            # convert the operator to its corresponding method
            OP <- getMethod(operators[ii])
            # default to p-value
            if(filters[ii] %in% c('p', 'FDR(p)', 'Bonf(p)')) {
                val = res[3,]
            } else if (filters[ii] %in% c('t', 'abs(t)')) {
                val = res[2,]
            } else if (filters[ii] %in% c('mean', 'abs(mean)')) {
                val = res[1,]                
            }
            
            FILT <- make_stat_filter(filters[ii])
            
            pass_the_test = pass_the_test & OP(FILT(val),as.numeric(operands[ii]))
        }
    }
    
    # now we do the check on the anatomical filters
    emeta <- results$get_value('electrodes_csv')
    afilt <- sapply('analysis_filter_variable' %&% c('', '_2'), function(e) results$get_value(e))
    aval <- lapply('analysis_filter_elec' %&% c('', '_2'), function(e) results$get_value(e))
    
    # print(results$get_value('analysis_filter_elec'))
    
    for(ii in seq_along(afilt)) {
        if(afilt[ii] != 'none') {
            key <- as.character(afilt[ii])
            val <- unlist(aval[[ii]])
            el_vals <- paste0(emeta[[key]])
            
            ### there is weirdness here with the NA label
            if(any(is.na(val))) val %<>% paste0
            
            pass_the_test = pass_the_test & (el_vals %in% val)
        }
    } 
    
    if(shiny_is_running()){
        updateTextInput(getDefaultReactiveDomain(), 'current_active_set', 
            value=deparse_svec(emeta$Electrode[pass_the_test]))
    }
    return(pass_the_test)
}

across_electrodes_xaxis <- function(xmat) {
    xat = as.integer(pretty(seq_along(xmat[1,])))
    xat[xat==0] = 1 
    xat %<>% unique
    abline(v=xat, col='gray80', lwd=0.5)
    rave_axis(1, at=xat, labels=colnames(xmat)[xat])
    rave_axis_labels(xlab='Electrode #')
    invisible(xat)
}

draw_passing_points <- function(y, results, passing_electrodes) {
    if(missing(passing_electrodes)) {
        passing_electrodes <- determine_passing_electrodes(results)
    }
    points(y, cex=1.1, pch=ifelse(passing_electrodes, 19, 1),
        col=get_foreground_color())#ifelse(passing_els, get_foreground_color(), 'gray50'))
}

get_active_result <- function(results, results_matrix, ...) {
    res_name = results$get_value('which_result_to_show_on_electrodes')
    
    if(missing(results_matrix)) {
        results_matrix = results$get_value('omnibus_results')
    }
    begin = 1
    if(res_name != "Omnibus Activity (across all active trial types)") {
        begin = which(endsWith(rownames(results_matrix), res_name))[1]
    }
    # also need to check if rownames of OR match those we just built
    shiny::validate(shiny::need(length(begin) == 1 && !is.na(begin),
        message = 'Selected data not available. Press Recalculate button'))
    
    ind = begin:(begin+2)
    
    # keep the result a matrix
    return (results_matrix[ind,,drop=FALSE])
}

vector_to_row_matrix <- function(y) {
    matrix(y, nrow=1, dimnames = list(c(),names(y)))
}

get_result_name <- function(full_name) {
    if(startsWith(full_name, 'Omnibus')) {
        return ("Across all trials")
    }
    return(full_name)
}


#' @title By Trial Plot With Statistics
#' @param results results returned by module
#' @param ... other parameters passed to module output
windowed_comparison_plot <- function(results, ...){
    rave_context()
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    # dipsaus::cat2("Starting WCP")
    set_palette_helper(results)
    
    # po = results$get_value('plot_options')
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    sbd <- results$get_value('scatter_bar_data')
    # sbdf1 <- lapply(sbd, `[[`, 'F1')
    
    ### check if we need to highlight any points
    # trial_scatter_plot(
    #     group_data = sbdf1,
    #     show_outliers = results$get_value('show_outliers_on_plots'),
    #     PANEL.LAST = trial_scatter_plot_decortator(plot_title_options = po$plot_title_options)
    # )
    
    fd <- results$get_value('flat_data')
    fd$freq %<>% factor
    fd$group_name %<>% factor
    
    # outliers <- results$get_value('trial_outliers_list')
    # dipsaus::cat2('Outliers: ')
    # dipsaus::cat2(str_collapse(outliers))
    # outliers <- sample(fd$orig_trial_number, 50)
    
    k = prod(nlevels(fd$group_name), nlevels(fd$freq))
    
    layout(matrix(c(0,1,0), nrow=1), widths = c(1, lcm(15 + 2*(k-1)), 1))
    par(mar=.1+c(5,10,4,2))
    
    if(nlevels(fd$freq) > 1) {
        pt.loc <- plot_grouped_data(mat = fd, yvar = 'y', xvar = 'group_name', gvar='freq', draw0=min(fd$y)<0,
                               jitter_seed=results$get_value('jitter_seed'))
        rave_axis_labels(xlab='Frequency', ylab=attr(sbd[[1]]$F1$data, 'ylab'), line=4)
    } else {
        pt.loc <- plot_grouped_data(mat = fd, yvar = 'y', xvar = 'group_name', draw0 = min(fd$y)<0,
                               jitter_seed=results$get_value('jitter_seed'))
        axis_label_decorator(sbd[[1]]$F1, label_alignment=FALSE, line=4)
    }
    
    rave_axis(2, at=axTicks(2))
    # if(min(fd$y, axTicks(2)) < 0) abline(h=0, col='lightgray')
    # assign('pt.loc', pt.loc, envir=globalenv())
    # line up the x-locations with the y-locations to support clickable locations
    # first we need to create a data.frame out of the two lists
    all_points <- do.call(rbind, mapply(cbind, 'x'=pt.loc$x, pt.loc$y, SIMPLIFY = FALSE))
    # colnames(all_points) <- c('x', 'y')
    # withx <- merge(fd, all_points, by='y', sort=FALSE)

    # dipsaus::cat2("caching wcpd data", level='INFO')
    cache(key = 'current_rbn_windowed_comparison_plot_data',
          val = force(all_points),
          name='rbn_windowed_comparison_plot_data',
          replace = TRUE)
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
heat_map_plot <- function(results, ...){
    hmd <- results$get_value('heat_map_data')
    # str(hmaps)
    
    hmaps <- lapply(hmd, `[[`, 'F1')
    
    
    args <- setup_heat_map_plot(results)
    args$hmaps = hmaps
    
    do.call(draw_many_heat_maps, args)

}

setup_heat_map_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    decorator <- spectrogram_heatmap_decorator(plot_options = po)
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator %<>% add_decorator(stimulation_window_decorator)
    }
    ignore_time_range = NULL
    if(results$get_value('censor_stimulation_window')) {
        ignore_time_range <- results$get_value('stimulation_window')
    }
    
    return(
        list(
            log_scale = results$get_value('log_scale'),
            max_zlim = results$get_value('max_zlim', 0),
            percentile_range=results$get_value('percentile_range'),
            plot_time_range = results$get_value('plot_time_range'),
            ignore_time_range = ignore_time_range,
            PANEL.LAST = decorator,
            max_columns = results$get_value('max_column_heatmap'),
            decorate_all_plots = results$get_value('redundant_labels', FALSE),
            center_multipanel_title = results$get_value('center_multipanel_title'),
            PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator, 0)
        )
    )
    
}

frequency_correlation_plot <- function(results, ...) {
    hmd <- results$get_value('heat_map_data')
    hmaps <- lapply(hmd, `[[`, 'F1_F2')
    
    shiny::validate(shiny::need(!all(sapply(hmaps, is.null)), 'Frequency correlations not available'))
    
    args <- setup_heat_map_plot(results)
    
    args$hmaps = hmaps
    
    # we need to customize a few things because these are correlation plots
    args$plot_time_range = range(hmaps[[1]]$x)
    
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    po$plot_title_options = with(po, plot_title_options[!plot_title_options %in% c('Baseline Window',
                                                                                   'Analysis Window', 'Frequency Range')])
    
    po$plot_title_options %<>% c('Frequency Range')
    
    po$draw_decorator_labels = FALSE
    fw <- hmaps[[1]]$frequency_window
    fw2 <- fw
    if(hmaps[[1]]$enable_frequency_window2) {
        fw2 = hmaps[[1]]$frequency_window2
    }
    args$PANEL.LAST = add_decorator(spectrogram_heatmap_decorator(plot_options = po), function(..., Xmap=force, Ymap=force) {
        draw.box(Xmap(fw[1]), Ymap(fw[1]), Xmap(fw[2]), Ymap(fw[2]), lty=2, lwd=2)
        text(labels='F1', fw[1] + diff(fw)/2, fw[2], pos='3')
        if(!all(fw == fw2)) {
            draw.box(Xmap(fw2[1]), Ymap(fw2[1]), Xmap(fw2[2]), Ymap(fw2[2]), lty=2, lwd=2)
            text(labels='F2', fw2[1] + diff(fw2)/2, fw2[2], pos='3')
        }
        
    })
    # args$extra_plot_parameters = list('asp'=1)
    if(length(hmaps) < 3) {
        args$do_layout = FALSE
        cbar <- lcm(3.5)
        
        widths = rep(1, length(hmaps))
        widths = c(widths, cbar, rep(1, 3-length(hmaps)))
        
        m = c(1:(length(hmaps) + 1), rep(0, 3-length(hmaps)))
        # m = c(m, rep(0, 4-length(m)))
        
        # widths[seq_along(hmaps)] = lcm(15)
        
        # widths[which.max(m)] = cbar
        layout(mat = matrix(m, nrow=1), widths = widths)
        par(pty='s')
        
        args$marginal_text_fields = character(0)
    } else {
        args$marginal_text_fields = c('Subject ID', 'Electrode')
    }
    
    do.call(draw_many_heat_maps, args)
}

heatmap_plot_helper <- function(results, hmap_varname, ...) {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    ncol <- results$get_value('max_column_heatmap')
    
    hmd <- results$get_value(hmap_varname)
    
    draw_many_heat_maps(hmd,
        percentile_range=results$get_value('percentile_range'),
        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
        plot_time_range = results$get_value('plot_time_range'),
        max_columns = ncol,
        decorate_all_plots = results$get_value('redundant_labels', FALSE),
        center_multipanel_title = results$get_value('center_multipanel_title'),
        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE),
            color_bar_title_decorator, 0),
        ...
    )
}

setup_by_electrode_heat_map_plot <- function(behmd, results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    decorator <- by_electrode_heat_map_decorator(plot_options = po)
    
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator %<>% add_decorator(stimulation_window_decorator)
    }
    
    ignore_time_range = NULL
    if(results$get_value('censor_stimulation_window')) {
        ignore_time_range <- results$get_value('stimulation_window')
    }
    args <- list(
        hmaps=behmd,
        percentile_range=results$get_value('percentile_range'),
        max_zlim = results$get_value('max_zlim'),
        log_scale=FALSE,
        plot_time_range = results$get_value('plot_time_range'),
        PANEL.LAST=decorator,
        max_columns = results$get_value('max_column_heatmap'),
        ignore_time_range = ignore_time_range,
        decorate_all_plots = results$get_value('redundant_labels', FALSE),
        center_multipanel_title = results$get_value('center_multipanel_title'),
        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE),
            color_bar_title_decorator, 0)
    )
    
    return(args)
}

by_electrode_heat_map_plot <- function(results, ...) {
    d <- results$get_value('by_electrode_heat_map_data')
    df <- lapply(d, `[[`, 'F1')
    
    if('F2' %in% names(d[[1]])) {
        # grab this before it changes!    
        ngroups <- sum(get_list_elements(df, 'has_trials'))
        
        df2 <- lapply(d, `[[`, 'F2')
        
        # determine the order of the data based on who has the lower frequency. Put the higher frequency range FIRST because plots are down
        # in tabular order, not graphics order
        df <- if(mean(df2[[1]]$frequency_window) > mean(df[[1]]$frequency_window)) {
            append(df2, df)
        } else {
            append(df, df2)
        }
        
        fs <- sapply(unique(get_list_elements(df, 'frequency_window', use_sapply = FALSE)), paste, collapse=':')
        
        args <- setup_by_electrode_heat_map_plot(df, results)
        
        args$PANEL.LAST= NULL
        args$marginal_text_fields <- c('Subject ID', "Electrode")
        args$do_layout = FALSE
        
        # make our own layout, force frequency to be on the same row
        layout_heat_maps(length(df), max_col = ngroups, layout_color_bar = TRUE)
        par('oma' = c(0, 0.5, 1.5, 0))
        
        do.call(draw_many_heat_maps,args)
        
        # put some labels in the margin
        mtext(outer = TRUE, side=2, line=-2, cex=rave_cex.axis,# * get_cex_for_multifigure(),
              text = paste('Freq', fs, 'Hz'), at= c(0.75, 0.25)
        )
                
    } else {
        do.call(draw_many_heat_maps,
                args = setup_by_electrode_heat_map_plot(df, results)
        )
    }
    
}
# 
# over_time_correlation_plot <- function(results, ...) {
#     shiny::validate(shiny::need(FALSE, 'Plot disabled'))
# }

windowed_correlation_plot <- function(results, ...) {
    # shiny::validate(shiny::need(FALSE, 'Plot disabled'))
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    sbd <- results$get_value('scatter_bar_data')
    
    sbdf2 <- lapply(sbd, `[[`, 'F2')
    
    shiny::validate(shiny::need(!all(sapply(sbdf2, is.null)), 'No second frequency available'))
    
    freqs <- list('F1' = lapply(sbd, `[[`, 'F1'), 'F2' = sbdf2)
    # dipsaus::cat2("Starting WCP")
    set_palette_helper(results)

    gr <- function(ll) {
        pretty(sapply(ll, function(li) range(li$data)))
    }
    
    xlim <- gr(freqs$F2)
    ylim <- gr(freqs$F1)
    
    ncol = min(3, length(freqs$F1))
    r = ceiling(length(freqs$F1) / ncol)
    par(mfrow=c(r,ncol), mar=rep(2,4), oma=c(3,3,0,0))
    mapply(function(f1,f2) {
        plot_clean(xlim, ylim)
        rave_axis(1, at=axTicks(1))
        rave_axis(2, at=axTicks(2))
        points(f2$data, f1$data, pch=16, col=f2$group_info$current_group)
        legend('topright', cex=rave_cex.lab, bty = 'n', legend=cor.test(f1$data, f2$data) %$% 
                   sprintf("r = %s\np = %s", round(estimate,2), format.pval(p.value,digits=2)))
        abline(lm(
            f1$data ~ f2$data
        ), lty=2, col=f2$group_info$current_group)
    }, freqs$F1, freqs$F2)
    
    for(ii in 1:2) {
        mtext(outer=TRUE, side=3-ii, cex = rave_cex.lab*get_cex_for_multifigure(),
            line=1, attr(freqs[[ii]][[1]]$data, 'ylab') %&% ' ' %&% paste0(collapse=':', freqs[[ii]][[1]]$frequency_window) %&% "Hz")
    }
    
}

by_electrode_heat_map_plot2 <- function(results, ...) {
    shiny::validate(shiny::need(FALSE, 'Plot disabled'))
    
    # d <- results$get_value('by_electrode_heat_map_data')
    # 
    # df2 <- lapply(d, `[[`, 'F2')
    # shiny::validate(shiny::need(!all(sapply(df2, is.null)), 'No second frequency available'))
    # 
    # args <- setup_by_electrode_heat_map_plot(df2, results)
    # 
    # do.call(draw_many_heat_maps, args)
}

# this is separated out as other plots may need to do this
remove_outliers_from_by_trial_data <- function(bthmd) {
    for(ii in seq_along(bthmd)) {
        
        .clean <- bthmd[[ii]]$is_clean
        if(sum(.clean) == 0) {
            bthmd[[ii]]$has_trials <- FALSE
            dipsaus::cat2('All trials flagged as outliers...', level='WARNING')
        } else if(all(.clean)){
            # do nothing
        } else {
            # subsetting the data drops the attributes, so temp storage until we re-assign
            xlab = attr(bthmd[[ii]]$data, 'xlab')
            ylab = attr(bthmd[[ii]]$data, 'ylab')
            
            # note that $data is perhaps the transpose of what you expect, that's because the image() function requires
            # the transpose of what you might expect to plot what you might expect
            bthmd[[ii]]$data <- bthmd[[ii]]$data[,.clean]
            
            attr(bthmd[[ii]]$data, 'xlab') = xlab
            attr(bthmd[[ii]]$data, 'ylab') = ylab
            
            # update all the meta data
            bthmd[[ii]]$Trial_num <- bthmd[[ii]]$Trial_num[.clean]
            bthmd[[ii]]$trials <- bthmd[[ii]]$trials[.clean]
            bthmd[[ii]]$range <- .fast_range(c(bthmd[[ii]]$data))
            bthmd[[ii]]$y <- seq_along(bthmd[[ii]]$Trial_num)
        }
    }
    return(bthmd)
}

# setup function to handle by_trial_hmp for multiple frequencies
setup_by_trial_heat_map_plot <- function(bthmd_single_frequency, results, ...)  {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    #base decorator
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    decorator <- by_trial_heat_map_decorator(plot_options = po)
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    sort_trials_by_type <- results$get_value('sort_trials_by_type', 'Trial Number')
    if(sort_trials_by_type != 'Trial Number') {
        for(ii in which(results$get_value('has_trials'))) {
            bthmd_single_frequency[[ii]] %<>% reorder_trials_by_event(event_name = sort_trials_by_type)
        }
        # add a decorator that can draw the trial labels
        if(sort_trials_by_type == 'Condition') {
            decorator %<>% add_decorator(trial_type_boundaries_hm_decorator)
        } else  {
            decorator %<>% add_decorator(by_trial_analysis_window_decorator(event_name= sort_trials_by_type,
                show_label = po$draw_decorator_labels))
        }
    }
    
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator %<>% add_decorator(stimulation_window_decorator)
    }
    
    ignore_time_range = NULL
    if(results$get_value('censor_stimulation_window')) {
        ignore_time_range <- results$get_value('stimulation_window')
    }
    
    show_outliers <- results$get_value('show_outliers_on_plots', FALSE)
    if(show_outliers) {
        # print('showing outliers')
        decorator %<>% add_decorator(heatmap_outlier_highlighter_decorator)
    } else {
        # print('not showing outliers, removing them, start with: ' %&% nrow(by_trial_heat_map_data[[1]]$data))
        bthmd_single_frequency %<>% remove_outliers_from_by_trial_data
    }
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    need_wide = ('Condition' == sort_trials_by_type)
    
    # user-controlled heatmap nrow
    args <- list(
        hmaps = bthmd_single_frequency,
        max_zlim = results$get_value('max_zlim'),
        log_scale=FALSE,
        percentile_range=results$get_value('percentile_range'),
        wide = need_wide,
        PANEL.LAST=decorator,
        ignore_time_range = ignore_time_range,
        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator, 0),
        plot_time_range = results$get_value('plot_time_range'),
        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
        axes=c(TRUE, !need_wide),
        center_multipanel_title = results$get_value('center_multipanel_title'),
        decorate_all_plots = results$get_value('redundant_labels', FALSE),
        max_columns = results$get_value('max_column_heatmap')
    )
    return(args)
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
by_trial_heat_map_plot <- function(results, ...) {
    d <- results$get_value('by_trial_heat_map_data')
    df1 <- lapply(d, `[[`, 'F1')
    
    args <- setup_by_trial_heat_map_plot(results, bthmd_single_frequency = df1)
    
    do.call(draw_many_heat_maps, args)
}

by_trial_heat_map_plot2 <- function(results, ...) {
    d <- results$get_value('by_trial_heat_map_data')
    
    df2 <- lapply(d, `[[`, 'F2')
    shiny::validate(shiny::need(!all(sapply(df2, is.null)), 'No second frequency available'))
    
    args <- setup_by_trial_heat_map_plot(results, bthmd_single_frequency = df2)
    
    do.call(draw_many_heat_maps, args)
}


trialwise_correlation_plot <- function(results, ...) {
    d <- results$get_value('by_trial_heat_map_data')
    
    df2 <- lapply(d, `[[`, 'F2')
    shiny::validate(shiny::need(!all(sapply(df2, is.null)), 'No second frequency available'))
    
    df1 <- lapply(d, `[[`, 'F1')
    
    # rem out the baseline window from the by-trial correlation
    bw <- with(df2[[1]], x %within% baseline_window)
        # f1 = df1[[1]]
        # f2=df2[[1]]
    all_cors <- mapply(function(f1, f2) {
        diag(cor(f1$data[!bw,], f2$data[!bw,]))
    },df1, df2, SIMPLIFY = FALSE)
    brks <- -20:20/20
    hists <- lapply(all_cors, hist, breaks=brks, plot=FALSE)
    
    xlim <- round_to_nearest(val=.1, c(-0.05, 0.05) + range(sapply(all_cors, range)))
    ylim <- round_to_nearest(val=5, range(sapply(hists, function(h) range(h$counts))))
    
    ncol = min(3, length(all_cors))
    r = ceiling(length(all_cors) / ncol)
    par(mfrow=c(r,ncol), mar=rep(2,4), oma=c(4,4,0,0))
    for(ii in seq_along(df1)) {
        y <- all_cors[[ii]]
        cl <- df1[[ii]]$group_info$current_group
        hist(y, col=adjustcolor(cl, .5), border=cl, breaks=brks, ylim=ylim, xlim=xlim, axes=F, main='')
        abline(v=median(y), lty=2, col=cl)
        rave_axis(1, at=seq(from=xlim[1], to=xlim[2], length.out=5))
        rave_axis(2, at=round(seq(from=ylim[1], to=ylim[2], length.out=3)))
        rave_title(df1[[ii]]$name, col=cl)
        legend('topleft', inset = c(-.075,-0.025), legend = {
            sprintf('n = %d\nmed = %4.2f\np = %s', length(y), round(median(y),2), format.pval(digits=2,wilcox.test(y)$p.value))
        }, bty='n', cex=get_cex_for_multifigure()*rave_cex.lab)
        rug(y, col=cl)
    }
    mtext('Trial-level F1-F2 correlation', side=1, line=1, outer=TRUE, cex=rave_cex.axis)
    mtext('Count', side=2, line=1, outer=TRUE, cex=rave_cex.axis)
        
}


replace_middle <- function(x, rpl) {
    x[c(-1, -length(x))] = rpl
    
    return(x)
}

assess_normality_plot <- function(results, ...) {
    # rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    # layout(matrix(c(1,2,3,3), nrow=1), widths = rep(1,4))
    # layout(matrix(2,1), widths = lcm(20))
    
    all_res = results$get_value('scatter_bar_data')
    
    if('F2' %in% c(sapply(all_res, names))) {
        par(mfrow=c(2,2))
        nms <- c('F1', 'F2')
    } else {
        par(mfrow=c(1,2))
        nms <- c('F1')
    }
    
    lapply(nms, function(nm) {
        sbd <- lapply(all_res, `[[`, nm)
        
        # only grab columsn with data
        has_trials = which(sapply(sbd, `[[`, 'has_trials'))
        
        all_data = lapply(sbd[has_trials], `[[`, 'data')
        names(all_data) = sapply(sbd[has_trials], `[[`, 'name')
        
        all_data_v = unlist(all_data)
        
        # overall normality uses all data, ignore the condition variable
        d.omni = density(all_data_v)
        # overlay m_sd of a normal distribution?
        n.sim = 50
        
        msd = m_sd(all_data_v)
        set.seed(results$get_value('jitter_seed'))
        sims <- matrix(nrow=n.sim,rnorm(n.sim*length(all_data_v), msd[1], msd[2]))
        many_dens <- apply(sims, 1, density, bw=d.omni$bw)
        
        xlim = c(d.omni$x,
            plus_minus(msd[1], 3*msd[2])
        )
        
        ylim = c(d.omni$y, sapply(many_dens, function(d) range(d$y)))
        
        par(mar=c(5,6,4,3))
        plot_clean(xlim, ylim)
        # title_decorator()
        rave_title(paste('Omni dist for E', 
            dipsaus::deparse_svec(sbd[[1]]$electrodes, max_lag=1), ', F', paste0(sbd[[1]]$frequency_window, collapse=':'))
        )
        rug(all_data_v)
        do_density_axes <- function(ylab=TRUE) {
            rave_axis(1, at=axTicks(1), mgpx=c(3,1,0))
            
            rave_axis(2, at=axTicks(2), labels = replace_middle(axTicks(2), ""))
            if(ylab) {
                rave_axis_labels(ylab='Density', xlab=attr(sbd[[1]]$data, 'ylab'))
            }
        }
        sapply(many_dens, lines, col=get_middleground_color(0.2), lwd=0.5)
        lines(d.omni$x, d.omni$y, lwd=2, col=get_foreground_color())
        
        ks <- ks.test(all_data_v, y='pnorm', mean = msd[1], sd=msd[2])
        legend(x=max(axTicks(1)), y = par('usr')[4],
            yjust=1, adj=c(0.5,1), xpd=TRUE, xjust=0.5,
            bty='n', inset=c(0,0),
            legend=paste0('K-S Test\np = ', format.pval(ks$p.value, digits = 2)),
            cex = 0.9*rave_cex.lab)
        do_density_axes()
        
        # get the colors for each group    
        ci = has_trials
        
        # get the conditional distributions?
        par(mar=c(5,3,4,4))
        cond.dens = lapply(all_data, density)
        plot_clean(sapply(cond.dens, `[[`, 'x'),  sapply(cond.dens, `[[`, 'y'))
        mapply(function(dens, col, y){
            lines(dens, col=col, lwd=2)
            rug(y, col=col)
        }, cond.dens, col=ci, all_data)
        
        do_density_axes(ylab=FALSE)
        
        ksp = sapply(all_data, function(x) {
            format.pval(
                ks.test(x, y='pnorm', mean=mean(x), sd=sd(x))$p.value,
                digits = 2
            )
        })
        
        legend(x=max(axTicks(1)), y = par('usr')[4],
            yjust=1, adj=c(0.5,1), xpd=TRUE, xjust=0.5,
            bty='n', text.col = ci, cex = rave_cex.lab*0.9,
            legend = mapply(function(a,b) paste0(a, ' p = ', b), names(all_data), ksp))
        
        rave_title('Conditional Distribution')
    })

}

assess_stability_over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    fd <- results$get_value('flat_data')
    ed <- results$get_value('epoch_data')
    
    sbd <- results$get_value('scatter_bar_data')
    
    combined <- merge(ed, fd, by.x = 'Trial',
        by.y = 'orig_trial_number')
    
    combined = combined[order(combined$Trial),]
    
    # find the blocks
    by_block <- split(combined, combined$Block)
    nfreq = nlevels(as.factor(by_block[[1]]$freq))
    ylim = range(sapply(by_block, function(b) scale(b$y)))
    
    if(nfreq>1) ylim = ylim + c(0, 4)
    
    plot_clean(combined$Trial, ylim)
    
    block_markers <- which(diff(combined$Time)<0)
    marks <- colMeans(rbind(combined$Trial[block_markers], combined$Trial[block_markers+1]))
    abline(v=marks, col=get_middleground_color(k=0.2), lty=2)
    
    midpoints <- c(0,marks) + diff(c(0, marks, max(combined$Trial)))/2
    
    #draw the spearman correlations
    sprmn <- by_block %>% lapply(function(blk) {
        #  blk <- by_block[[1]]
        shift = c(0, 3.5)[seq_len(nfreq)]
        col = c('gray30', 'gray60')[seq_len(nfreq)]
        
        blk %>% split((.)$freq) %>% mapply(FUN = function(byf, col, shift) {
            sy = scale(byf$y) + shift
            lines(byf$Trial, sy, type='l', col=col)
            points(byf$Trial, sy, pch=16, cex=1, col=blk$group_i)
            cor.test(byf$Trial, byf$y, method='spearman')
        }, col, shift, SIMPLIFY = FALSE)
    })
    
    mapply(function(x,rhos) {
        ps = format.pval(get_list_elements(rhos, 'p.value'), digits = 2)
        rhos = round(get_list_elements(rhos, 'estimate'),2)
        
        for(ii in seq_along(ps)) {
            rho = rhos[ii]
            p = ps[ii]
        text(x, ifelse((ii==2 || nfreq==1), par('usr')[4], par('usr')[3]*.9),
            bquote(rho == .(rho)*','~ p == .(p)), cex = rave_cex.axis, xpd=TRUE)
        }
        
    }, midpoints, sprmn)
    
    # block labels
    rave_axis(1, at=midpoints, tcl=0, lwd=0, labels=paste('block', unique(combined$Block)), mgpx = c(3,1.5,0))
    
    abline(h=0)
    if(nfreq>1) {
        abline(h=0+3.5)
        rave_axis(lwd=0, 2, 0, labels = 'F1')
        rave_axis(lwd=0, 2, 3.5, labels = 'F2')
    }
    
    rave_axis(1, at=c(min(combined$Trial), combined$Trial[block_markers+1], max(combined$Trial)),
        mgpx=c(3,1,0))
    
    # rave_axis(2, at=c(0, range(axTicks(2))))
    abline(v=0)
    
    # rave_axis_labels(ylab=paste0('z(', attr(sbd[[1]]$data, 'ylab'), ')'))
    # rave_axis_labels(xlab='Trial #', line = 3)
    
    rave_title(paste('Scaled Response By Trial Over Time | E', 
        dipsaus::deparse_svec(sbd[[1]]$F1$electrodes, max_lag=1))
    )
    
}
