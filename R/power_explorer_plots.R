#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    
    
    
    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    decorator <- NULL
    if(results$get_value('show_stimulation_window', FALSE)) {
         decorator <- stimulation_window_decorator
    }
    
    
    # dipsaus::cat2('TSP START')
    
    time_series_plot(plot_data = results$get_value('over_time_data'),
                     plot_time_range = results$get_value('plot_time_range'),
                     PANEL.FIRST = time_series_decorator(plot_options=po),
                     PANEL.LAST = decorator
    )
    
    # dipsaus::cat2('TSP END')
}

#' Draws an orange, dashed horizontal line at cut. Checks for not null and 
#' length > 0
#'
#' @param cut the location(s) of the lines
#'
#' @return the value of cut (invisibly)
#' @export
#'
#' @examples
#' plot(-log10(runif(100)), ylab='p value')
#' draw_cut_point(-log10(0.05))
draw_cut_point <- function(cut=NULL) {
    if(!is.null(cut) & length(cut)>0) {
        abline(h=cut, lwd=1, col='orangered', lty=2)
    }
    invisible(cut)
}

across_electrode_statistics_plot <- function(results, ...) {
    validate(need(results$get_value('has_data', FALSE),
                  message="No Condition Specified"))
    
    # dipsaus::cat2("Starting AESP")
    # assign('omnibus_results', results$omnibus_results, envir=globalenv())

    wrap_density <- function(expr) {
        og_mar = par('mar')
        on.exit(par(mar=og_mar), add = TRUE)
        with(eval(expr), {
            par(mar=c(og_mar[1], 0, og_mar[3], 1))
            plot_sideways_density(y, ylim, cut)
        })
    }
    passing_electrodes <- determine_passing_electrodes(results)
    
    if(isTRUE(results$get_value('show_result_densities')) & length(passing_electrodes) > 1) {
        layout(matrix(1:6, nrow=1), widths = rep(c(3.5,1), 3))
    } else {
        layout(matrix(1:3, nrow=1), widths = 1)
        wrap_density = force
    }
    par(mar=c(5.1, 4.1+2, 4.1, 2.1))
    
    lapply(c('mean', 't'), function(sv) {
        wrap_density(across_electrode_statistics_plot_helper(results, sv,
                                                             passing_electrodes=passing_electrodes))
    })
    
    wrap_density(
        across_electrode_statistics_plot_helper(
            results, 'p', 
            TRANSFORM = function(p) {-log10(p)},
            PANEL.LAST = function(yat, ...){
                for(tick in yat) {
                    rave_axis(2, at=tick, labels=bquote(10**.(-tick)), tcl=0,
                              cex.axis = rave_cex.axis*.5*get_cex_for_multifigure())
                }
            }
        )
    )
    
    # dipsaus::cat2("Done AESP")
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

across_electrode_statistics_plot_helper <- function(results, 
                                                    stat_var,
                                                    TRANSFORM=force,
                                                    PANEL.LAST=NULL, 
                                                    show_yaxis_labels = !is.function(PANEL.LAST),
                                                    passing_electrodes, ...) {
    available_stats = c('mean', 't', 'p')
    stat_var <- match.arg(stat_var, available_stats)
    stat_ind = which(stat_var == available_stats)
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No data available"))
    
    set_palette_helper(results)
    
    res = get_active_result(results)[which(stat_var == available_stats),]
    
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
determine_passing_electrodes <- function(results, ...) {
    ### we need to update this to select the appropriate value!
    # res <- results$get_value('omnibus_results')
    res <- get_active_result(results)
    
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

get_active_result <- function(results, ...) {
    res_name = results$get_value('which_result_to_show_on_electrodes')
    res = results$get_value('omnibus_results')
    begin = 1
    if(res_name != "Omnibus Activity (across all active trial types)") {
        begin = which(endsWith(rownames(res), res_name))[1]
    }
    ### also need to check if rownames of OR match those we just built
    shiny::validate(shiny::need(length(begin) == 1 && !is.na(begin),
                                message = 'Selected data not available. Press Recalculate button'))
    ind = begin:(begin+2)
    
    return (res[ind,,drop=FALSE])
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
#' @export
windowed_comparison_plot <- function(results, ...){
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    # dipsaus::cat2("Starting WCP")
    set_palette_helper(results)
    
    # po = results$get_value('plot_options')
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    ### check if we need to highlight any points
    trial_scatter_plot(
        group_data = results$get_value('scatter_bar_data'),
        show_outliers = results$get_value('show_outliers_on_plots'),
        PANEL.LAST = trial_scatter_plot_decortator(plot_title_options = po$plot_title_options)
    )
    
    # dipsaus::cat2("END WCP")
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
    
    # dipsaus::cat2("Starting HMP")
    

    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    
    # user-controlled heatmap nrow
    ncol <- results$get_value('max_column_heatmap')
    
    decorator <- spectrogram_heatmap_decorator(plot_options = po)
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator %<>% add_decorator(stimulation_window_decorator)
    }
    ignore_time_range = NULL
    if(results$get_value('censor_stimulation_window')) {
        ignore_time_range <- results$get_value('stimulation_window')
    }
    
    draw_many_heat_maps(hmaps = results$get_value('heat_map_data'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = results$get_value('max_zlim', 0),
                        percentile_range=results$get_value('percentile_range'),
                        plot_time_range = results$get_value('plot_time_range'),
                        ignore_time_range = ignore_time_range,
                        PANEL.LAST = decorator,
                        max_columns = ncol,
                        decorate_all_plots = results$get_value('redundant_labels', FALSE),
                        center_multipanel_title = results$get_value('center_multipanel_title'),
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator, 0)
    )
    
    # dipsaus::cat2("Done HMP")
}

heatmap_plot_helper <- function(results, hmap_varname, ...) {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    ncol <- results$get_value('max_column_heatmap')
    
    by_electrode_heat_map_data <- results$get_value(hmap_varname)
    
    draw_many_heat_maps(by_electrode_heat_map_data,
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

by_electrode_heat_map_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    # dipsaus::cat2("Starting BEHMP")
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)

    ncol <- results$get_value('max_column_heatmap')
    
    
    behmd <- results$get_value('by_electrode_heat_map_data')
    
    po <- results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    decorator <- by_electrode_heat_map_decorator(plot_options = po)
    
    if(results$get_value('show_stimulation_window', FALSE)) {
        decorator %<>% add_decorator(stimulation_window_decorator)
    }
    
    ignore_time_range = NULL
    if(results$get_value('censor_stimulation_window')) {
        ignore_time_range <- results$get_value('stimulation_window')
    }
    # dipsaus::cat2("Starting DMHM")
    draw_many_heat_maps(behmd,
                        percentile_range=results$get_value('percentile_range'),
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        plot_time_range = results$get_value('plot_time_range'),
                        PANEL.LAST=decorator,
                        max_columns = ncol,
                        ignore_time_range = ignore_time_range,
                        decorate_all_plots = results$get_value('redundant_labels', FALSE),
                        center_multipanel_title = results$get_value('center_multipanel_title'),
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE),
                                                 color_bar_title_decorator, 0)
                        )
    
    # dipsaus::cat2("Done BEHMP")
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

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
by_trial_heat_map_plot <- function(results, ...) {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    by_trial_heat_map_data <- results$get_value('by_trial_heat_map_data')
    
    
    #base decorator
    po = results$get_value('ravebuiltins_power_explorer_plot_options')$as_list()
    decorator <- by_trial_heat_map_decorator(plot_options = po)
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    sort_trials_by_type <- results$get_value('sort_trials_by_type', 'Trial Number')
    if(sort_trials_by_type != 'Trial Number') {
        for(ii in which(results$get_value('has_trials'))) {
            by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_event(event_name = sort_trials_by_type)
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
        by_trial_heat_map_data %<>% remove_outliers_from_by_trial_data
    }
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    need_wide = ('Condition' == sort_trials_by_type)
    
    # user-controlled heatmap nrow
    ncol <- results$get_value('max_column_heatmap')
    
    draw_many_heat_maps(hmaps = by_trial_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        percentile_range=results$get_value('percentile_range'),
                        wide = need_wide,
                        PANEL.LAST=decorator,
                        ignore_time_range = ignore_time_range,
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator,0),
                        plot_time_range = results$get_value('plot_time_range'),
                        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
                        axes=c(TRUE, !need_wide),
                        center_multipanel_title = results$get_value('center_multipanel_title'),
                        decorate_all_plots = results$get_value('redundant_labels', FALSE),
                        max_columns = ncol)
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
    par(mfrow=c(1,2), mar=c(5,6,4,1))
    # layout(matrix(2,1), widths = lcm(20))
    
    sbd = results$get_value('scatter_bar_data')
    
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
    
    plot_clean(xlim, ylim)
    # title_decorator()
    rave_title(paste('Omni Dist | E', 
                     dipsaus::deparse_svec(sbd[[1]]$electrodes, max_lag=1))
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
    plot_clean(combined$Trial, sapply(by_block, function(b) scale(b$y)))
    
    block_markers <- which(diff(combined$Time)<0)
    marks <- colMeans(rbind(combined$Trial[block_markers], combined$Trial[block_markers+1]))
    abline(v=marks, col=get_middleground_color(k=0.2), lty=2)
    
    midpoints <- c(0,marks) + diff(c(0, marks, max(combined$Trial)))/2
    
    #draw the spearman correlations
    sprmn <- by_block %>% lapply(function(blk) {
        sy = scale(blk$y)
        lines(blk$Trial, sy, type='l')
        points(blk$Trial, sy, pch=16, cex=1.5, col=blk$group_i)
        cor.test(blk$Trial, blk$y, method='spearman')
    })
    mapply(function(x,rho) {
        p = format.pval(rho$p.value, digits = 2)
        rho = round(rho$estimate,2)
        text(x, par('usr')[4], bquote(rho == .(rho)*','~ p == .(p)),
             cex = rave_cex.axis, xpd=TRUE)
    }, midpoints, sprmn)
    
    # block labels
    rave_axis(1, at=midpoints, tcl=0, lwd=0, labels=paste('block', unique(combined$Block)), mgpx = c(3,1.5,0))
    abline(h=0)
    
    rave_axis(1, at=c(min(combined$Trial), combined$Trial[block_markers+1], max(combined$Trial)),
              mgpx=c(3,1,0))
    
    rave_axis(2, at=c(0, range(axTicks(2))))
    
    rave_axis_labels(ylab=paste0('z(', attr(sbd[[1]]$data, 'ylab'), ')'))
    # rave_axis_labels(xlab='Trial #', line = 3)
    
    rave_title(paste('Mean Response By Trial Over Time | E', 
                     dipsaus::deparse_svec(sbd[[1]]$electrodes, max_lag=1))
    )
    
}
