#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    time_series_plot(plot_data = results$get_value('over_time_data'),
                     xrange = results$get_value('plot_time_range'),
                     PANEL.FIRST = time_series_decorator(results = results))
}

draw_cut_point <- function(cut=NULL) {
    if(!is_null(cut) & length(cut)>0) {
        abline(h=cut, lwd=1, col='orangered', lty=2)
    }
    return(cut)
}


#' @title Histogram of F-tests per electrode
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
across_electrodes_f_histogram <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    
    validate(need(has_data, message="No Condition Specified"))
    set_palette_helper(results)
    
    ts <- get_active_result(results)[2,]
    # par('mar'=.1 + c(5,4,1,2))
    ylim = range(pretty(ts))
    plot_clean(seq_along(ts), ylim)
    draw_passing_points(ts, results)
    
    unit_of_analysis=results$get_value('unit_of_analysis')
    rave_axis(2, at=axTicks(2) %>% round %>% unique)
    
    wrtsoe = results$get_value('which_result_to_show_on_electrodes')
    rave_title(paste('t-test', '\n',
                     get_result_name(wrtsoe), unit_of_analysis),
               cex = rave_cex.main*.75)
    
    rave_axis_labels(ylab='t-score')
    across_electrodes_xaxis(vector_to_row_matrix(ts))
    
    cut <- as.numeric(results$get_value('tval_operand'))
    draw_cut_point(cut)
    
    return(invisible(list(
        y =ts,
        ylim=ylim,
        cut=cut
    )))
}

across_electrode_statistics_plot <- function(results, ...) {
    validate(need(results$get_value('has_data', FALSE),
                  message="No Condition Specified"))
    
    # assign('omnibus_results', results$omnibus_results, envir=globalenv())

    wrap_density <- function(expr) {
        og_mar = par('mar')
        on.exit(par(mar=og_mar), add = TRUE)
        with(eval(expr), {
            par(mar=c(og_mar[1], 0, og_mar[3], 1))
            plot_sideways_density(y, ylim, cut)
        })
    }
    
    if(isTRUE(results$get_value('show_result_densities'))) {
        layout(matrix(1:6, nrow=1), widths = rep(c(3.5,1), 3))
    } else {
        layout(matrix(1:3, nrow=1), widths = 1)
        wrap_density = force
    }
    par(mar=c(5.1, 4.1+2, 4.1, 2.1))
    
    
    wrap_density(
        across_electrode_statistics_plot_helper(results, 'mean')
    )
    
    wrap_density(
        across_electrode_statistics_plot_helper(results, 't')
    )
    
    wrap_density(
        across_electrode_statistics_plot_helper(
            results, 'p', 
            TRANSFORM = function(p) {-log10(p)},
            PANEL.LAST = function(yat, ...){
                for(tick in yat) {
                    rave_axis(2, at=tick, labels=bquote(10**-.(tick)), tcl=0,
                              cex.axis = rave_cex.axis*.5*get_cex_for_multifigure())
                }
            }
        )
    )
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


# get_pretty_stat_name <- function(stat_var, results) {
#     available_stats = c('mean', 'tval', 'pval')
#     stat_var = match.arg(stat_var, available_stats)
#     
#     # need to take into account filters
#     results$get_value(stat_var %&% '_filter')
# }


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
                                                     ...) {
    available_stats = c('mean', 't', 'p')
    stat_var <- match.arg(stat_var, available_stats)
    stat_ind = which(stat_var == available_stats)
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No data available"))
    
    set_palette_helper(results)
    
    res = get_active_result(results)[which(stat_var == available_stats),]
    
    # we need to see if there is an a priori transform being requested
    filter_name = results$get_value(stat_var %&% '_filter')
    FILT = make_stat_filter(filter_name)
    res %<>% FILT
    
    # now apply the user supplied transform
    if(is.function(TRANSFORM)) {
        res %<>% TRANSFORM
    }
    
    # if there is a cutpoint, we should force it to be shown on the plot
    # by building it into our ylim calculation
    cut_val = as.numeric(results$get_value(stat_var %&% '_operand'))
    
    ylim = pretty(c(cut_val, res), n=4) %>% pretty_round
    plot_clean(seq_along(res), ylim)
    
    across_electrodes_xaxis(vector_to_row_matrix(res))
    rave_axis_labels(ylab=filter_name)
    draw_passing_points(res, results)
    
    yat = axTicks(2) %>% pretty(n=4) %>% unique
    rave_axis(2, at=yat, labels = show_yaxis_labels)
    
    wrtsoe = results$get_value('which_result_to_show_on_electrodes')
    unit_of_analysis = results$get_value('unit_of_analysis')
    
    rave_title(paste(results$get_value(stat_var %&% '_filter'), '\n',
                     get_result_name(wrtsoe), unit_of_analysis),
               cex = rave_cex.main*.75)
    
    cut <- draw_cut_point(as.numeric(results$get_value(stat_var %&% '_operand')))
    
    if(is.function(PANEL.LAST)) {
        PANEL.LAST(yat=yat, ...)
    }
    
    invisible(list(
        y=res,
        ylim=ylim,
        cut=cut
    ))
}

#' @title Histogram of per-condition means, per electrode
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
across_electrodes_beta_histogram <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    unit_of_analysis = results$get_value('unit_of_analysis')
    # omnibus_results <- results$get_value('omnibus_results')
    ms <- get_active_result(results)[1,]
    
    # par('mar'=.1 + c(5,4,1,2))
    ylim = range(pretty(ms, n=4) %>% pretty_round)
    plot_clean(seq_along(ms), ylim)
    draw_passing_points(ms, results)
    
    across_electrodes_xaxis(vector_to_row_matrix(ms))
    yat = axTicks(2) %>% pretty_round %>% pretty(n=4) %>% unique
    rave_axis(2, at=yat)
    rave_axis_labels(ylab=unit_of_analysis)
    wrtsoe = results$get_value('which_result_to_show_on_electrodes')
    
    # rave_title(sprintf('Mean %s', unit_of_analysis))
    rave_title(paste('Expected Value', '\n',
                     get_result_name(wrtsoe), unit_of_analysis),
               cex = rave_cex.main*.75)
    
    cut <- draw_cut_point(as.numeric(results$get_value('mean_operand')))
    
    return(invisible(list(
        y=ms,
        ylim=ylim,
        cut=cut
    )))
}


rave_axis_labels <- function(xlab=NULL, ylab=NULL, col=NULL, cex.lab=rave_cex.lab, ...) {
    col %?<-% get_foreground_color()
    title(xlab=xlab, ylab=ylab, cex.lab=cex.lab*get_cex_for_multifigure(), col.lab=col, ...)
}

# several functions will need to use this
determine_passing_electrodes <- function(results, ...) {
    res <- results$get_value('omnibus_results')
    v <- c('mean', 'p', 't')
    filters <- sapply(v %&% '_filter', function(e) results$get_value(e))
    operators <- sapply(v %&% '_operator', function(e) results$get_value(e))
    operands <- sapply(v %&% '_operand', function(e) results$get_value(e))
    
    pass_the_test <- rep(TRUE, length(results$get_value('electrodes')))
    
    pval_filters <- c('p', 'FDR(p)', 'Bonf(p)')
    pval_funcs <- list('p' = c,
                       'FDR(p)' = function(p) p.adjust(p, method='fdr'),
                       'Bonf(p)' = function(p) p.adjust(p, method='bonferroni'))
    
    # operands[1] = '50'
    
    for(ii in seq_along(filters)) {
        # first check if there is a valid operand
        if(all(operands[ii] != "", not_NA(as.numeric(operands[ii]))) ) {
            # convert the operator to its corresponding method
            OP <- getMethod(operators[ii])
            # default to p-value
            val = res[3,]
            if(filters[ii] %in% pval_filters) {
                val = pval_funcs[[filters[ii]]](val)
            } else if (filters[ii] == 't') {
                val = res[2,]
            } else if (filters[ii] == 'b0') {
                val = res[1,]                
            }
            pass_the_test = pass_the_test & OP(val,as.numeric(operands[ii]))
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

    # update the local_data variable -- can this be done?
    # local_data$electrodes_passing_the_test <- as.numeric(names(pass_the_test))
    # no. instead let's put this value in a textInput??
    
    if(shiny_is_running()){
        updateTextInput(getDefaultReactiveDomain(), 'current_active_set', 
                        value=deparse_svec(emeta$Electrode[pass_the_test]))
    }
    return(pass_the_test)
}

shiny_is_running <- function() {
    return(shiny::isRunning())
    
    # cls <- class(getDefaultReactiveDomain())
    # any(cls %in% c('ShinySession', 'session_proxy'))
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

draw_passing_points <- function(y, results) {
    passing_els <- determine_passing_electrodes(results)
    points(y, cex=1.1, pch=ifelse(passing_els, 19, 1), col=ifelse(passing_els, get_foreground_color(), 'gray50'))
}

get_active_result <- function(results, ...) {
    res_name = results$get_value('which_result_to_show_on_electrodes')
    if(res_name == "Omnibus Activity (across all active trial types)") {
        ind = 1:3
        if(results$get_value('p_filter')!='p') {
            ind[3] = 4
        }
    } else {
        # print('returning contrast results')
        lbls = build_group_contrast_labels(build_group_names(results$get_value('GROUPS')))
        b = 5 + 3*(which(res_name == lbls)-1)
        ind = b:(b+2)
    }
     
    return (results$get_value('omnibus_results')[ind,])
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

across_electrodes_corrected_pvalue <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    set_palette_helper(results)
    
    ps <- get_active_result(results)[3,]
    
    filt <- results$get_value('p_filter')
    pval_funcs <- list('p' = c,
                       'FDR(p)' = function(p) p.adjust(p, method='fdr'),
                       'Bonf(p)' = function(p) p.adjust(p, method='bonferroni'))
    
    
    ps <- pval_funcs[[filt]](ps)
    nl10 <- function(p) -log10(p)
    # we want to determine the cut point based on the currently selected filters
    # we need to check all the filters, in case they have multiple filters 
    
    .col <- get_foreground_color()
    
    
    cut <- as.numeric(results$get_value('p_operand'))
    ylim = pretty(nl10(c(ps, ifelse(is.null(cut), 0.01, cut))))
    
    plot_clean(seq_along(ps), ylim=ylim)
    
    if(!is.null(cut)) {
        segments(x0=1, x1=length(ps), y0=nl10(cut), lty=2, col='orangered')
        rave_axis(4, at=nl10(cut), labels=results$get_value('p_operand'),
                  tcl=0, cex.axis = 1, lwd=0, mgpy=c(-3, -1, -0))
    }
    # get_foreground_color()
    across_electrodes_xaxis(vector_to_row_matrix(ps))
    draw_passing_points(nl10(ps), results)
    
    title(xlab='Electrode #', ylab=results$get_value('p_filter'),
          col.lab = .col, cex.lab=rave_cex.lab*get_cex_for_multifigure(), main = '')
    
    unit_of_analysis = results$get_value('unit_of_analysis')
    wrtsoe = results$get_value('which_result_to_show_on_electrodes')
    rave_title(paste(filt, '\n', wrtsoe, unit_of_analysis),
               cex = rave_cex.main*.75)
    
    axt <- axTicks(2)
    rave_axis(2, at=axt, labels = F, tcl=0, mgpy=c(3, .75, 0))
    
    # not sure how to vectorize an expression involving bquote :(
    for(ii in seq_along(axt)) {
        rave_axis(2, at=axt[ii], labels=bquote(10**-.(axt[ii])), cex.axis = rave_cex.axis*.5*get_cex_for_multifigure())
    }
    invisible(list(y=nl10(ps), ylim=ylim, cut=nl10(cut)))
}

get_foreground_color <- function() {
    switch(par('bg'),
           'black' = 'white',
           'white' = 'black',
           '#1E1E1E' = 'gray70',
           'gray' = '#A5A5A5'
    ) 
}

invert_palette <- function(pal) {
    inv = c(255, 255, 255, 255) - col2rgb(pal, alpha=TRUE)
    rgb(t(inv), alpha=255, maxColorValue = 255)    
}


#works by side effect to change the palette used by the current graphics device
set_palette_helper <- function(results, ...) {
    rave_context()
    
    .bg <- results$get_value('background_plot_color_hint', 'White')
    # session = shiny::getDefaultReactiveDomain()
    if(.bg %in%  c('white', 'White')) {
        theme = set_rave_theme('light')
    }else{
        theme = set_rave_theme('dark')
    }

    # setting the background color here triggers a cascade of color changes
    if(.bg == 'Gray') {
        par('bg'='#1E1E1E')
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
        show_outliers = results$get_value('show_outliers_on_plots'),
        PANEL.LAST = trial_scatter_plot_decortator(results=results)
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
    rave_context()
    
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    draw_many_heat_maps(hmaps = results$get_value('heat_map_data'),
                        log_scale = results$get_value('log_scale'),
                        max_zlim = results$get_value('max_zlim'),
                        xrange = results$get_value('plot_time_range'),
                        PANEL.LAST = spectrogram_heatmap_decorator(results=results),
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator,0)
    )
}

# create_results_object <- function(...) {
#     env <- new.env(parent = baseenv())
#     local({
#         .current_env = environment()
#         get_value = function(nm, default = NULL){
#             if(exists(nm, envir = .current_env)){
#                 return( .current_env[[nm]] )
#             }else{
#                 default
#             }
#         }
#     }, envir = env)
#     list2env(list(...), envir = env)
#     env
# }


by_electrode_heat_map_plot <- function(results, ...) {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    set_heatmap_palette_helper(results)
    
    by_electrode_heat_map_data <- results$get_value('by_electrode_heat_map_data')
    
    draw_many_heat_maps(by_electrode_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        xrange = results$get_value('plot_time_range'),
                        PANEL.LAST=by_electrode_heat_map_decorator(results=results),
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator, 0)
                        )
}

pretty_round <- function(x) {
    max_x <- max(abs(x))
    dig = 0
    if(max_x < 1) {
        dig = abs(floor(log10(max_x)))
    } 
    round(x, dig)
}

color_bar_title_decorator <- function(m) {
    rave_title(paste0('Range\n[', paste0(pretty_round(get_data_range(m)), collapse = ':'), ']'),
               font=1, cex = rave_cex.lab*.8)
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
# expects by_trial_heat_map_data to exist
by_trial_heat_map_plot <- function(results) {
    rave_context()
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    set_heatmap_palette_helper(results)
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
    
    show_outliers <- results$get_value('show_outliers_on_plots', FALSE)
    if(show_outliers) {
        # print('showing outliers')
        decorator %<>% add_decorator(heatmap_outlier_highlighter_decorator)
    } else {
        # print('not showing outliers, removing them, start with: ' %&% nrow(by_trial_heat_map_data[[1]]$data))
        ##FIXME Is this an ok place to do this?
        for(ii in seq_along(by_trial_heat_map_data)) {
            .clean <- by_trial_heat_map_data[[ii]]$is_clean
            if(sum(.clean) == 0) {
                by_trial_heat_map_data[[ii]]$has_trials <- FALSE
                print('no trials')
            } else {
                # note that $data is perhaps the transpose of what you expect, that's because the image() function requires
                # the transpose of what you might expect to plot what you might expect
                by_trial_heat_map_data[[ii]]$data <- by_trial_heat_map_data[[ii]]$data[,.clean]
                by_trial_heat_map_data[[ii]]$Trial_num <- by_trial_heat_map_data[[ii]]$Trial_num[.clean]
                by_trial_heat_map_data[[ii]]$trials <- by_trial_heat_map_data[[ii]]$trials[.clean]
                by_trial_heat_map_data[[ii]]$range <- .fast_range(c(by_trial_heat_map_data[[ii]]$data))
                by_trial_heat_map_data[[1]]$y <- seq_along(by_trial_heat_map_data[[ii]]$Trial_num)
            }
        }
    }
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data,
                        max_zlim = results$get_value('max_zlim'), log_scale=FALSE,
                        wide = sort_trials_by_type,
                        PANEL.LAST=decorator,
                        PANEL.COLOR_BAR = ifelse(results$get_value('show_heatmap_range', FALSE), color_bar_title_decorator,0),
                        xrange = results$get_value('plot_time_range'),
                        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
                        axes=c(TRUE, !sort_trials_by_type))
}
