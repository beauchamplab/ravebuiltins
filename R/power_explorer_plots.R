#' @title Time Series Plot
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
over_time_plot <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)

    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    time_series_plot(plot_data = results$get_value('line_plot_data'),
                     xrange = results$get_value('plot_time_range'),
                     PANEL.FIRST = time_series_decorator(results = results))
}

#' @title Histogram of F-tests per electrode
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
across_electrodes_f_histogram <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    
    validate(need(has_data, message="No Condition Specified"))
    set_palette_helper(results)
    
    omnibus_results <- results$get_value('omnibus_results')
    ts <- omnibus_results[2,]
    par('mar'=.1 + c(5,4,1,2))
    hist(ts, xlab='', ylab='', col='gray50', main='', border=get_foreground_color(),
         las=1, cex.axis=rave_cex.axis*get_cex_for_multifigure(), axes=F)
    rave_axis(1, at=axTicks(1))
    rave_axis(2, at=axTicks(2) %>% round %>% unique)
    rave_axis_labels(xlab='T-test for mean response', ylab='# of Electrodes')
    
    cut <- as.numeric(results$get_value('tval_operand'))
    if(!is_null(cut)) {
        abline(v=cut, lwd=2, col='orangered')
    }
}

#' @title Histogram of per-condition means, per electrode
#' @param results results returned by module
#' @param ... other parameters passed to module output
#' @export
across_electrodes_beta_histogram <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
    omnibus_results <- results$get_value('omnibus_results')
    ms <- omnibus_results[1,]
    par('mar'=.1 + c(5,4,1,2))
    hist(ms, main='', col='gray50', border=get_foreground_color(),
         las=1, axes=F, xlab='', ylab='')
    rave_axis(1, at=axTicks(1))
    rave_axis(2, at=axTicks(2) %>% round %>% unique)
    rave_axis_labels(xlab='Mean Response', ylab='# of Electrodes')
    
    cut <- as.numeric(results$get_value('mean_operand'))
    if(!is_null(cut)) {
        abline(v=cut, lwd=2, col='orangered')
    }
}


rave_axis_labels <- function(xlab=NULL, ylab=NULL, col=NULL, cex.lab=rave_cex.lab, ...) {
    col %?<-% get_foreground_color()

    title(xlab=xlab, ylab=ylab, cex.lab=cex.lab*get_cex_for_multifigure(), col.lab=col, ...)
}


# several functions will need to use this
determine_passing_electrodes <- function(results, ...) {
    res <- results$get_value('omnibus_results')
    v <- c('mean', 'pval', 'tval')
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
        # updateTextInput(getDefaultReactiveDomain(), 'current_active_set', value=deparse_svec(
        #     as.numeric(names(which(pass_the_test)))
        # ))
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

across_electrodes_corrected_pvalue <- function(results, ...) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    set_palette_helper(results)
    par('mar'=.1 + c(5,4,1,2))
    omnibus_results <- results$get_value('omnibus_results')
    ps <- omnibus_results[3,]
    
    filt <- results$get_value('pval_filter')
    pval_funcs <- list('p' = c,
                       'FDR(p)' = function(p) p.adjust(p, method='fdr'),
                       'Bonf(p)' = function(p) p.adjust(p, method='bonferroni'))
    
    ps <- pval_funcs[[filt]](ps)
    nl10 <- function(p) -log10(p)
    # we want to determine the cut point based on the currently selected filters
    # we need to check all the filters, in case they have multiple filters 
    passing_els <- determine_passing_electrodes(results)    
    
    .col <- get_foreground_color()
    cut <- as.numeric(results$get_value('pval_operand'))
    
    plot_clean(1:ncol(omnibus_results),
               ylim=pretty(nl10(c(ps, ifelse(is.null(cut), 0.01, cut)))))
    
    if(!is.null(cut)) {
        segments(x0=0, x1=ncol(omnibus_results), y0=nl10(cut), lty=2, col='orangered')
        rave_axis(4, at=nl10(cut), labels=results$get_value('pval_operand'),
                  tcl=0, cex.axis = 1, lwd=0, mgpy=c(-3, -1, -0))
    }
    
    # get_foreground_color()
    
    points(nl10(ps), pch=16, col=ifelse(passing_els, 'gray10', 'gray70'))
    
    title(xlab='Electrode #', ylab=results$get_value('pval_filter'),
          col.lab = .col, cex.lab=rave_cex.lab*get_cex_for_multifigure(), main = '')
    rave_axis(1, at=seq_along(omnibus_results[2,]), labels=colnames(omnibus_results))
    
    axt <- axTicks(2)
    # rave_axis(2, at=axTicks(2), labels=lapply(lbl, expression) %>% unlist)
    # not sure how to vectorize an expression involving bquote :(
    rave_axis(2, at=axt, labels = F, tcl=0)
    for(ii in seq_along(axt)) {
        rave_axis(2, at=axt[ii], labels=bquote(10**-.(axt[ii])), cex.axis = rave_cex.axis*.9*get_cex_for_multifigure())
    }
    print(omnibus_results[3,])
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
    # pal = c('white', 'black', 'white', 'black')
    
    .alpha <- col2rgb(pal, alpha=TRUE)[4,]
    pal = apply(col2rgb(pal), 2, function(rgb) 255-rgb)
    rgb(t(pal), alpha=.alpha, maxColorValue = 255)
}


#works by side effect to change the palette used by the current graphics device
set_palette_helper <- function(results, ...) {
    .bg <- results$get_value('background_plot_color_hint', 'white')
    
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
        show_outliers = results$get_value('show_outliers_on_plots')
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


by_electrode_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))
    
    set_palette_helper(results)
    
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
        dig = abs(round(log10(max_x)))
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
by_trial_heat_map <- function(results) {
    has_data <- results$get_value('has_data', FALSE)
    validate(need(has_data, message="No Condition Specified"))

    set_palette_helper(results)
    
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
    
    if(results$get_value('show_outliers_on_plots', FALSE)) {
        decorator %<>% add_decorator(heatmap_outlier_highlighter_decorator)
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
