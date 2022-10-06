# src_data_snapshot <- function() {
#     if(!is.list(local_data$analysis_data_raw)){
#         return(htmltools::div(style='color:#a1a1a1; text-align:center', 'Analysis data not loaded yet.'))
#     }
#     
#     # show snapshot of analysis table
#     tbl_header = local_data$analysis_data_raw$headers
#     tbl_dim = dim(local_data$analysis_data_raw$data)
#     collapsed_data = local_data$collapsed_data
#     
#     str2 = function(val){
#         suppressWarnings({
#             if(is.character(val) || is.factor(val)){
#                 val = as.factor(val)
#                 lv = levels(val);
#                 nlv = length(lv)
#                 if(nlv){ lv = lv[1:min(nlv, 4)] }
#                 str = sprintf(' Factor w/ %d level%s [%s%s]', nlv,
#                     ifelse(nlv>1, 's', ''),
#                     paste(lv, collapse = ', '), ifelse(nlv>4, ', ...', ''))
#             }else if(is.numeric(val)){
#                 str = sprintf(' %s [range: %.4g ~ %.4g]', storage.mode(val),
#                     min(val, na.rm = TRUE), max(val, na.rm = TRUE))
#             }else{
#                 str = utils::capture.output(str(val))
#             }
#             str
#         })
#     }
#     
#     tagList(
#         tags$p(
#             # 1. dimensions
#             'Original analysis table (raw): ', strong(sprintf('%d rows x %d columns', tbl_dim[1], tbl_dim[2])), br(),
#             
#             # 2. columns
#             'Variables: ', strong(paste(tbl_header, collapse = ', ')), br(),
#             
#             hr(),
#             
#             # 3. filtered table
#             'Filtered analysis table (input data): ', strong(sprintf(
#                 '%d rows ', nrow(collapsed_data)
#             )), br()
#         )
#     )
# }

mass_univariate_results <-  function(){
    shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    
    htmltools::div(
        # hr(),
        # h3('Results per electrode'),
        p(
            actionLink(ns('link_clear_show_by_electrode_results'), label = "Clear all selected rows."),
            "Click a row to add it to (or remove it from) the subset plots. Scroll right (", HTML("&rarr;"), ") for more columns."
        ),
        
        DT::dataTableOutput(ns('show_by_electrode_results'))
    )
}

observeEvent(input$link_clear_show_by_electrode_results, {
    dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
    DT::selectRows(dtp, NULL)
    local_data$show_by_electrode_results_rows_selected = NULL
})

power_over_time <- function(lmer_results, collapsed_data, agg_over_trial, analysis_window,
    ylab, ignoreROIs) {
    
    if(missing(lmer_results)) {
        shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
        lmer_results <- model_params$get('lmer_results')
    } 
    
    if(missing(ignoreROIs)) {
        ignoreROIs = !(model_params$get('how_to_model_roi', missing = 'UNK') %in% 
                RAVE_ROI_TYPES[c('ROI_TYPE_I', 'ROI_TYPE_II')])
            # c('Stratify (Random+Fixed)', 'All possible ITX (Random+Fixed)'))
    }
    
    set_palette(local_data$omnibus_plots_color_palette)
    
    collapsed_data %?<-% model_params$get('collapsed_data')
    agg_over_trial %?<-% model_params$get('agg_over_trial')
    analysis_window %?<-% model_params$get('analysis_window')
    ylab %?<-% model_params$get('var_dependent_label_wo_event')
    
    agg_over_trial = agg_over_trial[order(agg_over_trial$Time), ]
    
    vars = attr(terms(lmer_results), 'term.labels')
    # we need to drop TimeWindow and interactions out of this formula
    to_exclude = which(vars == 'TimeWindow' | stringr::str_detect(vars, ':'))
    
    fixed_effects = vars
    if(length(to_exclude) > 0) {
        fixed_effects = vars[-to_exclude]
    }
    
    do_time_series_plot <- function(aot, ignoreROI=TRUE, use_this_range=NULL, axes=c(T,T), decorate=TRUE) {
        by_group = list(aot)
        # need to build a name vector. should be the interaction of all the fixed effects
        names = 'Overall'
        if(ignoreROI) {
            to_rem = which(fixed_effects == 'ROI')
            if(any(to_rem)) {
                fixed_effects = fixed_effects[-to_rem]
            }
        }
        if(length(fixed_effects) > 0) {
            if(length(fixed_effects) == 1) {
                by_group  <- split(aot, list(
                    aot[[fixed_effects]]
                ))
            } else {
                if('data.table' %in% class(aot)){
                    by_group <- split(aot, as.data.frame(aot)[fixed_effects])
                } else {
                    by_group = split(aot, aot[fixed_effects])
                }
            }
            names = levels(interaction(
                as.data.frame(aot)[fixed_effects], drop=TRUE, sep=':'
            ))
        }
        # The sample_sizes will be too large if we have multiple TimeWindows involved. So we need to first 
        # aggregate over TimeWindow.... Maybe we could just get the degrees of freedom from the LME ?
        
        # summary(lmer_results)
        sample_sizes = rep(1, length(by_group))#length(unique(collapsed_data$uuid)) %>%
        #   do_aggregate(as.formula('y ~ 1 ' %?&% fixed_effects %?&%
        #                             paste0(..local_data$var_random_effects, collapse = '+')), function(x)1) %>% split()
        # %$% unlist(y)
        
        lpd <- mapply(function(group, sample_size, name) {
            # group <- by_group[[1]]
            
            # ensure that there are no NA in the se for when we get the range
            # we aren't using the values other than to plot, so setting NA to 0 shouldn't matter here
            # e.g., sample size has already been calculated
            group$se[is.na(group$se)] = 0
            
            # if we're ignoreROI then we need to ensure the data are collapsed appropriately
            # this is not efficient to do here inside the plot...
            if(ignoreROI && !is.null(aot[['ROI']]) && sum(table(aot[['ROI']])>0)>1) {
                group <- group %>% split((.)$Time) %>% lapply(function(tt) {
                    res <- tt[1,]
                    
                    res$m = sum(tt$m * tt$n / sum(tt$n))
                    
                    res$se <- 0
                    res$ROI <- NULL
                    
                    if(!is.null(res[['n']]))  {
                        res$n <- sum(tt$n)
                        res$se <- sqrt((1/(res$n-1)) * sum(
                            ((tt$se*sqrt(tt$n))^2)*(tt$n-1) + c(
                                tt$n * ((tt$m - res$m)^2)
                            )
                        )) / sqrt(res$n)
                    } else {
                        tt$se = mean(tt$se)
                        warning("average SE, assumes equal sample size per group")
                    }
                    
                    return(res)
                }) %>% rbind_list
            }
            
            if(any(is.null(use_this_range))) {
                .range = range(plus_minus(as.matrix(group[,c('m', 'se')])))
            } else {
                .range = use_this_range
            }
            
            res = list(
                x = group$Time,
                data = as.matrix(group[,c('m', 'se')]),
                N = sample_size,
                range = .range,
                has_trials = TRUE,
                name = name
            )
            
            attr(res$data, 'xlab') = 'Time'
            attr(res$data, 'ylab') = pretty_string(ylab)
            res
        }, by_group, sample_sizes, names, SIMPLIFY = FALSE)
        
        time_series_plot(lpd,
            plot_time_range=local_data$omnibus_plots_time_range,
            do_update_ylim = is.null(use_this_range), axes=axes)
        
        axis_label_decorator(lpd)
        if('TimeWindow' %in% vars) {
            tw = tapply(aot$Time, aot$TimeWindow, range)
            tw = tw[which(names(tw) != 'ZZZ___')]
            txt = names(tw)
            if(!decorate) {
                txt = rep_len("", length(names(tw)))
            }
            mapply(window_decorator, tw, text=txt)
        } else {
            txt = 'Analysis Window'
            if(!decorate) txt = ''
            window_decorator(analysis_window, text=txt)
        }
        
        # we can't get the sample size right :(
        legend_decorator(lpd, include = c('name'))#, 'N'))
        
        # label the trial event
        event_label_decorator(1, model_params$get('var_dependent_event'), at = 0)
    }
    
    # determine if we are using ROIs
    if(!ignoreROIs) {
        if(!is.null(agg_over_trial$ROI)) {
            
            common_range = NULL
            if(isTRUE(input$omnibus_plots_use_common_range)) {
                common_range = range(plus_minus(agg_over_trial[,c('m', 'se')]))
            }
            agg_over_trial$ROI %<>% factor
            by_roi = split(agg_over_trial, agg_over_trial$ROI)
            
            nc = min(nlevels(agg_over_trial$ROI), 3)
            nr = ceiling(nlevels(agg_over_trial$ROI)/nc)
            graph_ind = NULL
            
            if(isTRUE(input$omnibus_plots_roi_as_lattice)) {
                if(is.null(common_range)) common_range = range(plus_minus(agg_over_trial[,c('m', 'se')]))
                graph_ind = seq_len((nc + 1) * (nr + 1))
                layout(
                    matrix(graph_ind, nrow = nr + 1, byrow = T),
                    widths = c(lcm(2), rep(4, nc)),
                    heights = c(rep(4, nr), lcm(1))
                )
                current_graph = 1
                for(ii in graph_ind) {
                    if(ii %% (nc+1) == 1) {
                        if(ii < nr*(nc+1)) {
                            par(mar=c(0,2,2,0))
                            plot_clean(0:1, pretty(common_range))
                            rave_axis(2, at = axTicks(2), pos=1)
                            rave_axis_labels(ylab=model_params$get('var_dependent_label_wo_event'), line=0, cex.lab = 1)
                        }  else {
                            par(mar=rep(0,4))
                            plot_clean(0:1, pretty(common_range))
                        }
                    } else if(ii > (nr*(nc+1)) + 1) {
                        par(mar=rep(0,4))
                        plot_clean(local_data$omnibus_plots_time_range, 0:1)
                        rave_axis(1, at=axTicks(1), pos=1)
                    } else {
                        par(mar=c(.5,.5,2,.5))
                        if(current_graph <= length(by_roi)) {
                            do_time_series_plot(by_roi[[current_graph]], axes=c(F,F),
                                ignoreROI = TRUE, use_this_range = common_range,
                                decorate = current_graph == 1)
                            rave_title(as.character(by_roi[[current_graph]]$ROI[1]))
                        } else {
                            rutabaga::plot_clean(pretty(local_data$omnibus_plots_time_range),
                                pretty(common_range))
                        }
                        current_graph = current_graph + 1
                    }
                }
                
            } else {
                par(mfrow=c(nr, nc))
                lapply(by_roi, function(aot) {
                    do_time_series_plot(aot, ignoreROI = TRUE, use_this_range = common_range)
                    rave_title(as.character(aot$ROI[1]))
                })
            }
        } else {
            do_time_series_plot(agg_over_trial)
        }
    } else {
        do_time_series_plot(agg_over_trial)
    }
}

electrode_inspector_time_series <- function() {
    # local_data = ..local_data
    shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    shiny::validate(shiny::need(!is.null(local_data$show_by_electrode_results_rows_selected), message = 'No rows selected'))
    
    fe = attr(terms(model_params$get('lmer_results')), 'term.labels')# paste(, collapse=' * ')
    
    selected = get_selected_subjel()
    otd = model_params$get('over_time_data')
    
    .by = unique(c('Time', 'Subject', 'Electrode', fe))
    .by <- .by[.by %in% names(otd)]
    
    # first we collapse over trial
    aot <- otd[(Subject %in% selected$Subject) & (Electrode %in% selected$Electrode), list(y=mean(y)), by=.by]
    
    # now collapse over Subj:El
    .by <- .by[!.by %in% c('Subject', 'Electrode')]
    aot <- aot[,list(m = mean(y), se=rutabaga:::se(y), n=.N), by = .by]
    
    cd = model_params$get('collapsed_data')
    ind = cd$Subject %in% attr(factor(selected$Subject), 'levels') &
        cd$Electrode %in% attr(factor(selected$Electrode), 'levels')
    cd = cd[ind,]
    
    # power over time will get the rest of its parameters from local_data        
    power_over_time(collapsed_data = cd, agg_over_trial = aot)
    rave_title(attr(selected, 'label'))
}

get_selected_subjel <- function() {
    res = list(lbl='', sbjel_id='')
    
    if(is.null(local_data$show_by_electrode_results_rows_selected)) {
        return(res)
    }
    rows = local_data$show_by_electrode_results_rows_selected
    to_keep = model_params$get('by_electrode_results')[rows,c('Subject', 'Electrode')]
    to_keep$Subject %<>% as.character
    
    to_keep$SubjEl = mapply(paste0, to_keep$Subject, to_keep$Electrode)
    
    attr(to_keep, 'label') = 
        aggregate(Electrode ~ Subject, dipsaus::deparse_svec, data=to_keep) %>%
        apply(1, paste0, collapse=':') %>%
        paste0(collapse=', ')
    
    return (to_keep)
}



custom_windowed_activity <- function(lmer_results, collapsed_data, ...) {
    if( shiny_is_running() ){
        shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    }
    
    
    # print(grDevices::palette())
    # lmer_results = ..local_data$lmer_results
    # collapsed_data = ..local_data$collapsed_data
    
    # model_params <- dbg_model_params
    # local_data <- dbg_local_data
    
    lmer_results %?<-% model_params$get('lmer_results')
    collapsed_data %?<-% model_params$get('collapsed_data')
    
    # shiny::validate(shiny::need(FALSE || !is.null(lmer_results), message = 'I cant'))
    
    set_palette(local_data$omnibus_plots_color_palette)
    .y <- collapsed_data %>% do_aggregate(terms(lmer_results), m_se)
    
    panel_var <- input$custom_plot_panelvar
    gvar <- input$custom_plot_gvar
    if(nchar(gvar) < 1 || gvar=='none') gvar = NULL
    
    xvar <- input$custom_plot_xvar
    
    .ylim <- NULL
    if(input$omnibus_plots_use_common_range) {
        .ylim <- range(pretty(collapsed_data$y))    
    }
    
    if(nchar(input$custom_plot_ylim) > 0) {
        tmp = range(as.numeric(unlist(str_split(input$custom_plot_ylim, "[,|;|:]"))))

        if(all(!is.na(tmp))) {
            if(diff(tmp) == 0) {
                .ylim <- c(-abs(tmp[1]), abs(tmp[1]))
            } else {
                .ylim <- tmp
            }
        } 
    }
    
    # figure out the x-axis label
    xl <- xvar
    if(!is.null(gvar)) {
        xl <- gvar
    }
    
    xlab <- ""
    if(xl == 'ConditionGroup') {
        xlab = model_params$get('first_factor_name')
    } else if (xl == 'ConditionGroup2') {
        xlab = model_params$get('second_factor_name')
    } else if (xl == 'ROI'){
        xlab = xl
    }
    
    leg_title <- if(xvar == 'ConditionGroup'){
        model_params$get('first_factor_name')
    } else if (xvar == 'ConditionGroup2'){
        model_params$get('second_factor_name')
    } else if (xvar == 'ROI') {
        'Region of Interest'
    }
    
    
    plot_features <- unlist(input$custom_plot_types)
    # plot_features <- c('rugs', 'densities', 'ebar polygons')
    # determine if a panel var is being using
    if(panel_var != 'none' && nchar(panel_var) > 0) {
        k <- nlevels(collapsed_data[[panel_var]])
        ncol = min(k, 3)
        nrow = ceiling(k / ncol)
        pln <- matrix(seq_len(ncol*nrow), ncol = ncol, byrow=TRUE)
        layout(pln)
        par(mar=rep(2,4), oma=c(3,3,0,0))
        
        by_panel <- split(drop = TRUE, as.data.frame(collapsed_data),
                          as.data.frame(collapsed_data)[[panel_var]])
        
        #re-level the panel var 
        by_panel %<>% lapply(function(panel) {
            panel[[panel_var]] %<>% factor
            panel
        })
        
        for(ii in seq_len(k)) {
            op <- par(cex = get_cex_for_multifigure()*rave_cex.lab, no.readonly = TRUE)
            plot_grouped_data(by_panel[[ii]], yvar = 'y',
                         ylim = .ylim,
                         type=plot_features,
                         xvar=xvar, gvar=gvar)
            par(op)
            rave_title(levels(collapsed_data[[panel_var]])[[ii]])
            rave_axis(2, at=axTicks(2))
            
            if(ii == 1 && !is.null(gvar)) {
                lvls <- levels(collapsed_data[[xvar]])
                legend('topleft', legend = lvls, text.col = seq_along(lvls),
                       bty = 'n', cex = rave_cex.lab, horiz = TRUE, title.col = par('fg'),
                       title = leg_title)
            }
        }
        mtext(text=model_params$get('var_dependent_label_wo_event'),
              cex=rave_cex.lab, side=2, line=1.5, outer=TRUE)
        mtext(xlab, side=1, line=1, outer=TRUE, cex=rave_cex.lab)
        
    } else {
        op <- par(cex = get_cex_for_multifigure()*rave_cex.lab, no.readonly = TRUE)
        plot_grouped_data(mat = as.data.frame(collapsed_data), yvar = 'y',
                     ylim = .ylim, layout = 'grouped',#input$custom_plot_overlay,
                     type=plot_features,
                     xvar=xvar, gvar=gvar)
        par(op)
        # rave_title(levels(collapsed_data[[panel_var]])[[ii]])
        rave_axis(2, at=axTicks(2))
        
        # check if there is a user supplied name for this variable
        rave_axis_labels(xlab=xlab, ylab=model_params$get('var_dependent_label_wo_event'))
        
        if(!is.null(gvar)) {
            lvls <- levels(collapsed_data[[xvar]])
            legend('topleft', legend = lvls, text.col = seq_along(lvls),
                   bty = 'n', cex = rave_cex.lab, horiz = TRUE, title.col = par('fg'),
                   title = leg_title)
        }
    }
    
}

windowed_activity <- function(lmer_results, collapsed_data) {
    # group_analysis_cat2t('IN WA')
    # print(grDevices::palette())
    # lmer_results = ..local_data$lmer_results
    # collapsed_data = ..local_data$collapsed_data
    
    lmer_results %?<-% model_params$get('lmer_results')
    collapsed_data %?<-% model_params$get('collapsed_data')
    
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))
    
    set_palette(local_data$omnibus_plots_color_palette)
    
    .y <- collapsed_data %>% do_aggregate(terms(lmer_results), m_se)
    
    # .y <- collapsed_data %>% do_aggregate(y ~ TimeWindow*ConditionGroup, m_se)
    
    # plot options
    po = local_data$omnibus_plots_plot_aesthetics
    if(ncol(.y) > 2) {
        # .y <- collapsed_data %>% do_aggregate(y ~ Subject + TimeWindow, m_se)
        yy = matrix(.y$y[,1], ncol = nlevels(factor(.y[,2])), byrow=F)
        rownames(yy) = unique(.y[,1])
        colnames(yy) = unique(.y[,2])
        
        .border <- .col <- NA
        
        if('border' %in% po) .border = seq_len(nrow(yy))
        if('filled' %in% po) .col = adjustcolor(seq_len(nrow(yy)), rave_colors$BAR_PLOT_ALPHA)
        
        .ylim = range(pretty(c(0, plus_minus(.y$y[,1], .y$y[,2]))))
        if(any( c('points', 'jittered points', 'connect points') %in% po)) {
            .ylim = range(pretty(c(0, collapsed_data$y)))
        }
        
        xp <- rutabaga::rave_barplot(yy, axes=F, col = .col,
            border=.border, beside=TRUE, 
            ylim = .ylim,
            xlab=attr(terms(lmer_results), 'term.labels')[2])
        # rave_axis(2, at=axTicks(2))
        # rave_axis_labels(ylab=)
        leg = unique(.y[,1])
        legend(local_data$omnibus_plots_legend_location, legend=leg, text.col=seq_along(leg),
            cex=rave_cex.lab, bty='n', horiz=F, ncol = floor((length(leg)-1) / 3)+1 ) 
        
        ### lots of duplication here, needs to be cleaned up with code in the }else{ block below
        jit_len = mean(diff(xp))*.33
        if(is.nan((jit_len))) jit_len = .33
        
        pts = collapsed_data %>% do_aggregate(terms(lmer_results), list)
        set.seed(local_data$jitter_seed)
        
        if(!('jittered points' %in% po)) jit_len = 0
        
        #note that here we're relying on xp being a matrix,
        # but indexed based on a single numeric running from 1 to length(xp) rather than nrow(xp)
        xlocs = lapply(seq_along(pts$y), function(ii) {
            runif(length(pts$y[[ii]]), xp[ii] - jit_len, xp[ii] + jit_len)
        })
        
        
        #lines first, then points on top of lines
        if('connect points' %in% po) {
            np = length(pts$y[[1]])
            
            for(ni in seq_len(np)) {
                ..x = sapply(xlocs, `[`, ni)
                ..y = sapply(pts$y, `[`, ni)
                lines(..x, ..y, lwd=.5, col=adjustcolor('black', rave_colors$BAR_PLOT_ALPHA))
            }
        }
        
        if(any(c('points', 'jittered points') %in% po)) {
            for(ii in seq_len(length(xlocs))) {
                # we want to recycle the colors based on the number of rows in the result (nlevels of legend factor)
                
                points(x=xlocs[[ii]],
                    y=pts$y[[ii]], col=adjustcolor(1 + ((ii-1) %% nrow(yy)),
                        rave_colors$BAR_PLOT_ALPHA), pch=16)
            }
        }
        
        
        ebars(xp, .y$y, col=seq_len(nrow(yy)), code=0, lwd=2, lend=0)
        
        
        
        
        
    } else {
        nms = .y[,1]
        if(ncol(.y) == 1) nms = 'Overall'
        
        .border <- .col <- NA
        
        if('border' %in% po) .border = seq_len(nrow(.y))
        if('filled' %in% po) .col = adjustcolor(seq_len(nrow(.y)), 0.7)
        
        .ylim = range(pretty(c(0, plus_minus(.y$y[,1], .y$y[,2]))))
        if(any( c('points', 'jittered points', 'connect points') %in% po)) {
            .ylim = range(pretty(c(0, collapsed_data$y)))
        }
        
        xp <- rutabaga::rave_barplot(
            .y$y[,1],
            ylim = .ylim,
            names.arg=nms, cex.names = ifelse('pdf' == names(dev.cur()), 1, rutabaga:::rave_cex.lab),
            axes=F, col = .col, border=.border,
            xlab=attr(terms(lmer_results), 'term.labels'))
        
        jit_len = mean(diff(xp))*.33
        if(is.nan((jit_len))) jit_len = .33
        
        pts = collapsed_data %>% do_aggregate(terms(lmer_results), list)
        set.seed(local_data$jitter_seed)
        
        if(!('jittered points' %in% po)) jit_len = 0
        
        xlocs = lapply(seq_along(pts$y), function(ii) {
            runif(length(pts$y[[ii]]), xp[ii,] - jit_len, xp[ii,] + jit_len)
        })
        
        if(any(c('points', 'jittered points') %in% po)) {
            for(ii in seq_len(nrow(xp))) {
                points(x=xlocs[[ii]],
                    y=pts$y[[ii]], col=adjustcolor(ii,175/255), pch=16)
            }
        }
        if('connect points' %in% po) {
            np = length(pts$y[[1]])
            
            for(ni in seq_len(np)) {
                ..x = sapply(xlocs, `[`, ni)
                ..y = sapply(pts$y, `[`, ni)
                lines(..x, ..y, lwd=.5)
            }
        }
        
        # no legend needed
        ebars(xp, .y$y, col=1:nrow(.y), code=0, lwd=2, lend=0)
        
        if('show means' %in% po) {
            segments(xp[,1] - jit_len*.75, x1 = xp[,1] + jit_len*.75, y0=.y$y[,1], col=1:nrow(.y), lwd=4, lend=1)
        }
        
        if('connect means' %in% po) {
            lines(xp[,1], .y$y[,1], col='gray30', type='o', lwd=2, pch=16)
        }
    }
    
    rave_axis_labels(ylab=model_params$get('var_dependent_label_wo_event'))
    rave_axis(2, at=axTicks(2))
    
    abline(h=0, col=rave_colors$TRIAL_TYPE_SEPARATOR)
}


# electrode_inspector_barplot <- function() {
#   shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
#   shiny::validate(shiny::need(!is.null(local_data$show_by_electrode_results_rows_selected), message = 'No rows selected'))
#   # local_data = ..local_data
#     selected = get_selected_subjel()
#     cd = local_data$collapsed_data
#     
#     cd$subjel <- mapply(paste0, cd$Subject, as.numeric(cd$Electrode))
#     selected$subjel <- mapply(paste0, selected$Subject, as.numeric(selected$Electrode))
# 
#     ind = cd$subjel %in% selected$subjel    
# 
#     cd = cd[ind,]
#     
#     windowed_activity(collapsed_data = cd)
#     rave_title(attr(selected, 'label'))
# }

get_cbar_height <- function() {
    PX_HEIGHT_COLOR_BAR <- 55
    
    return(PX_HEIGHT_COLOR_BAR)
}

electrode_inspector_trial_heat_map_plot <- function(...) {
    
    # local_data = dbg_local_data
    # model_params = dbg_model_params
    # local_data$show_by_electrode_results_rows_selected = 1
    shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    shiny::validate(shiny::need(!is.null(local_data$show_by_electrode_results_rows_selected), message = 'No rows selected'))
    
    set_palette(local_data$omnibus_plots_color_palette)
    
    #base decorator
    po = build_plot_options(sort_trials_by_type='Condition')
    decorator <- by_trial_heat_map_decorator(plot_options = po$as_list(),
        title_options=list(allow_sid=TRUE, allow_enum=F, allow_cond=F, allow_freq=F))
    
    lmer_results <- model_params$get('lmer_results')
    
    vars = attr(terms(lmer_results), 'term.labels')
    # we need to drop TimeWindow and interactions out of this formula
    to_exclude = which(vars == 'TimeWindow' | stringr::str_detect(vars, ':'))
    
    fixed_effects = vars
    if(length(to_exclude) > 0) {
        fixed_effects = vars[-to_exclude]
    }
    
    # add a decorator that can draw the trial labels
    decorator %<>% add_decorator(trial_type_boundaries_hm_decorator(
        try_to_label_groups = 'ConditionGroup' %in% fixed_effects,
        label_colors = get_palette(local_data$omnibus_plots_color_palette))
    )
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    # sel <- list(Subject = c('YAD', 'YAK'), Electrode = c(14, 95))
    sel <- get_selected_subjel()
    ptr = local_data$omnibus_plots_time_range
    ptr %?<-% range(otd$Time)
    
    # do an initial subset because the SubjEl work later is time consuming
    # otd <- model_params$get('over_time_data') %>%
    #     subset((.)$Subject %in% unique(sel$Subject) &
    #                (.)$Electrode %in% unique(sel$Electrode) &
    #                !is.na((.)$y))
    # otd$subjel <- paste0(otd$Subject, otd$Electrode)
    
    otd <- ((model_params$get('over_time_data')[Subject %in% unique(sel$Subject)])[Electrode %in% unique(sel$Electrode)])
    # this is the slowest, do it when the data are smallest
    otd <- (otd[!is.na(y)])[Time %within% ptr]
    otd$subjel <- paste0(otd$Subject, otd$Electrode)
    otd <- otd[subjel %in% paste0(sel$Subject, sel$Electrode)]
    
    # are there fixed effects?
    if('ConditionGroup' %in% fixed_effects) {
        otd$Condition %<>% factor(levels=
                unlist(sapply(model_params$get('ConditionGroups'), `[[`, 'group_conditions'))
        )
    } else {
        otd$Condition %<>% factor(levels = sort(unique(otd$Condition)))
    }
    
    # otd %<>% subset((.)$subjel %in% sel$SubjEl & (.)$Time %within% ptr)
    
    otd_agg <- if("ConditionGroup" %in% names(otd)) {
        do_aggregate(otd, y ~ Trial+Condition+ConditionGroup, mean)
    }else {
        do_aggregate(otd$Condition, y ~ Trial+Condition, mean)
    }
    
    cond_tbl <- table(otd_agg$Condition)
    
        # aggregate(data=otd, y ~ Trial+Condition + Time, mean, na.rm=TRUE) %>%
        # split((.)$Condition) %>% lapply(function(cnd) {
        #     rbind_list(lapply(split(cnd,cnd$Trial), `[[`, 'y'))
        # }) %>% rbind_list
    collapsed <- otd[,list(y=mean(y)),keyby=list(Trial, Time, Condition)] %>%
        split((.)$Condition) %>% lapply(function(cnd) {
            rbind_list(lapply(split(cnd,cnd$Trial), `[[`, 'y'))
        }) %>% rbind_list
    
        # the reshape version is nearly 2x slower
        # as.matrix(reshape(tmp, direction='wide', idvar = c('Condition', 'Trial'), timevar = 'Time')[,-(1:2)])
    
    attr(collapsed, 'ylab') = 'trials'
    attr(collapsed, 'xlab') = 'Time (s)'
    attr(collapsed, 'zlab') = model_params$get('var_dependent_label_wo_event')
    attr(collapsed, 'dimnames') = list(NULL, NULL)
    .t <- sort(unique(otd$Time))
    
    hmaps = list(
        list(data=t(collapsed),
            subject_code = attr(sel, 'label'),
            has_trials=TRUE,
            name ='nm',
            range = range(collapsed),
            x=.t[.t %within% ptr],
            trial_alignment=model_params$get('var_dependent_event'),
            Trial_num = otd_agg$Trial,
            conditions = levels(otd$Condition),
            ConditionGroup = otd_agg$ConditionGroup,
            trials = otd_agg$Condition,
            y=seq_len(nrow(collapsed)),
            analysis_window = model_params$get('analysis_window'),
            events = data.frame(Condition=as.character(otd_agg$Condition))
        ))
    
    hmaps[[1]] %<>% reorder_trials_by_event(event_name='Condition')
    
    decorator %<>% add_decorator(event_label_decorator(event_label = guess_analysis_event()))
    
    draw_many_heat_maps(hmaps = hmaps,
        max_zlim = 95,
        log_scale=FALSE,
        percentile_range=T,
        wide = TRUE,
        PANEL.LAST=decorator,
        PANEL.COLOR_BAR = color_bar_title_decorator,
        plot_time_range = ptr,
        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
        axes=c(TRUE, FALSE))
}

guess_analysis_event <- function() {
    res <- ''
    if(!is.null(local_data$lmer_results)) {
        nms <- paste0(ugly_string(get_unit_of_analysis(names=T)), '_')
        nms %<>% c('z_score_Power_', 'z_score_Amplitude')
        repl = rep_len('', length(nms))
        names(repl) = nms
        
        res <- stringr::str_replace_all(model_params$get('var_dependent'), pattern=repl)
        
        res %<>% pretty_string
        res %<>% trimws
    }
    return (res)
}

get_effect_overview_dimensions <- function() {
    if(!model_params$has('by_electrode_results')) return (0)
    
    ber <- model_params$get('by_electrode_results')
    
    
    # ncol is total condition 
    nc <- 2 + sum(startsWith(colnames(ber), 'm('))
    
    nr <- nrow(ber)
    
    if(isTRUE(input$pes_group_by_roi)) {
        nr = nr + nlevels(ber$ROI)
    }
    
    return(c(nr,nc))
}

estimate_height_of_eo_plot <- function(components=FALSE) {
    cbar <- get_cbar_height()
    eod_h <- 20 * (get_effect_overview_dimensions()[1])
    
    if(!components) return (cbar + 200 + eod_h)
    
    return(list(
        'cbar' = cbar,
        'plus' = 200,
        'eod' = eod_h
    ))
}

effect_overview_plot_ui <- function() {
    
    shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    
    lmer_results = model_params$get('lmer_results')
    
    htmltools::div(
        htmltools::p(HTML("Single-click the data in a row (not the label) to view only that electrode in the subset plots.",
            "Double-click to add the electrode to the plots.<br/>",
            "Clicking an ROI row adds all electrodes within that ROI.")),
        
        shiny::plotOutput(ns('effect_overview_plot'),
            click = clickOpts(shiny::NS('group_analysis_lme')('effect_overview_plot_click'), clip = TRUE),
            dblclick = clickOpts(shiny::NS('group_analysis_lme')('effect_overview_plot_dblclick'), clip = TRUE),
            height = paste0(estimate_height_of_eo_plot(), 'px'))
    )
}

output$effect_overview_plot <- shiny::renderPlot({
    shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
    # 
    # input <- list(
    #     omnibus_plots_color_palette = 'Beautiful Field',
    #     pes_means_heatmap_palette = 'BlueWhiteRed',
    #     pes_contrasts_heatmap_palette = 'PurpleWhiteGreen',
    #     pes_group_by_roi = FALSE
    # )
    
    # model_params <- dbg_model_params
    
    po <- build_plot_options(color_palette=input$omnibus_plots_color_palette,
        heatmap_color_palette = input$pes_means_heatmap_palette)
    
    set_palette_helper(plot_options = po$as_list())
    
    ber = model_params$get('by_electrode_results')
    # ber$SubjEl = mapply(ber$Subject, ber$Electrode, FUN=paste0)
    
    ber = ber[,which(colnames(ber) %in% c('ROI', 'Subject', 'Electrode') | startsWith(colnames(ber), 'm('))]
    
    # Note: we'll need to separate the data into 2 pieces, the means and then the contrasts
    
    # we also need to get the condition means <-- we've already got one 
    # sub_d <- model_params$get('over_time_data')
    # sub_d$Condition %<>% factor
    # names(sub_d)
    
    # is there an ROI var?
    roi_var_name = ifelse('ROI' %in% names(ber), " + ROI", "")
    
    #TODO this is really slow... figure a way to make it faster -- we could do this calculation in by_electrode? 
    # df <- sub_d %>% subset((.)$Time %within% c(0,0.55)) %>% 
    #   do_aggregate(as.formula(sprintf("%s ~ Subject + Electrode + Condition %s",
    #   'y', roi_var_name)), FUN=mean)
    
    # add the means for the ROIs
    
    # df$SubjEl <- mapply(paste0, df$Subject, df$Electrode)
    # stopifnot(all(duplicated(table(df$Condition))[-1]))
    
    # df = df[,-which(colnames(df) %in% c('Subject', 'Electrode'))]
    
    if (!('ROI' %in% names(ber))) {
        ber$ROI = "    "
    }
    
    # wide_df <- reshape(df, direction='wide',
    #                    idvar = c('Subject', 'Electrode', 'SubjEl', 'ROI'),
    #                    timevar = 'Condition')
    # colnames(wide_df) %<>% str_remove_all('y\\.')
    
    # full_mat = merge(data.frame(wide_df), data.frame(m, check.names = FALSE))
    
    e_width <- ceiling(max(log10(as.numeric(ber$Electrode))))
    s_width <- max(nchar(ber$Subject))
    
    ber$SubjEl <- mapply(function(s,e) {
        paste(formatC(s, width=s_width, flag='-'),
            formatC(as.integer(e), width=e_width))
    }, ber$Subject, ber$Electrode)
    
    # re-order the matrix so it's easier to parse
    ber = ber[order(ber$ROI, ber$Subject, as.numeric(ber$Electrode)), ]
    
    ##TODO need to take into account ConditionGroup in the order of the columns
    
    # we also want to drop Electrode column
    rem <- names(ber) %in% c('Subject', 'ROI', "Electrode", "SubjEl")
    combined <- ber[,!rem]
    
    # need to add the pad into the matrix?
    .ROIS = as.character(unique(ber$ROI))
    nr = nrow(combined) + length(.ROIS)
    just_numeric <- matrix(0, ncol=ncol(combined), nrow = nr,
        dimnames = list(paste(seq_len(nr))))
    
    
    nchar_roi = max(nchar(.ROIS))
    qq = 1; rr = 1
    for(ii in 1:nrow(just_numeric)) {
        if(ii == 1) {
            rownames(just_numeric)[ii] = formatC(.ROIS[rr],
                width = nchar_roi, flag = '-') %&% "  "
        } else {
            if(qq == 1) {
                just_numeric[ii,] = unlist(combined[qq,])
                rownames(just_numeric)[ii] = ber$SubjEl[qq]
                qq = qq+1
            } else {
                if(isTRUE(ber$ROI[qq] == .ROIS[rr])) {
                    just_numeric[ii,] = unlist(combined[qq,])
                    rownames(just_numeric)[ii] = ber$SubjEl[qq]
                    qq = qq+1
                } else {
                    rr = rr + 1
                    rownames(just_numeric)[ii] = formatC(.ROIS[rr], 
                        width = nchar_roi, flag = '-') %&% "  "
                }
            }
        }
    }
    # conn='~/Desktop/per_electrode_overview.pdf'
    colnames(just_numeric)= colnames(combined)
    # clean up the column names
    # colnames(just_numeric) = str_remove_all(colnames(just_numeric),
    #                                         paste0(model_params$var_dependent, ".")
    # )
    .h <- nrow(just_numeric) - which(apply(abs(just_numeric), 1, sum)< 1e-16) + 1.5
    contrasts_only = which(str_detect(colnames(just_numeric), ' - ') &
            str_detect(colnames(just_numeric), 'm\\('))
    roi_rows = which(!(rownames(just_numeric) %in% ber$SubjEl))
    # as_pdf(conn, w=10, h=13, {
    
    .h <- estimate_height_of_eo_plot()
    
    # 
    # if(scale_by_condition) {
    #   just_numeric[-roi_rows,] = apply(just_numeric[-roi_rows,], 2, scale)
    # }
    
    if(length(contrasts_only) > 0) {
        jn_wo_contrasts <- just_numeric[,-contrasts_only]
        jn_only_contrasts <- just_numeric[,contrasts_only]
    } else {
        jn_only_contrasts = matrix(0)
        jn_wo_contrasts = just_numeric
    }
    
    #TODO read in from input 
    means_zlim <- calculate_abs_max(jn_wo_contrasts[-roi_rows,],
        requested_max = input$pes_max_for_means,
        percentile_range = input$pes_max_for_means_is_percentile)
    
    contrasts_zlim <- ifelse(is.null(jn_only_contrasts) || length(jn_only_contrasts[-roi_rows,])<1,
        0,
        calculate_abs_max(jn_only_contrasts[-roi_rows,],
            requested_max = input$pes_max_for_contrasts,
            percentile_range = input$pes_max_for_contrasts_is_percentile)
    )
    
    
    cond_to_group <- model_params$get('ConditionGroups')
    raw_names <- str_remove_all(colnames(just_numeric), 'm\\(|\\)$')
    gcond <- sapply(cond_to_group, function(x) unlist(x$group_conditions), USE.NAMES = TRUE, simplify = FALSE)
    cond_cols <- which(raw_names %in% unlist(gcond))
    
    cond_order <-  order(sapply(raw_names[cond_cols],
        function(nm) {
            which(sapply(gcond, `%in%`, x=nm))
        }
    ))
    
    # why is this needed?
    if(max(cond_order) < ncol(just_numeric)) {
        cond_order = c(cond_order, seq(max(cond_order)+1,ncol(just_numeric), by=1))
    }
    
    
    if(contrasts_zlim > 0) {
        layout(matrix(c(3,3,3, 1, 0, 2), nrow=2, byrow = T),
            widths = c(1,.5,1), heights = c(1, lcm(2)))
        .ylab <- model_params$get('var_dependent_label_wo_event') %&% ' (Color bar for conditions)'
    } else {
        layout(matrix(c(2,2,2, 0,1,0), nrow=2, byrow = T),
            widths = c(0.75, 1, 0.75), heights = c(1, lcm(2)))
        .ylab <- model_params$get('var_dependent_label_wo_event')
    }
    # par(family='mono')
    means_heatmap_colors <- expand_heatmap(input$pes_means_heatmap_palette)
    par(cex=0.5)
    rave_color_bar(means_zlim, round(range(jn_wo_contrasts)),
        ylab=.ylab,
        horizontal = TRUE, mar=c(2,3,2,3),
        clrs = means_heatmap_colors)
    
    if(contrasts_zlim > 0) {
        contrasts_heatmap_colors <- expand_heatmap(input$pes_contrasts_heatmap_palette)
        rave_color_bar(contrasts_zlim, round(range(jn_only_contrasts)),
            ylab='Color bar for contrasts',
            horizontal = TRUE, mar=c(2,3,2,3),
            clrs = contrasts_heatmap_colors)
    }
    
    #trying to leave space here for the labels
    par(mai=c(.25, (8/72)*(max(nchar_roi, 10)),
        (7/72) * max(nchar(colnames(just_numeric))),
        0),
        cex=0.75)
    
    make_image(t(just_numeric[,cond_order])[,nrow(just_numeric):1,drop=FALSE],
        x=seq_len(ncol(just_numeric)),
        y=seq_len(nrow(just_numeric)),
        zlim=c(-1,1)*means_zlim, clip_to_zlim = TRUE,
        add = F, useRaster = TRUE, col=means_heatmap_colors)
    par(cex=1)
    
    cnames = colnames(just_numeric)[cond_order]
    if(contrasts_zlim>0) {
        
        # here we are setting the colors for the conditions
        already_plotted <- c()
        for(ii in seq_along(cond_to_group)) {
            mtch <- sprintf('m(%s)', c(cond_group[[ii]]$group_name, cond_group[[ii]]$group_conditions))
            
            ind = which(cnames %in% mtch)
            already_plotted %<>% c(ind)
            
            mtext(cnames[ind], side = 3, at = ind, col=ii, las=2)
            # mtext(cnames[ind], side = 3, at = ind, col=ii, las=2)
        }
        
        #what if we find intercept, then plot from there?
        ind = which(cnames == c('m(Intercept)')) : length(cnames)# | stringr::str_detect(cnames, ' - '))
        ind <- ind[!(ind %in% already_plotted)]
        
        mtext(cnames[ind], side=3, at=ind, col='gray30', las=2)
    } else {
        mtext(colnames(just_numeric[,cond_order]),
            side=3, at=seq_len(ncol(just_numeric)), las=2, col='black')
    }
    par(cex=0.75)
    rave_axis(2, rev(seq_len(nrow(just_numeric)))[-roi_rows],
        lwd=0, tcl=0, las=1, labels = (rownames(just_numeric)[-roi_rows]), cex.axis = 1, font=1)
    
    rave_axis(2, rev(seq_len(nrow(just_numeric)))[roi_rows],
        lwd=0, tcl=0, las=1, labels = (rownames(just_numeric)[roi_rows]), cex.axis = 1, font=2)
    
    # text(x=rep(1, nrow(just_numeric)), y = seq_len(nrow(just_numeric)), labels = rev(rownames(just_numeric)), adj=c(1,.5), xpd=TRUE)
    
    # here we're overplotting the contrasts on a new scale and with a different color bar
    .x = contrasts_only
    if(length(.x)==1) {
        .x = contrasts_only + c(-.5,.5)
    }
    if(length(contrasts_only) > 0) {
        make_image(
            t(just_numeric[,contrasts_only,drop=FALSE])[,nrow(just_numeric):1,drop=FALSE],
            x=.x,
            y=seq_len(nrow(just_numeric)), add = T, useRaster = F,
            col=contrasts_heatmap_colors,
            zlim = c(-1,1)*contrasts_zlim, clip_to_zlim = TRUE
        )
    }
    
    # fill in the ROI labels with white
    sapply(nrow(just_numeric) - roi_rows, function(r) {
        image(x=seq_len(ncol(just_numeric)),
            y=0.5+c(r,r+1),
            z=t(matrix(0, ncol=ncol(just_numeric), nrow=1)),
            add=T, col='white')
        
        text(x=seq_len(ncol(just_numeric)), y = 1+r, labels = '-')
        
    }) %>% invisible
    
    # abline(h=.h[-1], lwd=1, lty=1, xpd=T, col='gray40')
    
    if(length(contrasts_only) > 0) {
        # determine how many conditions there are...
        # there should be a better way, but this is < 1ms and works for up to 10000 variables
        k <- 1 + which(length(contrasts_only) == (2:10000) * (1:9999) / 2)
        
        
        
        abline(v = -0.5 + c(contrasts_only[1] -k, contrasts_only[1]))
        
        # we also want to draw a line before the "intercept" column, basically all the aggregates
        abline( v= -0.5 + which(cnames == 'm(Intercept)'))
    } else{
        abline(v = -0.5 + ncol(just_numeric))
    }
    
    # segments(x0=par('usr')[1], x1=par('usr')[2], y0=4+(-0.5+par('usr')[4]), xpd=TRUE, col='orange')
    
    # plot the NA fields
    par(cex=1)
    if(any(is.na(just_numeric))) {
        na_mat <- which(is.na(just_numeric), arr.ind = TRUE, useNames = F)
        na_mat[,1] = nrow(just_numeric) - na_mat[,1]
        apply(na_mat, 1, function(row) {
            text(row[2], row[1]+1, '.')
        })
    }
    
    # we made it!  
    model_params$set('results_overview_data', just_numeric)
})

output$show_by_electrode_results <- DT::renderDataTable({
    shiny::validate(shiny::need(!is.null(local_data$lmer_results),
        message = "No results to display"))
    
    ber = model_params$get('by_electrode_results')
    
    to_rem = which(names(ber) %in% c('jitter(Subject)'))
    if(any(to_rem)) {
        ber = ber[,-to_rem]
    }
    
    DT::datatable(ber, class = 'nowrap',
        options = list(
            scrollX = TRUE,
            order = list(list(2, 'asc'), list(3, 'desc'))
        ))
})

custom_plot_download_renderers <- function(plot_name, ...) {
    rave_context()
    .__rave_context__. = 'rave_running_local'
    
    FUNS = list(
        'Activity over time' = power_over_time,
        'Mean activity in analysis window' = windowed_activity,
        'Subset time series' = electrode_inspector_time_series,
        'Subset barplot' = electrode_inspector_barplot,
        'Compare post-hoc variables' = post_hoc_plot
    )
    nm = match.arg(plot_name, names(FUNS))
    
    FUNS[[nm]]()
}
custom_plot_download <- custom_plot_download_impl(
    module_id = 'group_analysis_lme',
    choices=c(
        'Activity over time', 'Mean activity in analysis window', 'Subset time series', 
        'Subset barplot', 'Compare post-hoc variables')
)

output$btn_custom_plot_download <- downloadHandler(
    filename=function(...){
        paste0(stringr::str_replace_all(input$custom_plot_select,' ', '_'),
            format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.', input$custom_plot_file_type)
    },
    content = function(conn) {
        pt = input$custom_plot_file_type
        DEV = match.fun(pt)
        args = build_file_output_args(pt, input$custom_plot_width, input$custom_plot_height, conn)
        
        on.exit(dev.off(), add = TRUE)
        do.call(DEV, args = args)
        
        ##### set the margins of the plot.... this might get overriden
        par(mar = c(2.75, 3.5, 2, 1))
        
        custom_plot_download_renderers(input$custom_plot_select)
    })

lmer_diagnosis = function(){
    lmer_results = local_data$lmer_results
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))
    
    plot_clean(1:10, 1:20)
    # pointr(rnorm(10, mean = 10))
    return()
    resid = stats::residuals(lmer_results, type = 'pearson', scaled = TRUE)
    fitt = fitted(lmer_results)
    hat_val = hatvalues(lmer_results)
    tbl = shiny::isolate(local_data$analysis_data_filtered)
    sub = as.factor(tbl$Subject)
    
    nobs = length(resid); n_plot = min(10000, nobs)
    if(nobs > n_plot){
        sel = sample(nobs, n_plot)
        resid = resid[sel]
        fitt = fitt[sel]
        hat_val = hat_val[sel]
        sub = sub[sel]
    }
    pretty2 = function(v, digits = 2){
        c(pretty(v), round(range(v), digits))
    }
    
    graphics::layout(matrix(c(1,1,2,3), 2, byrow = TRUE))
    par(mar = c(4.1, 2.1, 4.1, 1))
    # 1. resid vs fitted
    rutabaga::plot_clean(xlim = fitt, ylim = resid, 
        main = sprintf('Resid vs. Fitted (%d of %d)', n_plot, nobs))
    points(fitt, resid, pch = 20, cex = 0.3)
    rutabaga::ruta_axis(1, pretty(fitt))
    rutabaga::ruta_axis(2, pretty(resid))
    abline(h = 0, col = 'orange3', lty = 2, lwd = 2)
    
    # 2. qqplot
    tmp = sort(rnorm(n_plot))
    rutabaga::plot_clean(xlim = tmp, ylim = resid,
        main = 'Normal Q-Q plot')
    points(tmp, sort(resid), pch = 20, cex = 0.3)
    rutabaga::ruta_axis(1, pretty(tmp))
    rutabaga::ruta_axis(2, pretty(resid))
    abline(a = 0, b = sd(resid)/sd(tmp), col = 'orange3', lty = 2, lwd = 2)
    
    # 3. Boxplot of residual vs subjects
    boxplot(resid ~ sub, axes = FALSE, 
        main = 'BoxPlot of Resid/Subj', cex.main = 1.5, cex.lab = 1.4)
    rutabaga::ruta_axis(2, pretty(resid))
    
    # 4. Boxplot of residuals vs Electrodes
}

download_pes <- function() {
    ber = model_params$get('by_electrode_results')
    
    if(is.null(ber))  return(
        tagList(tags$p('No results available for download...'))
    )
    
    tagList(tags$p(' ', style='margin-top:20px'),
        downloadLink(ns('btn_download_pes'),
            'Download per-electrode statistics'),
        tags$p(' ', style='margin-top:20px'))
}

output$btn_download_pes <- downloadHandler(
    filename=function(...) {
        paste0('per_electrode_statistics_',
            format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.pdf')
    },
    content = function(conn) {
        data.table::fwrite(model_params$get('by_electrode_results'),
            file = conn, row.names = FALSE)
    }
)

download_all_results <- function() {
    lmer_results = local_data$lmer_results
    
    if(is.null(lmer_results))  return()
    
    tagList(tags$p(' ', style='margin-top:20px'),
        downloadLink(ns('btn_download_all_results'),
            'Download All Results'),
        tags$p(' ', style='margin-top:20px'))
}

hide_everything_but_post_hoc_plot <- function() {
    tagList(tags$p(' ', style='margin-top:5px'),
        actionLink(ns('btn_hide_everything_but_post_hoc_plot'),
            'Minimize other inputs/outputs'),
        tags$p(' ', style='margin-bottom:15px')
    )
}

observeEvent(input$btn_hide_everything_but_post_hoc_plot, {
    
    nms <- c("Data import", "Build condition groups", "Single time window analysis", 
        "Multiple time window analysis", "Build model", 'Configure group plots', 'Download per-electrode statistics', 'Export hi-res plot', 
        'Electrode-level results overview', 'Electrode-level results on template brain', 'Electrode-level results table',
        'Electrode subset activity by-trial', 'Electrode subset activity over time', 'Group-level activity over time', 'Group-level mean activity in analysis window',
        'Overall results', 'Full model output', 'Contrast results'
    )
    lapply(nms, rave::close_tab, module_id = 'group_analysis_lme')
    
    nms <- c('Compare post-hoc variables')
    lapply(nms, rave::open_tab, module_id = 'group_analysis_lme')
    
})

output$btn_download_all_results <- downloadHandler(
    filename=function(...) {
        paste0('results_output_',
            format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.zip')
    },
    content = function(conn) {
        tmp_dir = tempdir()
        wrp <- function(nm) sprintf('across_electrodes_%s.csv', nm)
        wcsv <- function(v, f, rn=T) write.csv(v, file=file.path(tmp_dir, f), row.names=rn)
        
        f1 <- 'by_electrode_results.csv'
        wcsv(model_params$get('by_electrode_results'), f1, rn=FALSE)
        fnames <- c(f1,
            wrp("lmer_coefficients"), 
            wrp('lmer_omnibus'),
            wrp('compare_conditions_to_0'),
            wrp('pairwise_comparisons')#, 
            # 'windowed_activity.pdf',
            # 'power_over_time.pdf'
            )
        
        wcsv(model_params$get("lmer_summary_coefficients"), fnames[2])
        
        wcsv(model_params$get("anova_summary"), fnames[3])
        
        wcsv(model_params$get("test_conditions"), fnames[4])
        wcsv(model_params$get("compare_conditions"), fnames[5])
        
        #trying to set the width of the plot based on the number of groups in the data
        # H=6
        # WFAC = 8/H
        # .w = WFAC * nlevels(model_params$get('collapsed_data')$Group)
        # as_pdf(file.path(tmp_dir, fnames[6]), w=.w, h=H, {
        #     par(mar=.1 + c(5,4,1,1))
        #     # call_with_local_data(windowed_activity)
        #     windowed_activity()
        # })
        # 
        # as_pdf(file.path(tmp_dir, fnames[7]), w=8, h=4, {
        #     par(mar=.1 + c(5,4,1,1))
        #     # call_with_local_data(power_over_time)
        #     power_over_time()
        # })
        
        
        wd = getwd()
        on.exit({setwd(wd)}, add = TRUE)
        
        setwd(tmp_dir)
        
        zip(conn, fnames, flags='-r5X')
    }
)

output$download_3dv_colobar <- downloadHandler(
    filename = function(...) {
        'group_analysis_lme_colorbar.pdf'
    }, content = function(conn) {
        
        if(is.null(local_data$by_electrode_results)) {
            showNotification('No data calculated yet, returning default color bar')
            
            dd = 'Value'
            vals = -10:10
            data_range = range(vals)
            
        } else {
            ber = local_data$by_electrode_results
            dd = brain_proxy$controllers[['Display Data']]
            vals = ber[[dd]]
            
            dr = brain_proxy$controllers[['Display Range']]
            dr_tokens = stringr::str_split(dr, ',')[[1]]
            dr_tokens %<>% as.numeric
            
            if(all(is.na(dr_tokens))) {
                data_range = c(-1,1) * ceiling(max(abs(vals)))
            } else if(length(dr_tokens) == 1) {
                data_range = c(-1,1)*abs(dr_tokens[1])
            } else {
                data_range = range(dr_tokens)
            }
        }
        
        if(startsWith(dd, 'p(')) {
            pal = .__lme_color_palette$pval_pal
            
        } else {
            pal = .__lme_color_palette$pal
        }
        as_pdf(conn, w=.4, h=3/2, {
            par('mar' = c(.75,.75,.75,0.25))
            image(matrix(seq_along(pal), nrow=1), col=pal, axes=F, useRaster = TRUE)
            ruta_axis(1, at=0, labels = min(data_range), lwd=0, cex.axis = .65, mgpx=c(0,-.2,0))
            ruta_axis(3, at=0, labels = max(data_range), lwd=0, mgpx=c(0,.1,0), cex.axis=.65)
            # go right to the drawing function...
            rave_axis_labels(ylab=dd, cex.lab=.65, line=.1)
        })
    }
)

.__lme_color_palette <- list(
    pname = 'BlueWhiteRed',
    ncolor = 128,
    
    pal = expand_heatmap(get_heatmap_palette('BlueWhiteRed'), ncolors=128),
    
    pval_pal = expand_heatmap(
        rev(tail(get_heatmap_palette('BlueWhiteRed'),
            ceiling(length(get_heatmap_palette('BlueWhiteRed'))/2))),
        ncolors=128, bias=10)
)


# 3D viewer, takes 3 args
lme_3dviewer_fun <- function(need_calc, side_width, daemon_env, proxy, ...){
    shiny::validate(shiny::need(!is.null(local_data$lmer_results),
        message = 'Please run LMER model first'))
    
    by_electrode_results = model_params$get('by_electrode_results')
    
    to_rem = which(names(by_electrode_results) %in% c('jitter(Subject)'))
    
    if(any(to_rem)) {
        by_electrode_results = by_electrode_results[,-to_rem]
    }
    
    #make sure all the pvalues are numeric
    by_electrode_results[startsWith(names(by_electrode_results), 'p(')] %<>% lapply(as.numeric)
    
    
    # if there are any "currently selected" electrodes, we need to create a column indicating such
    if(!is.null(local_data$show_by_electrode_results_rows_selected)){
        sel <- get_selected_subjel()
        
        print(dput(sel))
    }
    
    # load brain
    brains = lapply(unique(by_electrode_results$Subject), function(sub){
        tryCatch({
            rave::rave_brain2(sprintf('%s/%s', by_electrode_results$Project[1], sub), usetemplateifmissing = TRUE)
        }, error = function(e){ NULL })
    })
    
    brains = dipsaus::drop_nulls(brains)
    brain = threeBrain::merge_brain(.list = brains, template_surface_types = c('pial', 'inf_200'))
    
    # set_palette()
    # nms <- sapply(names(by_electrode_results)[startsWith(names(by_electrode_results),
    #                                               c('m(', 't(', 'p('))
    # ]
    nms <- names(by_electrode_results)[grepl("^[m(|t(|p(|F(]", names(by_electrode_results))]
    val_ranges = sapply(nms, function(d) {
        if (startsWith(d, 'p('))
            return(c(-.2, .2))
        
        c(-1, 1) * ceiling(max(abs(by_electrode_results[[d]])))
    }, simplify = FALSE, USE.NAMES = TRUE)
    .colors = get_heatmap_palette('BlueWhiteRed')
    pal = expand_heatmap(.colors, ncolors=128)
    pval_pal = expand_heatmap(
        rev(tail(.colors, ceiling(length(.colors)/2))),
        ncolors=128, bias=10)
    pals = list(pal)
    
    pals[seq_along(nms)] = pals
    # names(pals) = fix_name_for_js(names(by_electrode_results))
    names(pals) = nms
    
    pals[startsWith(names(pals), 'p(')] = list(pval_pal)
    pals[names(pals) %in% c('p')] = list(pval_pal)
    
    make_factor = which(sapply(by_electrode_results, is.character))
    if(any(make_factor)) {
        for(ii in make_factor)
            by_electrode_results[[ii]] %<>% factor
    }
    
    brain$set_electrode_values(by_electrode_results)
    
    re = brain$plot(side_width = side_width, val_ranges = val_ranges, palettes = pals,
        side_display = FALSE, control_display=FALSE, timestamp=FALSE)
}

build_custom_var <- function(ber, var_name, var_string) {
    vals = eval_in_dataframe(ber, var_string)
    
    vals %?<-% runif(nrow(ber))
    
    ber[[var_name]] = vals
    return(ber)
}


# this function does not work with full generality...
# It works by string matching, so "simple" variable names in your dataframe may conflict with 
# function names you're using. Be careful. all instances of names(df) in your var_string are going to be str_replace'd
# this functions work for our purposes because variables names have parentheses and all manner of special characters that
# reduce the likelihood of a collision
eval_in_dataframe <- function(df, var_string) {
    dipsaus::cat2('eidf::start', 'INFO')
    
    # detect variables inside string and the escape with backticks
    replace_string = names(df)
    replace_string = paste0('`', replace_string, '`')
    names(replace_string) = names(df)
    
    var_string %<>% stringr::str_replace_all(stringr::fixed(replace_string))
    
    if(nchar(var_string) < 1 && shiny_is_running()) {
        showNotification('Custom variable requested, but no text supplied...', type='error')
        return(NULL)
    }
    expr = str2lang(var_string)
    
    vals=NULL
    tryCatch({
        dipsaus::cat2('eidf::eval', level='INFO')
        vals = eval(expr, envir=df)
    }, error=function(e) {
        if(shiny_is_running()) {
            showNotification(e, duration=10)
        }
        warning(e)
    })
    
    return(vals)
}

post_hoc_plot <- function() {
    shiny::validate(shiny::need(model_params$has('by_electrode_results'),
        message = 'No results available'))
    
    ber = model_params$get('by_electrode_results')
    
    # for printing the tables, p-values are converted to string representations, so convert them 
    # back so they can be used in numeric representations
    ber[startsWith(names(ber), 'p(')] %<>% lapply(as.numeric)
    # if either var is a character, factorize it
    
    pal = input$post_hoc_plot_highlight_subject_color_palette
    
    xvar = input$post_hoc_plot_xvar
    if(xvar == 'CUSTOM') {
        xvar = 'CUSTOM_X'
        ber %<>% build_custom_var(xvar, input$post_hoc_plot_xvar_custom)
    }
    
    yvar = input$post_hoc_plot_yvar
    if(yvar == 'CUSTOM') {
        yvar = 'CUSTOM_Y'
        ber %<>% build_custom_var(yvar,input$post_hoc_plot_yvar_custom)
    }
    
    zvar = input$post_hoc_plot_zvar
    if(zvar == 'CUSTOM') {
        zvar = 'CUSTOM_Z'
        ber %<>% build_custom_var(zvar,input$post_hoc_plot_zvar_custom)
    }
    
    get_quantity <- function(nm, val) {
        switch(nm,
            'None' = NA,
            '0' = 0,
            'Mean' = mean(val, na.rm=TRUE),
            quantile(val, probs = as.numeric(str_remove(nm, "%"))/100)
        )
    }
    # choices=c('None', '0', 'Mean', '0%', '25%', '50%', '75%', '100%')))
    ..do_plot <- function(df) {
        if(is.character(df[[xvar]])) {
            df[[xvar]] %<>% factor
            xlim = c(1,nlevels(df[[xvar]]))
        }
        x = df[[xvar]] %>% as.numeric
        if(is.character(df[[yvar]])) {
            df[[yvar]] %<>% factor
            ylim = c(1,nlevels(df[[yvar]]))
        }
        y = df[[yvar]] %>% as.numeric
        
        if(tolower(zvar) != 'none') {
            print('z var is: ' %&% zvar)
            z = df[[zvar]]
            if(!is.null(z)) y = resid(lm(y ~ z))
        }
        
        # One hard part here is that the aspect ratio is hard to get right.
        # We could consider giving a call to layout...
        
        # check if we need to set the plot bounds
        xlim %?<-% text_to_range(input$post_hoc_plot_xlim)
        ylim %?<-% text_to_range(input$post_hoc_plot_ylim)
        
        ###this is lame, but it's fixing a lame problem where the range is compressed too much
        if(nlevels(df$Subject) == 1) {
            if(is.null(xlim) && tolower(xvar) %in% tolower(c('Subject', 'jitter(Subject)'))) {
                xlim %?<-% c(0.568, 1.432)
            }
            if(is.null(ylim) && tolower(yvar) %in% tolower(c('Subject', 'jitter(Subject)'))) {
                ylim %?<-% c(0.568, 1.432)
            }
        }
        
        # if we don't have a range, set it
        xlim %?<-% pretty(x)
        ylim %?<-% pretty(y)
        
        rutabaga::plot_clean(xlim, ylim)
        points(x, y, col=df$..point_color, pch=19)
        
        # maybe should only split when doing separate plots
        if(isTRUE(input$post_hoc_plot_regression_line)) {
            #     df %>% split((.)$..point_color) %>% lapply(function(..df) {
            #       .lm <- lm(..df[[yvar]] ~ ..df[[xvar]])
            #       
            #         try({
            #             abline(.lm, lty = 2, col = ..df$..point_color)
            #         })
            #     })
            try({
                .lm <- lm(y ~ x)
                abline(.lm, lty = 2, col = df$..point_color[1])
            })
        }
        
        correlation <- function(method) {
            nm = 'r = '
            if(tolower(method) == 'spearman') {
                ct = cor.test(x, y, method='spearman')
                nm = 'rho = '
            } else {
                ct = cor.test(x, y)
            }
            if(tolower(method) == 'r2') {
                nm = 'R2 = '
                ct$estimate = ct$estimate^2
            }
            paste0(nm, format(ct$estimate, digits=2), ', p = ', 
                format(ct$p.value, digits=1), collapse='')
        }
        difference_test <- function(method) {
            if(method == 't') {
                res = t.test(y-x)
                nm = 'M(Y-X) = '
            } else {
                res = wilcox.test(y-x)
                res$estimate = median(y-x)
                nm = 'Med(Y-X) = '
            }
            paste0(nm, format(res$estimate, digits=2),
                ', p = ', format(res$p.value, digits=1), collapse='')
        }
        
        post_hoc_stats = sapply(input$post_hoc_plot_show_stats, function(phpss) {
            switch(phpss,
                'Correlation (Pearson)' = correlation(method='pearson'),
                'R2' = correlation(method='R2'),
                'Correlation (Spearman)' = correlation(method='spearman'),
                'Difference test (t)' = difference_test(method='t'),
                'Difference test (Wilcoxon)' = difference_test(method='wilcoxon')
            )  
        })
        
        # sapply(post_hoc_stats, print)
        if(length(post_hoc_stats) > 0) {
            legend(input$post_hoc_plot_legend_location,
                ncol = ifelse(length(post_hoc_stats) > 3, 2, 1),
                legend = post_hoc_stats, bty='n', cex=rave_cex.lab)
        }
        
        if('None' != input$post_hoc_plot_vertical_reference_line) {
            abline(v=get_quantity(input$post_hoc_plot_vertical_reference_line, x), col= rave_colors$DARK_GRAY)
        }
        if('None' != input$post_hoc_plot_horizontal_reference_line) {
            abline(h=get_quantity(input$post_hoc_plot_horizontal_reference_line, y), col= rave_colors$DARK_GRAY)
        }
        if(isTRUE(input$post_hoc_plot_equality_line)) {
            abline(0, 1, col= rave_colors$DARK_GRAY)
        }
        
        ### check if xvar/yvar need to be special-cased
        nsub = nlevels(df$Subject)
        if(tolower(xvar) %in% tolower(c('ROI', 'Subject', 'jitter(Subject)'))) {
            if(tolower(xvar) == 'roi') {
                rave_axis(1, at=unique(as.integer(df$ROI)), labels=levels(df$ROI),
                    lwd = ifelse(nlevels(df$ROI)>1, 1, 0), cex.axis = .5, las=2)
                xvar = 'ROI'
            } else {
                rave_axis(1, at=unique(as.integer(df$Subject)), labels=levels(df$Subject),
                    lwd = ifelse(nsub==1, 0, 1))
                xvar = 'Subject'
            }
            
        } else {
            rave_axis(1, at=axTicks(1))
            if(xvar == 'CUSTOM_X') {
                xvar = input$post_hoc_plot_xvar_custom
            }
        }
        if(yvar %in% c('ROI', 'Subject', 'jitter(Subject)')) {
            if(yvar == 'ROI') {
                rave_axis(2, at=unique(as.integer(df$ROI)), labels=levels(df$ROI),
                    lwd = ifelse(nlevels(df$ROI)>1, 1, 0),
                    cex.axis = 1, las=1)
                yvar = 'ROI'
            } else {
                
                rave_axis(2, at=unique(as.integer(df$Subject)), labels=levels(df$Subject),
                    lwd = ifelse(nsub==1, 0, 1))
                yvar = 'Subject'
            }
        } else {
            if(yvar == 'CUSTOM_Y') {
                yvar = input$post_hoc_plot_yvar_custom
            }
            rave_axis(2, at=axTicks(2))
        }
        
        if(tolower(zvar) != 'none') {
            if(zvar == 'CUSTOM_Z') zvar = input$post_hoc_plot_zvar_custom
            yvar = paste(yvar, '|', zvar)
        }
        
        rave_axis_labels(xlab=xvar,ylab=yvar)
        
        if(isTRUE(input$post_hoc_plot_equality_line)) {
            abline(0,1, col= rave_colors$DARK_GRAY)
        }
        
        # custom horizontal and vertical lines
        mapply(function(str, orient) {
            if(nchar(str) > 0) {
                tryCatch({
                    vals = as.numeric(eval_in_dataframe(ber, str))
                    .args = list(vals)
                    names(.args) = orient
                    do.call(abline, args=.args)
                }, error=function(e) {
                    if(shiny_is_running()) {
                        showNotification('Requested reference line uncalculable: ' %&% str, id = 'REFLINE' %&% orient, type = 'warning')
                    }
                })
                
            }
            
        }, list(
            input$post_hoc_plot_vertical_reference_line_custom, input$post_hoc_plot_horizontal_reference_line_custom
        ), c('v', 'h'))
        
    }
    
    if('Separate Colors' %in% input$post_hoc_plot_highlight_subject) {
        grDevices::palette(get_palette(pal))
        ber$Subject %<>% factor
        ber$..point_color = as.integer(ber$Subject)
    } else {
        grDevices::palette("default")
        ber$..point_color = 1
    }
    
    w = as.numeric(input$post_hoc_plot_width_hint)
    if('Separate Plots' %in% input$post_hoc_plot_highlight_subject) {
        ###setup some kind of layout here
        nc = input$post_hoc_plot_column_count
        if(is.null(nc) || is.na(nc) || nc < 0) nc = 4
        
        nr = ceiling(nlevels(ber$Subject) / nc)
        layout(matrix(seq_len(nc*nr), ncol=nc, byrow = TRUE),
            widths = lcm(rep(w, nlevels(ber$Subject))))
        par(mar=c(5.1, 6.1, 4.1, 2.1))
        ber %>% split((.)$Subject, drop = TRUE) %>% lapply(function(sbj) {
            # because we're splitting by subject, we need to fix the
            # jitter(Subject) variable so it will show up in the right place
            sbj$Subject %<>% factor
            sbj[["jitter(Subject)"]] = jitter(as.integer(sbj$Subject))
            
            ..do_plot(sbj)
            add_strings_to_plot_title(unique(sbj$Subject), unique(sbj$..point_color))
        })
    } else {
        layout(matrix(1, ncol=1), widths = lcm(w))
        ..do_plot(ber)
        add_strings_to_plot_title(unique(ber$Subject), unique(ber$..point_color))
        # plot the subjects in the title
    }
}

lme_diagnosis <- function(){
    shiny::validate(shiny::need(TRUE == FALSE, message = 'Not implemented'))
}
