src_data_snapshot <- function() {
    if(!is.list(local_data$analysis_data_raw)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center', 'Analysis data not loaded yet.'))
    }
    
    # show snapshot of analysis table
    tbl_header = local_data$analysis_data_raw$headers
    tbl_dim = dim(local_data$analysis_data_raw$data)
    collapsed_data = local_data$collapsed_data
    
    str2 = function(val){
        suppressWarnings({
            if(is.character(val) || is.factor(val)){
                val = as.factor(val)
                lv = levels(val);
                nlv = length(lv)
                if(nlv){ lv = lv[1:min(nlv, 4)] }
                str = sprintf(' Factor w/ %d level%s [%s%s]', nlv,
                              ifelse(nlv>1, 's', ''),
                              paste(lv, collapse = ', '), ifelse(nlv>4, ', ...', ''))
            }else if(is.numeric(val)){
                str = sprintf(' %s [range: %.4g ~ %.4g]', storage.mode(val),
                              min(val, na.rm = TRUE), max(val, na.rm = TRUE))
            }else{
                str = utils::capture.output(str(val))
            }
            str
        })
    }
    
    tagList(
        tags$p(
            # 1. dimensions
            'Original analysis table (raw): ', strong(sprintf('%d rows x %d columns', tbl_dim[1], tbl_dim[2])), br(),
            
            # 2. columns
            'Variables: ', strong(paste(tbl_header, collapse = ', ')), br(),
            
            hr(),
            
            # 3. filtered table
            'Filtered analysis table (input data): ', strong(sprintf(
                '%d rows ', nrow(collapsed_data)
            )), br()
        )
    )
}

mass_univariate_results <-  function(){
    lmer_results = local_data$lmer_results
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))

    htmltools::div(
        # hr(),
        # h3('Results per electrode'),
        actionLink(ns('link_clear_show_by_electrode_results'), label = "Clear Selected Rows"),
        DT::dataTableOutput(ns('show_by_electrode_results'))
    )
}

observeEvent(input$link_clear_show_by_electrode_results, {
    dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
    DT::selectRows(dtp, NULL)
    local_data$show_by_electrode_results_rows_selected = NULL
})


power_over_time <- function(lmer_results, collapsed_data, agg_over_trial, analysis_window, ylab) {
  # local_data = ..local_data
  lmer_results %?<-% local_data$lmer_results
  shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))
  
  set_palette(local_data$omnibus_plots_color_palette)
  
  collapsed_data %?<-% local_data$collapsed_data
  agg_over_trial %?<-% local_data$agg_over_trial
  analysis_window %?<-% local_data$analysis_window_label
  ylab %?<-% local_data$var_dependent_label
  
  agg_over_trial = agg_over_trial[order(agg_over_trial$Time),]
  
  vars = attr(terms(lmer_results), 'term.labels')
  # we need to drop TimeWindow and interactions out of this formula
  to_exclude = which(vars == 'TimeWindow' | stringr::str_detect(vars, ':'))
  
  fixed_effects = vars
  if(length(to_exclude) > 0) {
    fixed_effects = vars[-to_exclude]
  }
  
  by_group = list(agg_over_trial)
  # need to build a name vector. should be the interaction of all the fixed effects
  names = 'Overall'
  if(length(fixed_effects) > 0) {
    
    if(length(fixed_effects) == 1) {
      by_group  <- split(agg_over_trial, list(
        agg_over_trial[[fixed_effects]]
      ))
    } else {
      by_group = split(agg_over_trial, agg_over_trial[fixed_effects])
    }
    
    names = levels(interaction(
      collapsed_data[fixed_effects]
      , drop=TRUE, sep=':'
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
    # ensure that there are no NA in the se for when we get the range
    # we aren't using the values other than to plot, so setting NA to 0 shouldn't matter here
    # e.g., sample size has already been calculated
    group$y[is.na(group$y[,2]),2] = 0
    
    res = list(
      x = group$Time,
      data = group$y,
      N= sample_size,
      range = range(plus_minus(group$y)),
      has_trials = TRUE,
      name = name
    )
    
    attr(res$data, 'xlab') = 'Time'
    attr(res$data, 'ylab') = pretty_string(ylab)
    res
  }, by_group, sample_sizes, names, SIMPLIFY = FALSE)
  
  time_series_plot(plot_data = lpd, plot_time_range=local_data$omnibus_plots_time_range)
  axis_label_decorator(lpd)
  
  if('TimeWindow' %in% vars) {
    tw = tapply(agg_over_trial$Time, agg_over_trial$TimeWindow, range)
    tw = tw[which(names(tw) != 'ZZZ___')]
    mapply(window_decorator, tw, text=names(tw))
  } else {
    window_decorator(analysis_window, text='Analysis Window')
  }
  
  # we can't get the sample size right :(
  legend_decorator(lpd, include = c('name'))#, 'N'))
  # group_analysis_cat2t('OUT POT')
}

electrode_inspector_time_series <- function() {
  shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
  shiny::validate(shiny::need(!is.null(local_data$show_by_electrode_results_rows_selected), message = 'No rows selected'))
  
  selected = get_selected_subjel()
  otd = local_data$over_time_data
  class(otd) = 'data.frame'
  
  cnames = unique(c('y', 'Time', 'Subject', 'Electrode', local_data$var_fixed_effects))

  # fastest way to get matched rows    
  ind = otd$Subject %in% attr(factor(selected$Subject), 'levels') & otd$Electrode %in% attr(factor(selected$Electrode), 'levels')
  #ind = otd$Subject %in% unique(selected$Subject) & otd$Electrode %in% unique(selected$Electrode)
  
  otd = otd[ind,cnames]
  
  fe = paste(local_data$var_fixed_effects, collapse=' * ')
  group_analysis_cat2t('Got fixed effect')
  aot = otd %>% do_aggregate(as.formula('y ~ Time + Subject + Electrode' %?&% fe),
                             FUN=mean) %>% do_aggregate(as.formula('y ~ Time' %?&% fe), .fast_mse)
  group_analysis_cat2t('Agg OTD')
  
  cd = local_data$collapsed_data
  ind = cd$Subject %in% attr(factor(selected$Subject), 'levels') & cd$Electrode %in% attr(factor(selected$Electrode), 'levels')
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
    to_keep = local_data$by_electrode_results[rows,c('Subject', 'Electrode')]
    to_keep$Subject %<>% as.character
    
    attr(to_keep, 'label') = aggregate(Electrode ~ Subject, dipsaus::deparse_svec, data=to_keep) %>%
      apply(1, paste0, collapse=':') %>% paste0(collapse=', ')
    # res$sbjel_id = paste(to_keep$Subject, to_keep$Electrode, sep='_')
    
    return (to_keep)
}

windowed_activity <- function(lmer_results, collapsed_data) {
  # group_analysis_cat2t('IN WA')
    set_palette(local_data$omnibus_plots_color_palette)
    
    # print(grDevices::palette())
    # lmer_results = ..local_data$lmer_results
    # collapsed_data = ..local_data$collapsed_data
    
    lmer_results %?<-% local_data$lmer_results
    collapsed_data %?<-% local_data$collapsed_data
    
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))
    
    .y <- collapsed_data %>% do_aggregate(terms(lmer_results), m_se)
    # .y <- collapsed_data %>% do_aggregate(y ~ TimeWindow*ConditionGroup, m_se)
    
    # plot options
    po = local_data$omnibus_plots_plot_aesthetics
    if(ncol(.y) > 2) {
        # .y <- collapsed_data %>% do_aggregate(y ~ Subject + TimeWindow, m_se)
        yy = matrix(.y$y[,1], ncol = nlevels(.y[,2]), byrow=F)
        rownames(yy) = unique(.y[,1])
        colnames(yy) = unique(.y[,2])
        
        
        
        xp <- rutabaga::rave_barplot(yy, axes=F, col = adjustcolor(1:nrow(yy), rave_colors$BAR_PLOT_ALPHA),
                                     border=NA, beside=TRUE, axes=T,
                                     ylim = range(pretty(c(0, plus_minus(.y$y[,1], .y$y[,2])))),
                                     xlab=attr(terms(lmer_results), 'term.labels')[2])
        # rave_axis(2, at=axTicks(2))
        # rave_axis_labels(ylab=)
        leg = unique(.y[,1])
        legend(input$omnibus_plots_legend_location, legend=leg, text.col=seq_along(leg),
               cex=rave_cex.lab, bty='n', horiz=F, ncol = floor((length(leg)-1) / 3)+1 ) 
        ebars(xp, .y$y, col=1:nrow(yy), code=0, lwd=2, lend=0)
        
    } else {
        nms = .y[,1]
        if(ncol(.y) == 1) nms = 'Overall'
        
        .border <- .col <- NA
        
        if('border' %in% po) .border = 1:nrow(.y)
        if('filled' %in% po) .col = adjustcolor(1:nrow(.y), 0.7)
        
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
          for(ii in 1:nrow(xp)) {
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
    
    rave_axis_labels(ylab=local_data$var_dependent_label)
    rave_axis(2, at=axTicks(2))
    
    abline(h=0, col=rave_colors$TRIAL_TYPE_SEPARATOR)
    # group_analysis_cat2t('OUT WA')    
}

electrode_inspector_barplot <- function() {
  shiny::validate(shiny::need(!is.null(local_data$lmer_results), message = 'No model calculated'))
  shiny::validate(shiny::need(!is.null(local_data$show_by_electrode_results_rows_selected), message = 'No rows selected'))
  
  group_analysis_cat2t('IN EIB')
  
    selected = get_selected_subjel()
    cd = local_data$collapsed_data
    ind = cd$Subject %in% attr(factor(selected$Subject), 'levels') &
      cd$Electrode %in% attr(factor(selected$Electrode), 'levels')
    
    cd = cd[ind,]
    
    windowed_activity(collapsed_data = cd)
    rave_title(attr(selected, 'label'))
    
    group_analysis_cat2t('OUT EIB')
}

output$show_by_electrode_results <- DT::renderDataTable({
    by_electrode_results = local_data$by_electrode_results
    group_analysis_cat2t('Rendering DT')
    DT::datatable(by_electrode_results, class = 'nowrap',
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
      
      ##### set the margins of the plot
      par(mar = c(2.75, 3.5, 2, 1))
      
      custom_plot_download_renderers(input$custom_plot_select)
    })

custom_plot_download <- custom_plot_download_impl(
  module_id = 'group_analysis_lme',
  choices=c(
    'Activity over time', 'Mean activity in analysis window', 'Subset time series', 
    'subset barplot', 'Compare post-hoc variables')
)

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
                         'Close all other tabs'),
            tags$p(' ', style='margin-bottom:20px')
    )
}

observeEvent(input$btn_hide_everything_but_post_hoc_plot, {
    nms <- c("Data Import", "Build Condition Groups", "Single time window analysis", 
      "Multiple time window analysis", "Build Model", 'Model Output', 'Activity over time',
      'Mean activity in analysis window', 'Univariate stat output',
      'Statistical results by electrode', 'Subset time series', 'Subset barplot')
      
    lapply(nms, rave::close_tab, module_id = 'group_analysis_lme')
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
        wcsv(local_data$by_electrode_results, f1, rn=FALSE)
        fnames <- c(f1,
                    wrp("lmer_coefficients"), 
                    wrp('lmer_omnibus'),
                    wrp('compare_conditions_to_0'),
                    wrp('pairwise_comparisons'), 
                    'windowed_activity.pdf',
                    'power_over_time.pdf')
        
        wcsv(local_data$lmer_summary_coefficients, fnames[2])
        
        wcsv(local_data$anova_summary, fnames[3])
        
        wcsv(local_data$test_conditions, fnames[4])
        wcsv(local_data$compare_conditions, fnames[5])
        
        #trying to set the width of the plot based on the number of groups in the data
        H=6
        WFAC = 8/H
        .w = WFAC * nlevels(local_data$collapsed_data$Group)
        as_pdf(file.path(tmp_dir, fnames[6]), w=.w, h=H, {
            par(mar=.1 + c(5,4,1,1))
            # call_with_local_data(windowed_activity)
            windowed_activity()
        })
        
        as_pdf(file.path(tmp_dir, fnames[7]), w=8, h=4, {
            par(mar=.1 + c(5,4,1,1))
            # call_with_local_data(power_over_time)
            power_over_time()
        })
        
        
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
      
      dipsaus::cat2('DR tokens: ', paste0(dr_tokens, collapse = '|'))
      
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
      .rave_axis_labels(ylab=dd, cex.lab=.65, line=.1)
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
    # Check whether load is needed
    lmer_results = local_data$lmer_results
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'Please run LMER model first'))
    
    by_electrode_results = local_data$by_electrode_results
    
    #make sure all the pvalues are numeric
    by_electrode_results[startsWith(names(by_electrode_results), 'p(')] %<>% lapply(as.numeric)
    
    # load brain
    brains = lapply(unique(by_electrode_results$Subject), function(sub){
        tryCatch({
            rave::rave_brain2(sprintf('%s/%s', by_electrode_results$Project[1], sub))
        }, error = function(e){ NULL })
    })
    brains = dipsaus::drop_nulls(brains)
    brain = threeBrain::merge_brain(.list = brains, template_surface_types = c('pial', 'inf_200'))
    
    # set_palette()
    nms <- names(by_electrode_results)[-(1:3)]
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
    
    pals[2:ncol(by_electrode_results)] = pals
    # names(pals) = fix_name_for_js(names(by_electrode_results))
    names(pals) = names(by_electrode_results)
    
    pals[str_detect(names(pals), 'p\\.')] = list(pval_pal)
    pals[names(pals) %in% c('p')] = list(pval_pal)
    
    brain$set_electrode_values(by_electrode_results)
    re = brain$plot(side_width = side_width, val_ranges = val_ranges, palettes = pals,
                    side_display = FALSE, control_display=FALSE)
}

build_custom_var <- function(ber, var_name, var_string) {
    vals = eval_in_dataframe(ber, var_string)
    
    vals %?<-% runif(nrow(ber))
    
    ber[[var_name]] = vals
    return(ber)
}


# this function does not work, in general.
# Because it works by string matching, "simple" variable names in your dataframe may conflict with 
# function names you're using. Be careful. all instances of names(df) in your var_string are going to be str_replaced
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
  shiny::validate(shiny::need(!is.null(local_data$by_electrode_results), message = 'No results available'))
  
  ber = local_data$by_electrode_results
  
  # for printing the tables, p-values are converted to string representations, so convert them 
  # back so they can be used in numeric representations
  ber[startsWith(names(ber), 'p(')] %<>% lapply(as.numeric)
  
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
    x = df[[xvar]] %>% as.numeric
    y = df[[yvar]] %>% as.numeric
    
    if(tolower(zvar) != 'none') {
      z = df[[zvar]]
      y = resid(lm(y ~ z))
    }
    
    # One hard part here is that the aspect ratio is hard to get right.
    # We could consider giving a call to layout...
    
    # check if we need to set the plot bounds
    xlim = text_to_range(input$post_hoc_plot_xlim)
    ylim = text_to_range(input$post_hoc_plot_ylim)
    
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
    if(tolower(xvar) %in% tolower(c('Subject', 'jitter(Subject)'))) {
      rave_axis(1, at=unique(as.integer(df$Subject)), labels=levels(df$Subject),
                lwd = ifelse(nsub==1, 0, 1))
      xvar = 'Subject'
    } else {
      rave_axis(1, at=axTicks(1))
      if(xvar == 'CUSTOM_X') {
        xvar = input$post_hoc_plot_xvar_custom
      }
    }
    if(yvar %in% c('Subject', 'jitter(Subject)')) {
      rave_axis(2, at=unique(as.integer(df$Subject)), labels=levels(df$Subject),
                lwd = ifelse(nsub==1, 0, 1))
      yvar = 'Subject'
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
