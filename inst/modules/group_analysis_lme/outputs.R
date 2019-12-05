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

# src_data_snapshot.orig <- function(){
#     
#     if(!is.data.frame(local_data$analysis_data)){
#         return(htmltools::div(style='color:#a1a1a1; text-align:center', 'Analysis data not loaded yet.'))
#     }
#     
#     # show snapshot of analysis table
#     tbl_raw = local_data$analysis_data
#     vars = names(tbl_raw)
#     tbl = local_data$analysis_data_filtered
#     
#     dv = input$model_dependent; dv = dv[dv %in% vars]
#     fe = input$model_fixed_effects; fe = fe[fe %in% vars]
#     fr = input$model_random_effects; fr = fr[fr %in% vars]
#     rest = vars[!vars %in% c(dv,fe,fr)]
#     
#     str2 = function(val, v){
#         suppressWarnings({
#             if(is.character(val) || v %in% fr || is.factor(val)){
#                 val = as.factor(val)
#                 lv = levels(val); 
#                 nlv = length(lv)
#                 if(nlv){ lv = lv[1:min(nlv, 4)] }
#                 str = sprintf(' Factor w/ %d level%s [%s%s]', nlv, 
#                               ifelse(nlv>1, 's', ''),
#                               paste(lv, collapse = ', '), ifelse(nlv>4, ', ...', ''))
#             }else if(is.numeric(val)){
#                 str = sprintf(' %s [range: %.4g ~ %.4g]', storage.mode(val), 
#                               min(val, na.rm = TRUE), max(val, na.rm = TRUE))
#             }else{
#                 str = utils::capture.output(str(val))
#             }
#             str
#         })
#     }
#     dv_tag = rest_tag = fe_tag = fr_tag = NULL
#     if(length(dv)){
#         dv_tag = tagList(
#             '- Dependent -',
#             tags$ul(
#                 tags$li(strong(dv), ': ', str2(tbl[[dv]], dv))
#             )
#         )
#     }
#     
#     if(length(fe)){
#         fe_tag = tagList(
#             '- Fixed effects -',
#             tags$ul(
#                 lapply(fe, function(v){
#                     tags$li(strong(v), ': ', str2(tbl[[v]], v))
#                 })
#             )
#         )
#     }
#     
#     if(length(fr)){
#         fr_tag = tagList(
#             '- Random effects -',
#             tags$ul(
#                 lapply(fr, function(v){
#                     tags$li(strong(v), ': ', str2(tbl[[v]], v))
#                 })
#             )
#         )
#     }
#     
#     if(length(rest)){
#         rest_tag = tagList(
#             '- Variables not in the model -',
#             tags$ul(
#                 lapply(rest, function(v){
#                     tags$li(strong(v), ': ', str2(tbl[[v]], v))
#                 })
#             )
#         )
#     }
#     
#     if(length(c(dv, fe, fr))){
#         n_complete = sum(complete.cases(tbl[,c(dv, fe, fr)]))
#     }else{
#         n_complete = nrow(tbl)
#     }
#     
#     
#     tagList(
#         tags$p(
#             # 1. dimensions
#             'Original analysis table (raw): ', strong(sprintf('%d rows x %d columns', nrow(tbl_raw), ncol(tbl_raw))), br(),
#             
#             # 2. columns
#             'Columns: ', strong(paste(vars, collapse = ', ')), br(),
#             
#             hr(),
#             
#             # 3. filtered table
#             'Filtered analysis table (input data): ', strong(sprintf(
#                 '%d rows (%d complete entries)', nrow(tbl), n_complete
#             )), br(),
#             
#             # 3. column types
#             'Column types: ', br(),
#             
#             dv_tag, fe_tag, fr_tag, rest_tag
#             
#         )
#         
#     )
#     
# }

windowed_activity <- function(lmer_results, collapsed_data) {
    lmer_results %?<-% local_data$lmer_results
    collapsed_data %?<-% local_data$collapsed_data
    
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))
    
    .y <- aggregate(Power ~ Group, m_se, data=collapsed_data)
    
    xp <- rutabaga::rave_barplot(.y$Power[,1], axes=F, col = adjustcolor(1:nrow(.y), 0.7),
                           border=NA,
                           ylim = range(pretty(c(0, plus_minus(.y$Power[,1], .y$Power[,2])))),
                           names.arg=.y$Group)
    
    rave_axis(2, at=axTicks(2))
    rave_axis_labels(xlab='Group', ylab='Power')
    abline(h=0)
    
    ebars(xp, .y$Power, col=1:nrow(.y), code=0, lwd=2, lend=0)
}

mass_univariate_results <-  function(){
    lmer_results = local_data$lmer_results
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))

    htmltools::div(
        hr(),
        h3('Results per electrode'),
        DT::dataTableOutput(ns('show_by_electrode_results'))        
    )
}

output$show_by_electrode_results <- DT::renderDataTable({
    by_electrode_results = local_data$by_electrode_results
    
    DT::datatable(by_electrode_results, class = 'nowrap',
                  options = list(
                      scrollX = TRUE,
                      order = list(list(2, 'asc'), list(3, 'desc'))
                  ))
})


power_over_time <- function(lmer_results, collapsed_data, agg_over_trial, analysis_window) {
    lmer_results %?<-% local_data$lmer_results
    collapsed_data %?<-% local_data$collapsed_data
    agg_over_trial %?<-% local_data$agg_over_trial
    analysis_window %?<-% local_data$analysis_window
    
    shiny::validate(shiny::need(!is.null(lmer_results), message = 'No model calculated'))

    sample_size = collapsed_data %>% do_aggregate(Power ~ Group, length) %$% {
        names(Power) = Group
        Power
    }
    
    lpd <- agg_over_trial %>% split((.)$Group) %>% lapply(function(aot) {
        res = list(
            x = aot$Time,
            data = aot$Power,
            N= sample_size[as.character(aot$Group[1])],
            range = range(plus_minus(aot$Power[,1], aot$Power[,2])),
            has_trials = TRUE,
            name = aot$Group[1]
        )
        
        attr(res$data, 'xlab') = 'Time'
        attr(res$data, 'ylab') = 'Power'
        res
    })
    set_palette('Beautiful Field')
    
    time_series_plot(plot_data = lpd)
    axis_label_decorator(lpd)
    
    abline(v=analysis_window, lty=2)
    
    legend_decorator(lpd, include = c('name', 'N'))
}

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

..old_code_for_visualize_lmer_results <- function() {
    tbl = shiny::isolate(local_data$analysis_data_filtered)
    
    # assign('lmer_results', lmer_results, envir = globalenv())
    # assign('tbl', tbl, envir = globalenv())
    
    # Get Random effect
    # randef = lme4::ranef(lmer_results)
    coef = stats::coefficients(lmer_results); coef
    project_name = subject$project_name
    
    # two cases: 
    # Electrode:Subject
    elec_table = NULL
    val_ranges = list()
    if('Electrode:Subject' %in% names(coef)){
        dat = data.matrix(coef$`Electrode:Subject`)
        # if('(Intercept)' %in% names(coef$`Electrode:Subject`)[[1]]){
        #     dat[, -1] = dat[, -1] + dat[, 1]
        # }
        data_range = max(abs(range(dat)))
        tmp = rownames(dat)
        tmp = stringr::str_split_fixed(tmp, ':', n = 2)
        dat = as.data.frame(dat)
        val_ranges = sapply(names(dat), function(d){ c(-data_range, data_range) }, 
                            simplify = FALSE, USE.NAMES = TRUE)
        dat$Electrode = as.integer(tmp[,1])
        dat$Subject = tmp[,2]
        elec_table = dat
    }else if('Subject:Electrode' %in% names(coef)){
        dat = data.matrix(coef$`Subject:Electrode`)
        # if('(Intercept)' %in% names(coef$`Subject:Electrode`)[[1]]){
        #     dat[, -1] = dat[, -1] + dat[, 1]
        # }
        data_range = max(abs(range(dat)))
        tmp = rownames(dat)
        tmp = stringr::str_split_fixed(tmp, ':', n = 2)
        dat = as.data.frame(dat)
        val_ranges = sapply(names(dat), function(d){ c(-data_range, data_range) }, 
                            simplify = FALSE, USE.NAMES = TRUE)
        dat$Electrode = as.integer(tmp[,2])
        dat$Subject = tmp[,1]
        elec_table = dat
    }else if('Electrode' %in% names(coef)){
        # Subject only has one
        
        dat = data.matrix(coef$Electrode)
        # if('(Intercept)' %in% names(coef$Electrode)[[1]]){
        #     dat[, -1] = dat[, -1] + dat[, 1]
        # }
        data_range = max(abs(range(dat)))
        dat = as.data.frame(dat)
        
        val_ranges = sapply(names(dat), function(d){ c(-data_range, data_range) }, 
                            simplify = FALSE, USE.NAMES = TRUE)
        dat$Electrode = rownames(coef$Electrode)
        elec_table = merge(unique(tbl[, c('Project', 'Subject', 'Electrode')]), dat, by = 'Electrode')
        
    }
    
}

download_all_results <- function() {
    lmer_results = local_data$lmer_results
    if(is.null(lmer_results))  return()

    tagList(tags$p(' ', style='margin-top:20px'),
            downloadLink(ns('btn_download_all_results'),
                         'Download All Results'),
            tags$p(' ', style='margin-top:20px'))
}


# call_with_local_data <- function(what) {
#     shiny::isolate({
#         args=shiny::reactiveValuesToList(local_data) [(names(formals(what)))]
#         do.call(what,args)
#     }) 
# }


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




# 3D viewer, takes 3 args
lme_3dviewer_fun <- function(need_calc, side_width, daemon_env, ...){
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
    
    brain$set_electrode_values(by_electrode_results)
    re = brain$plot(side_width = side_width, val_ranges = val_ranges,
                    side_display = FALSE, control_display=FALSE)
}


lme_diagnosis <- function(){
    shiny::validate(shiny::need(TRUE == FALSE, message = 'Not implemented'))
    
    # plot(1:10)
}
