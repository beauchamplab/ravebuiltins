input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

local_data %?<-% reactiveValues(
    # Full data has two parts: local_data$analysis_data_raw, and local_data$additional_data
    # together makes analysis_data
    analysis_data_raw = NULL,
    additional_data = NULL,
    analysis_data = NULL,
    collapsed_data = NULL,
    
    omnibus_plots_color_palette = NULL,
    
    analysis_window = 0:1,
    analysis_window_label = NULL,
    number_of_subjects = NULL,
    
    potential_analysis = list(),
    analysis_name = NULL,
    sample_table = NULL,
    # this stores the current value in the UI
    var_dependent = NULL,
    # this stores what was actually fitted (becomes var_dependent during run_analysis)
    var_dependent_label = NULL,
    
    var_fixed_effects = NULL,
    var_fixed_effects_available=NULL,
    var_random_effects = NULL,
    lmer_results = NULL,
    lmer_results_summary = NULL,
    
    show_by_electrode_results_rows_selected = NULL
)
local_filters = reactiveValues(
    filter_count = 0,
    filter_observers = 0
)


observeEvent(input$show_by_electrode_results_rows_selected, {
    local_data$show_by_electrode_results_rows_selected = input$show_by_electrode_results_rows_selected
})

observeEvent(input$omnibus_plots_color_palette, {
    local_data$omnibus_plots_color_palette = input$omnibus_plots_color_palette
})

# cond_group_ui = function(){
#     dipsaus::compoundInput2(
#         inputId = ns('cond_group'), prefix= 'Condition Group', inital_ncomp = 1, components = {
#             textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
#             selectInput('group_conditions', ' ', choices = '', multiple = TRUE, selected = character(0))
#         }, max_ncomp = 20)
# }


# Sync all group_names
# lapply(1:0, function(ii){
#     name_id = paste0('cond_group_group_name_', ii)
#     .env = environment()
#     observeEvent(input[[name_id]], {
#         val = val_raw = input[[name_id]]
#         if(length(val)){
#             if( stringr::str_detect(val, '^CondGroup[0-9]*') || 
#                 val %in% names(local_data$analysis_data_raw$headers) ){
#                 # Invalid group name, reset to default
#                 val = sprintf('CondGroup%d', ii)
#             }
#             if( val != val_raw ){
#                 updateTextInput(session, name_id, value = val)
#             }
#         }
#     }, event.env = .env, handler.env = .env)
# })

observe({
    dipsaus::cat2('main observe', level='INFO')
    
    raw = local_data$analysis_data_raw
    
    if( !is.list(raw) ){
        local_data$analysis_data_filtered = NULL
        return()
    }
    
    # raw = list(data = local_data$analysis_data_raw)
    local_data$analysis_data_filtered = raw$data
    
    conditions = unique(raw$data$Condition); if(!length(conditions)){ conditions = '' }
    time_range = range(raw$data$Time, na.rm = TRUE)
    analysis_window = time_range
    confs = dipsaus::drop_nulls(raw$confs)
    groups = list()
    if(length(confs)){
        confs = confs[[1]]
        groups = confs$GROUPS
        analysis_window = sort(c(confs$ANALYSIS_WINDOW, time_range)[1:2])
    }
    
    # store this in local_data so that we have everything in one place
    local_data$analysis_window = analysis_window
    
    dipsaus::updateCompoundInput2(session, 'cond_group', ncomp = max(length(groups), 1), 
                                  initialization = list(
                                      group_conditions = list( choices = conditions )
                                  ), value = groups)
    
    updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
                      max=time_range[[2]], value=analysis_window)
    
    nms = names(local_data$analysis_data_filtered)
    
    usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
    dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
    # usual_ivs = c('Electrode', 'Trial', 'Condition', 'Subject')
    # usual_ivs = usual_ivs[usual_ivs %in% nms]
    
    # DV
    dvs = nms[
        sapply(nms, function(x) any(startsWith(x, usual_dvs)))
    ]
    # IVs
    local_data$number_of_subjects = length(unique(local_data$analysis_data_filtered$Subject))
    re_sel = c('Electrode')
    if(isTRUE(local_data$number_of_subjects > 1)) {
        re_sel %<>% c('Subject')
    }
    
    # determine all possible effects
    # here we need to check the number of groups that area assigned (> 1 means create a factor) 
    dipsaus::updateCompoundInput2(session, 'multi_window_analysis', 
                                  initialization = list(
                                      analysis_window = list(min = time_range[1], max = time_range[2], value = time_range),
                                      window_is_active = list(value=FALSE)
                                  ))
    var_fe_avail <- nms[! nms %in% c(dvs, dnu)]
    local_data$var_fixed_effects_available = var_fe_avail
    updateSelectInput(session, 'model_random_effects', choices = var_fe_avail, selected = re_sel)
    updateSelectInput(session, 'model_fixed_effects', choices = var_fe_avail)
    updateSelectInput(session, 'model_dependent', selected = dvs[1], choices = dvs)
    
})

observeEvent(input$single_analysis_window, {
    if(isTRUE(input$single_analysis_window)) {
        updateCheckboxInput(session, 'multi_window_is_active', value = FALSE)
    }
})

observeEvent(input$analysis_window, {
    local_data$analysis_window = input$analysis_window
})

observeEvent(input$multi_window_is_active, {
    shiny::isolate({
        if(isTRUE(input$multi_window_is_active)) {
            updateCheckboxInput(session, 'single_analysis_window', value=FALSE)
            local_data$var_fixed_effects_available %<>% add_term('TimeWindow')
        } else {
            local_data$var_fixed_effects_available %<>% remove_term('TimeWindow')
        }
        
        update_available_effects()
    })
})




# update_available_effects <- function() {
#     mre = input$model_random_effects
#     updateSelectInput(session, 'model_random_effects',
#                       choices = local_data$var_fixed_effects_available, 
#                       selected = mre[mre %in% local_data$var_fixed_effects_available])
#     mfe = input$model_fixed_effects
#     updateSelectInput(session, 'model_fixed_effects',
#                       choices = local_data$var_fixed_effects_available,
#                       selected = mfe[mfe %in% local_data$var_fixed_effects_available])
# }

update_available_effects <- function() {
    mre = input$model_random_effects
    var_fe_avail = local_data$var_fixed_effects_available
    mfe = input$model_fixed_effects
    
    # ensure required random effects are loaded
    mre %<>% add_term('Electrode')
    if(isTRUE(local_data$number_of_subjects > 1)) {
        mre %<>% add_term('Subject')
    } else {
        mre %<>% remove_term('Subject')
    }

    updateSelectInput(session, 'model_random_effects',
                      choices = var_fe_avail,
                      selected = mre[mre %in% var_fe_avail])
    
    updateSelectInput(session, 'model_fixed_effects',
                      choices = var_fe_avail,
                      selected = mfe[mfe %in% var_fe_avail])
}

observeEvent(input$cond_group, {
    dipsaus::cat2('Groups changing ' %&% length(input$cond_group), level = 'INFO')
    shiny::isolate({
        cg = input$cond_group
        if(sum(unlist(lapply(cg, function(cg) length(cg$group_conditions) > 0))) > 1) {
            local_data$var_fixed_effects_available %<>% add_term('ConditionGroup')
        } else {
            local_data$var_fixed_effects_available %<>% remove_term('ConditionGroup')
        }
        update_available_effects()
    })
})

# the idea here is to add terms while ensuring no duplicates
add_term <- function(x, value) {
    unique(c(x, value))
}

remove_term <- function(x, value) {
    x[x!=value]
}

build_lme_formula_string = function(..., exclude = NULL){
    
    if(!isTRUE(nrow(local_data$analysis_data_filtered) > 1)) {
        warning('data not available...')
        return('y ~ X')
    }
    
    dv = local_data$var_dependent
    iv = local_data$var_fixed_effects
    re = local_data$var_random_effects
    
    # fixed effects are easy, except we need to make sure TimeWindow comes after ConditionGroup. This
    # is quickly done by just sorting
    iv %<>% sort
    fe_str = paste(iv, collapse='*')
    
    # randomf effects are harder because of potential nesting of electrode in subject
    re_str = ''
    
    nsub = length(unique(local_data$analysis_data_filtered$Subject))
    if(nsub > 1) {
        re_str = '(1|Subject/Electrode)'
        re %<>% remove_term('Electrode')
        re %<>% remove_term('Subject')
    } else {
        re_str = '(1|Electrode)'
        re %<>% remove_term('Electrode')
    }
    
    if(length(re) > 0) {
        re_str = paste(re_str,
                       paste0('(1|', re, ')', collapse='+'),
                       sep='+')
    }
    
    # res = sprintf("%s ~ %s + %s", dv, fe_str, re_str)
    res = dv
    if(isTRUE(nchar(fe_str) > 0)) {
        res = paste(res, '~', fe_str)
    } else {
        res = paste(res, '~ 1')
    }
    
    if(isTRUE(nchar(re_str) > 0)) {
        res = paste(res, '+', re_str)
    }
    
    return(res)
}


rebuild_model_formula <- function() {
    if(is.null(local_data$analysis_data_filtered)) return(1)
    
    # ensure required random effects are loaded
    vre = input$model_random_effects
    vre %<>% add_term('Electrode')
    if(isTRUE(local_data$number_of_subjects > 1)) {
        vre %<>% add_term('Subject')
    } else {
        vre %<>% remove_term('Subject')
    }
    
    local_data$var_random_effects = vre
    local_data$var_fixed_effects = input$model_fixed_effects
    local_data$var_dependent = input$model_dependent
    
    updateTextAreaInput(session, 'model_formula', value = build_lme_formula_string())
}

observeEvent(input$model_random_effects, rebuild_model_formula(), ignoreNULL = FALSE)
observeEvent(input$model_fixed_effects, rebuild_model_formula(), ignoreNULL = FALSE)
observeEvent(input$model_dependent, rebuild_model_formula())


build_stat_names <- function(lbls, stat.vars = c('m', 't', 'p')) {
    c(outer(c(stat.vars %&% '('),
            lbls, paste0)) %&% ')'
}


flatten_emmeans_pairwise <- function(summ) {
    # summ = bed.pairwise

    var.names = attr(summ[[1]], 'pri.vars')
    lbls = apply(summ[[1]][var.names], 1, paste0, collapse='&')
    
    a = summ[[1]] %$% {
        c(rbind(emmean, t.ratio, p.value))
    } %>% set_names (build_stat_names(lbls))
    
    b = summ[[2]] %$% {
        c(rbind(estimate, t.ratio, p.value))
    } %>% set_names(build_stat_names(summ[[2]]$contrast))
    
    res = c(a,b)
    names(res) = str_replace_all(names(res), ',', '&')
    
    res
}

analyze_single_electrode <- function(bed) {
    # create a string represent the requested fixed effects
    fe = paste(local_data$var_fixed_effects, collapse=' * ')
    
    # bed <- by_el_data %>% split((.)$UUID) %>% extract2(1)
    if(str_detect(fe, 'TimeWindow')) {
        .lm = lmer(as.formula('y ~ ' %&% fe %&% '+ (1|Trial)'), data=bed)
        # .lm = lmer(y ~ ConditionGroup*TimeWindow + (1|Trial), data=bed)
        omni.mat = as.matrix(car::Anova(.lm)[c('Chisq', 'Pr(>Chisq)')])
        
        bed.omni = c(t(omni.mat)) %>% set_names(
            build_stat_names(rownames(omni.mat), c('X2', 'p')))
        
        # this will help with the calls later
        emmeans::emm_options('lmer.df' = 'satterthwaite')
        
    } else {
        .lm = lm(as.formula('y ~ 1' %?&% fe), data=bed)
        omni.mat = as.matrix(car::Anova(.lm)[,c('F value', 'Pr(>F)')])
        omni.mat = omni.mat[str_detect(rownames(omni.mat),'Residuals', negate = 1),,drop=FALSE]
        
        bed.omni = c(t(omni.mat)) %>% set_names(
            build_stat_names(rownames(omni.mat), c('F', 'p'))
        )
    }
    
    # main effects
    bed.mains = lapply(local_data$var_fixed_effects, function(vfe) {
        m =summary(emmeans::emmeans(.lm,
                                    as.formula('~' %&% vfe)),
                   infer=c(F,T))[,c(1:2, 5:6)]
        nms = as.character(m[,1])
        c(t(m[,-1])) %>% set_names(build_stat_names(nms))
    }) %>% unlist
    
    # pairwise comparisons
    bed.pairwise = NULL
    if(length(local_data$var_fixed_effects) > 0) {
        bed.pairwise = summary(emmeans::emmeans(
            .lm, as.formula('pairwise ~ 1' %?&% fe)),
            infer=c(FALSE, TRUE)) %>% flatten_emmeans_pairwise
    }
    
    res <- data.frame(Project = bed$Project[1], Subject = bed$Subject[1], 
                      Electrode = bed$Electrode[1])
    res[names(bed.omni)] = bed.omni
    res[names(bed.mains)] = bed.mains
    if(!is.null(bed.pairwise)){
        res[names(bed.pairwise)] = bed.pairwise
    }
    res
}


observeEvent(input$run_analysis, {
    if(exists('.__DEBUG__')) {
        assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
        assign('..rm_windows', input$multi_window_analysis, envir=globalenv())        
        
        # multi_window = ..rm_windows
        # local_data = ..local_data
        # cond_group = list(list('group_conditions' = 'Dynamic'), list('group_conditions' = 'Static'))
        # cond_group = list(list('group_conditions' = c('Dynamic', 'Static')))
        
        cond_group = list(
            list(group_name = 'A-only', group_conditions = c('drive_a','last_a', 'meant_a', 'known_a')),
            list(group_name = 'V-only', group_conditions = c('drive_v','last_v', 'meant_v', 'known_v')),
            list(group_name = 'AV', group_conditions = c('drive_av','last_av', 'meant_av', 'known_av'))
        )
    }
    
    cond_group <- dipsaus::drop_nulls(lapply(input$cond_group, function(g){
        if(length(g$group_conditions) == 0) return( NULL )
        return(g)
    }))
    
    ### Things to check
    
    # are there any fixed effects supplied?
    if(isTRUE(length(local_data$var_fixed_effects) < 1)) {
        showNotification('No fixed effects specified. Running test against 0')
    }
    
    
    # if doing multi window, then need two windows with non-overlapping windows
    # check if null here, if so, assign to be all trials
    if(length(cond_group) < 1) {
        cond_group = list(list('group_conditions' = unique(local_data$analysis_data_filtered$Condition)))
    }

    # if(exists('.__DEBUG__')) assign('ldf', value = local_data$analysis_data_filtered, envir = globalenv())
    multi_window = input$multi_window_analysis
    
    # create a joint variable representing the Group as a factor
    showNotification(p('Fitting mixed effect model. Please wait...'), duration = NULL, type = 'default', id = ns('noti'))
    
    # ldf = ..local_data$analysis_data_filtered
    ldf <- local_data$analysis_data_filtered
    
    all_trial_types <- unique(ldf$Condition)
    
    selected_trial_types = unlist(lapply(cond_group, getElement, 'group_conditions')) %>% unique
    if(length(selected_trial_types) > 0) {
        all_trial_types = selected_trial_types
    }
    
    subset_data <- subset(ldf, subset = Condition %in% all_trial_types)
    
    # make sure all the groups have a name
    for (ii in seq_along(cond_group)) {
        if(!isTRUE(nchar(cond_group[[ii]]$group_name) > 0)) {
            cond_group[[ii]]$group_name = 'Group_' %&% LETTERS[ii]
        }
    }
    subset_data$ConditionGroup = cond_group[[1]]$group_name
    for(ii in seq_along(cond_group)[-1]) {
        subset_data$ConditionGroup[subset_data$Condition %in% cond_group[[ii]]$group_conditions] = cond_group[[ii]]$group_name
    }
    subset_data$ConditionGroup %<>% factor(levels = sapply(cond_group, `[[`, 'group_name'))
    
    cat2(paste('assigning ', local_data$var_dependent, ' to y'), level='INFO')
    local_data$var_dependent_label = pretty_string(local_data$var_dependent)
    subset_data$y = subset_data[[local_data$var_dependent]]
    
    local_data$over_time_data = subset_data
    analysis_window = local_data$analysis_window
    local_data$analysis_window_label = analysis_window
    
    # if we're doing multi-window analysis, the analysis_window is the union of all the windows
    # this is potentially (likely) discontinuous
    window_names_in_order = c()
    if(isTRUE(input$multi_window_is_active)) {
            # fix the window names
            for(ii in seq_along(multi_window)) {
                if(!isTRUE(nchar(multi_window[[ii]]$window_name) > 0)) {
                    multi_window[[ii]]$window_name = 'Window_' %&% LETTERS[ii]
                }
                window_names_in_order[ii] = multi_window[[ii]]$window_name
            }
            
            # fix the time window in the subset data and then assign labels to the resulting windows
            .times = round(unique(subset_data$Time),7)
            
            # create a dummy window name, this can be stripped out latter
            subset_data$TimeWindow = "ZZZ___"
            
            # build the time Union and assign labels to included time points
            included_times = rep(FALSE, length(.times))
            for(ii in seq_along(multi_window)) {
                aw = unlist(multi_window[[ii]]$analysis_window)
                included_times = included_times | (.times %within% aw)
                subset_data$TimeWindow[subset_data$Time %within% aw] = multi_window[[ii]]$window_name
            }
            
            local_data$over_time_data$TimeWindow = subset_data$TimeWindow
            
            # be careful here about %in% (correct) vs. %within% (as used above, but not the thing to use here)
            subset_data %<>% subset(Time %in% .times[included_times])
            
            subset_data$TimeWindow %<>% factor
    } else {
        subset_data %<>% subset(Time %within% analysis_window)
    }
    fe = paste(local_data$var_fixed_effects, collapse=' * ')
    re = paste0(unique(c('Subject', 'Electrode', local_data$var_random_effects)), collapse='+')
    collapse_formula = as.formula(sprintf('y ~ %s', re %?&% fe))
    
    collapsed_data <- aggregate(collapse_formula, data=subset_data, FUN=mean)
    # we need to factor time window to make sure the comparisons have the appropriate order (and baseline)
    # do this here before the data are saved into local_data
    if('TimeWindow' %in% names(collapsed_data)) {
        collapsed_data$TimeWindow %<>% factor(levels=window_names_in_order)
    }
    local_data$collapsed_data = collapsed_data
    
    local_data$agg_over_trial = aggregate(as.formula('y ~ Time + Subject + Electrode' %?&% fe),
                                          data = local_data$over_time_data, FUN=mean) %>%
                                do_aggregate(as.formula('y ~ Time' %?&% fe), .fast_mse)
    
    # for the per-electrode plots, we're going back to the over_time data because we need the individual trials
    if(isTRUE(input$multi_window_is_active)) {
        windows = lapply(multi_window, function(mw) unlist(mw$analysis_window))
    } else {
        windows = analysis_window
    }
    # fe
    by_el_data = local_data$over_time_data %>%
        subset((.)$Time %within_any% windows) %>% 
        do_aggregate(as.formula('y ~ Condition + Trial + Subject + Electrode + Project' %?&% fe), mean)
    
    if(!is.null(by_el_data$TimeWindow)) {
        by_el_data$TimeWindow %<>% factor(levels = window_names_in_order)
    }
    
    if(!is.null(by_el_data$ConditionGroup)) {
        by_el_data$ConditionGroup %<>% factor
    }
    
    by_el_data$UUID = by_el_data %$% {Subject %&% Electrode}
    
    by_el_data %>% split((.)$UUID) %>%
        lapply(analyze_single_electrode) %>%
        rbind_list %>% magrittr::set_rownames(NULL) -> by_el_results
    
    round_pvals <- function(pval){
        lpval = pmax(-16, log10(pval))
        
        ifelse(lpval > -3.5,
               formatC(round(pval,4),width = 4, digits=4),
               paste0('1e', formatC(round(lpval), width=3,flag=0)))
    }
    
    
    ### let's clean up the doubled parens that can happen for Intercept tests
    names(by_el_results) %<>% stringr::str_replace_all(c('\\(\\('='(', '\\)\\)'=')'))
    
    by_el_results[names(by_el_results) %>% startsWith('p(')] %<>% lapply(round_pvals)
    by_el_results[names(by_el_results) %>% startsWith('t(')] %<>% lapply(round, 2)
    by_el_results[names(by_el_results) %>% startsWith('F(')] %<>% lapply(round, 2)
    by_el_results[names(by_el_results) %>% startsWith('X2(')] %<>% lapply(round, 2)
    by_el_results[names(by_el_results) %>% startsWith('m(')] %<>% lapply(round, 2)
    
    #de-factor the electrode label
    by_el_results$Electrode <-  as.numeric(as.character(by_el_results$Electrode))
    
    # create a jittered subject value that can be used for plotting
    by_el_results$`jitter(Subject)` = jitter(as.integer(by_el_results$Subject))
    
    # udate the UI for post hoc plots
    updateSelectInput(session, 'post_hoc_plot_xvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = 'jitter(Subject)')
    updateSelectInput(session, 'post_hoc_plot_yvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = names(by_el_results)[4])
    updateSelectInput(session, 'post_hoc_plot_zvar',
                      choices = c('None', names(by_el_results), 'CUSTOM'))
    
    local_data$by_electrode_results = by_el_results
    
    fo <-  str_replace_all(input$model_formula, input$model_dependent, 'y')
    # fo <- str_replace_all('Pct_Change_Power_1stWord ~ TimeWindow + (1|Subject/Electrode)', 'Pct_Change_Power_1stWord', 'y')
    # fo = 'y ~ 1 + (1|Subject/Electrode)'
    # fo = 'y ~ ConditionGroup + (1|Subject/Electrode) + (0+ConditionGroup|Subject) + (1|Condition)'
    fo %<>% as.formula
    
    tryCatch({
        # print('trying LME')
        if(length(local_data$var_fixed_effects) > 0) {
            lmer_results = lmerTest::lmer(fo,
                                          data=collapsed_data, na.action=na.omit)
            # summary(lmer(y ~ 1 + (1|Subject), data=collapsed_data))
        } else if(local_data$number_of_subjects > 1) {
            lmer_results = lmerTest::lmer(y ~ 1 + (1|Subject),
                                          data=collapsed_data, na.action=na.omit)
        } else {
            lmer_results = lm(y ~ 1, data=collapsed_data, na.action=na.omit)
        }
        # print('Model fit succeeded')
        local_data$lmer_results = lmer_results
        local_data$lmer_results_summary <- summary(lmer_results)
        # print('summary succeeded')
        
        showNotification(p('Model finished!'), duration = 3,
                         type = 'default', id = ns('noti'))
        
    }, error = function(e){
        print(e)
        if(is.list(e)){
            msg = e$message; msg %?<-% ''
            cal = e$call; cal %?<-% ''
            # e = sprintf('%s in %s', msg, cal)
        }
        # grouping factors must have > 1 sampled level
        showNotification(p(e), duration = 20, type = 'error', id = ns('noti'))
        local_data$lmer_results = NULL
        local_data$lmer_results_summary = NULL
    })
    
    if(exists('.__DEBUG__')) {
        assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
    }
    
    
})


#### Feature selection field handlers ####

# UI for filters
# var_sel = function(){
#     if(!is.data.frame(local_data$analysis_data)){
#         return(span(style = 'color: #a1a1a1', 'Analysis table not loaded'))
#     }
#     n_filters = local_filters$filter_count
#     vars = local_data$analysis_data; vars %?<-% ''
#     
#     filter_uis = NULL
#     minus_btn = NULL
#     
#     if(n_filters > 0){
#         minus_btn = actionButton(ns('filter_minus'), shiny::icon('minus'))
#         filter_uis = lapply( seq_len(n_filters), function(ii){ get_ui( ii , vars ) } )
#     }
#     
#     tagList(
#         filter_uis,
#         div(
#             # Put a div to make buttons within a row
#             actionButton(ns('filter_add'), shiny::icon('plus')),
#             minus_btn
#         ),
#         actionLink(ns('view_filtered'), 'Preview filtered data')
#     )
# }
# 
# observeEvent(input$view_filtered, {
#     # Collect data
#     shiny::showModal(shiny::modalDialog(
#         title = 'Preview input data', size = 'l', easyClose = TRUE, fade = FALSE,
#         tags$style('.modal-lg { min-width: 80vw; }'),
#         DT::dataTableOutput(ns('view_filtered_tbl'))
#     ))
# })
# output$view_filtered_tbl <- DT::renderDataTable({
#     shiny::validate(shiny::need(is.data.frame(local_data$analysis_data), message = 'No analysis loaded'))
#     sel = filter_summary()
#     local_data$analysis_data[sel,]
# })
# 
# get_ui = function(ii, vars = ''){
#     filter = shiny::isolate(local_filters[[paste0('filter', ii)]])
#     if(!is.list(filter)){ filter = list() }
#     tagList(
#         tagList(
#             tags$label(sprintf('Filter %d', ii), style = ifelse(ii == 1, '', 'margin-top: 15px;')),
#             div(
#                 # To make a box to wrap group inputs
#                 class = 'rave-grid-inputs',
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     selectInput(ns('filter_var_' %&% ii), 'Variable', choices = vars, selected = get_val(filter, 'var', default = NULL))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     selectInput(ns('filter_op_' %&% ii), 'Operator', choices = c('=', '!=', '>', '>=', '<', '<=', 'in', 'not in', 'between'), selected = get_val(filter, 'op', default = '='))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     textInput(ns('filter_val_' %&% ii), 'Value', value = get_val(filter, 'val', default = NULL))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     uiOutput(ns('filter_msg_' %&% ii))
#                 )
#             )
#             
#         )
#     )
# }
# 
# get_operator = function(op){
#     switch (op,
#             '=' = '%s == %s',
#             'in' = '%s %%in%% %s',
#             'between' = '%s %%within%% %s',
#             'not in' = '!%s %%in%% %s',
#             {
#                 paste('%s', op, '%s')
#             }
#     )
# }
# 
# filter_data = function(dat, op, val){
#     if( is.numeric(dat) && is.character(val) ){
#         val = parse_svec(val, sort = FALSE, unique = FALSE)
#     }
#     expr = get_operator(op)
#     expr = sprintf(expr, 'dat', deparse(val))
#     sel = rlang::eval_tidy(rlang::parse_expr(expr), data = list(dat = dat))
#     sel
# }
# 
# get_filter_results = function(ii){
#     filter = local_filters[[paste0('filter', ii)]]
#     if(!is.data.frame(local_data$analysis_data) || !is.list(filter) || !isFALSE(filter$failed)){ return(NULL) }
#     var = filter$var; op = filter$op; val = filter$val
#     dat = local_data$analysis_data[[var]]
#     if( is.numeric(dat) ){
#         val = parse_svec(val)
#     }
#     sel = filter_data(dat, op, val)
#     sel[is.na(sel)] = FALSE
#     sel
# }
# add_filter_observer = function(ii){
#     
#     
#     local({
#         observe({
#             n_filters = local_filters$filter_count
#             if(!is.data.frame(local_data$analysis_data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
#             var = input[[sprintf('filter_var_%d', ii)]]; op = input[[sprintf('filter_op_%d', ii)]]; val = input[[sprintf('filter_val_%d', ii)]]
#             var %?<-% ''; op %?<-% '='; val %?<-% ''
#             val_txt = val
#             # Do checks
#             msg = ''
#             failed = FALSE
#             if( !var %in% shiny::isolate(local_data$all_vars) ){
#                 msg = 'Variable not found'
#                 failed = TRUE
#             }else{
#                 dat = shiny::isolate({ local_data$analysis_data[[var]] })
#                 if( is.numeric(dat) ){
#                     val = parse_svec(val)
#                     if( !length(val) || any(is.na(val)) ){
#                         msg = 'Value is blank or contains NA'
#                         failed = TRUE
#                     }
#                 }
#                 if( !failed ){
#                     sel = filter_data(dat, op, val)
#                     n_na = sum(is.na(dat[sel]))
#                     n_sel = sum(sel, na.rm = TRUE)
#                     msg = sprintf('%d of %d selected (%d NAs)', n_sel, length(sel), n_na)
#                     if(n_sel == 0){
#                         msg = 'No data selected'
#                         failed = TRUE
#                     }
#                 }
#             }
#             
#             re = list(
#                 var = var, op = op, val = val_txt, failed = failed, msg = msg
#             )
#             local_filters[[paste0('filter', ii)]] = re
#         })
#         
#         output[[sprintf('filter_msg_%d', ii)]] = shiny::renderUI({
#             n_filters = shiny::isolate(local_filters$filter_count)
#             if(!is.data.frame(local_data$analysis_data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
#             
#             filter = local_filters[[paste0('filter', ii)]]
#             if(!is.list(filter)){ return() }
#             
#             col = ifelse( isTRUE(filter$failed) , 'red', 'grey' )
#             filter$msg %?<-% ''
#             htmltools::span(style = col2hex(col, prefix = 'color:#'), filter$msg)
#         })
#     })
#     
# }
# 
# # Add/remove filters
# observeEvent(input$filter_add, {
#     n_filters = shiny::isolate(local_filters$filter_count) + 1
#     n_observers = shiny::isolate(local_filters$filter_observers)
#     local_filters$filter_count = n_filters
#     # Check if observers are needed
#     if( n_filters > n_observers ){
#         add_filter_observer( n_filters )
#         local_filters$filter_observers = n_filters
#     }
# })
# observeEvent(input$filter_minus, {
#     n_filters = shiny::isolate(local_filters$filter_count) - 1
#     local_filters$filter_count = max(n_filters, 0)
# })
# 
# # summarise filters
# filter_summary = function(){
#     n_filters = shiny::isolate(local_filters$filter_count)
#     nrows = shiny::isolate({
#         re = 0
#         if(is.data.frame(local_data$analysis_data)){
#             re = nrow(local_data$analysis_data)
#         }
#         re
#     })
#     filters = shiny::isolate({
#         res = rep(TRUE, nrows)
#         for(ii in seq_len(n_filters)){
#             fil = get_filter_results( ii )
#             res = res & fil
#         }
#         res
#     })
#     filters
# }
# 
# observeEvent(local_filters$filter_count,{
#     print(sum(filter_summary()))
# })
