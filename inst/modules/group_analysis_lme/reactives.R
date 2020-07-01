input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

brain_proxy =  threeBrain::brain_proxy('lme_3dviewer_widget', session = session)


cat2_timestamp <- function() {
    t0 <- proc.time()[3]
    last_time = t0
    return(function(lbl, level='DEBUG') {
        .t = proc.time()[3]
        tick = .t - last_time
        last_time <<- .t
        elapsed = .t - t0
        dipsaus::cat2(lbl, '\t tick ', round(tick,1), ' sec\tTotal: ', round(elapsed,1), ' sec', level=level)
    })
}
group_analysis_cat2t <- cat2_timestamp()

model_params <- dipsaus::fastmap2()
    
local_data %?<-% reactiveValues(
    # Full data has two parts: local_data$analysis_data_raw, and local_data$additional_data
    # together makes analysis_data
    analysis_data_raw = NULL,
    additional_data = NULL,
    analysis_data = NULL,
    collapsed_data = NULL,
    
    omnibus_plots_color_palette = NULL,
    omnibus_plots_plot_aesthetics = NULL,
    
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
    
    show_by_electrode_results_rows_selected = NULL,
    omnibus_plots_legend_location = 'topleft'
)
local_filters = reactiveValues(
    filter_count = 0,
    filter_observers = 0
)


observeEvent(input$omnibus_plots_legend_location, {
    local_data$omnibus_plots_legend_location = input$omnibus_plots_legend_location
})

observeEvent(input$show_by_electrode_results_rows_selected, {
    group_analysis_cat2t('Detected rows selected')
    local_data$show_by_electrode_results_rows_selected = input$show_by_electrode_results_rows_selected
})

observeEvent(input$omnibus_plots_color_palette, {
    local_data$omnibus_plots_color_palette = input$omnibus_plots_color_palette
})

observeEvent(input$omnibus_plots_plot_aesthetics, {
    local_data$omnibus_plots_plot_aesthetics = input$omnibus_plots_plot_aesthetics
})

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
    
    local_data$omnibus_plots_time_range = time_range
    updateSliderInput(session, 'omnibus_plots_time_range', min = time_range[[1]], 
                      max=time_range[[2]], value=time_range)
    
    nms = names(local_data$analysis_data_filtered)
    
    usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
    dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
    
    # DV
    dvs = nms[
        sapply(nms, function(x) any(startsWith(x, usual_dvs)))
    ]
    # IVs
    local_data$number_of_subjects = length(unique(local_data$analysis_data_filtered$Subject))
    
    # Random effects
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
    
    # grab any ROI variables
    var_roi_avail <- nms[startsWith(nms, RAVE_ROI_KEY)]
    var_fe_avail <- nms[! nms %in% c(dvs, dnu, var_roi_avail)]
    
    var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
    
    current_roi_selected = model_params$roi_variable
    if (!isTRUE(current_roi_selected %in% var_roi_avail)) {
        print(paste('ROIs available: ', paste0(var_roi_avail, collapse='|')))
        current_roi_selected = character(0)
        updateSelectInput(session, 'model_roi_variable', choices = var_roi_avail)
    }
    
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

observeEvent(input$omnibus_plots_time_range, {
    local_data$omnibus_plots_time_range = input$omnibus_plots_time_range
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
    # dipsaus::cat2('Groups changing ' %&% length(input$cond_group), level = 'INFO')
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

build_lme_formula_string = function(..., exclude = NULL){
    
    if(!isTRUE(nrow(local_data$analysis_data_filtered) > 1)) {
        warning('data not available...')
        return('y ~ X')
    }
    
    ._ROI_TYPES = c('Stratify (Random+Fixed)', 'All possible ITX (Random+Fixed)',
                    'Random effect only', 'Average electrodes w/n ROI',
                    'Filter Only')
    
    dv = local_data$var_dependent
    iv = local_data$var_fixed_effects
    re = local_data$var_random_effects
    
    # fixed effects are easy, except we need to make sure TimeWindow comes after ConditionGroup. This
    # is quickly done by just sorting
    iv %<>% sort
    
    # random effects are harder because of potential nesting of electrode in subject
    re_str = ''
    
    nsub = length(unique(local_data$analysis_data_filtered$Subject))
    if(nsub > 1) {
        re_str = '(1|Subject/Electrode)'
        
        # check ROI variable
        if(!is.null(model_params$roi_variable)) {
            if(isTRUE(model_params$how_to_model_roi %in% ._ROI_TYPES[1:3])) {
                re_str = '(1|Subject/ROI/Electrode)'
            } else if (isTRUE(model_params$how_to_model_roi %in% ._ROI_TYPES[4])) {
                re_str = '(1|Subject/ROI)'
            } else {
                #do nothing
            }
        }
        
        if(length(iv) < 1) {
            re_str <- stringr::str_remove(re_str,'/Electrode')
        }
        
        # if we're stratifying the result by ROI, then add it to the fixed-effects list
        if(isTRUE(model_params$how_to_model_roi %in% ._ROI_TYPES[1:2])) {
             iv %<>% add_term('ROI')
        }
        
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
    
    # let's also check if freesurfer label is in fe_str. If so, remove Electrode from random effects
    fe_str = paste(iv, collapse='*')
    
    # if the only fixed effect is ROI, don't let ROI be a random effect, right?
    if(fe_str == 'ROI') {
        re_str <- stringr::str_remove(re_str,'/ROI')
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


build_roi_levels <- function() {
    if(!isTRUE(length(local_data$analysis_data_filtered)>1)) {
        return(character(0))
    }

    mrv = input$model_roi_variable
    adf = local_data$analysis_data_filtered
    
    if(nchar(mrv) > 0) {
        lvls = levels(factor(adf[[RAVE_ROI_KEY %&% mrv]]))
        
        if(isTRUE(input$roi_ignore_hemisphere)) {
            lvls %<>% remove_hemisphere_labels
        }
        
        if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
            lvls %<>% remove_gyrus_sulcus_labels
        }
    }
    
    return(lvls)
}

observeEvent(input$model_roi_variable, {
    if(length(local_data$analysis_data_filtered)<1) return()
    
    mrv = input$model_roi_variable
    if(nchar(input$model_roi_variable) > 0) {
        lvls = build_roi_levels() #levels(factor(local_data$ analysis_data_filtered[[RAVE_ROI_KEY %&% mrv]]))
        updateSelectInput(session, 'filter_by_roi', choices = lvls, selected=lvls)
    }
    model_params$roi_variable = mrv
    model_params$roi_filter = lvls
})

observeEvent(input$filter_by_roi, {
    model_params$roi_filter <- input$filter_by_roi
})

observeEvent(input$how_to_model_roi, {
    model_params$how_to_model_roi = input$how_to_model_roi
    rebuild_model_formula()
})


observeEvent(input$roi_ignore_hemisphere, {
    sel = input$filter_by_roi
    # sel = ..model_params$roi_filter

    if(isTRUE(input$roi_ignore_hemisphere)) {
        # we are currently NOT ignoring HEMI, but we'll start now
        sel <- unique(remove_hemisphere_labels(sel))
        
        updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
    } else {
        # this means we are currently ignoring HEMI, but we shall do so no longer
        new_lvls = build_roi_levels()
        
        sel = new_lvls[unlist(sapply(sel, function(s) which(endsWith(new_lvls, s))))]
        
        updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
    }
})

observeEvent(input$roi_ignore_gyrus_sulcus, {
    sel = input$filter_by_roi
    # sel = ..model_params$roi_filter
    
    if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
        # we are currently NOT ignoring G/S, but we'll start now
        sel <- unique(remove_gyrus_sulcus_labels(sel))
        updateSelectInput(session, 'filter_by_roi',
                          choices=build_roi_levels(), selected = sel)
    } else {
        # this means we are currently ignoring G/S, but we shall do so no longer
        new_lvls = build_roi_levels()
        
        if(isTRUE(input$roi_ignore_hemisphere)) {
            # if we're ignoring HEMI right now, then we don't need to do the additional check for startsWith
            sel = new_lvls[unlist(sapply(sel, function(s) which(endsWith(new_lvls, s))))]
        } else {
            sel <- new_lvls[unlist(sapply(sel, function(s) {
                which(startsWith(new_lvls, substr(s, 1,2)) & 
                    endsWith(new_lvls, remove_hemisphere_labels(s)))
            }))]
        }
        
        updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
    }
})

observeEvent(input$run_analysis, {
    if(exists('.__DEBUG__')) {
        assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
        assign('..rm_windows', input$multi_window_analysis, envir=globalenv())        
        assign('..model_params', model_params, envir=globalenv())        
        
        # multi_window = ..rm_windows
        # local_data = ..local_data
        # model_params = ..model_params
        # cond_group = list(list('group_conditions' = 'Dynamic'), list('group_conditions' = 'Static'))
        # cond_group = list(list('group_conditions' = c('Dynamic', 'Static')))
        
        # cond_group = list(
            # list(group_name = 'A-only', group_conditions = c('drive_a','last_a', 'meant_a', 'known_a')),
            # list(group_name = 'V-only', group_conditions = c('drive_v','last_v', 'meant_v', 'known_v')),
            # list(group_name = 'AV', group_conditions = c('drive_av','last_av', 'meant_av', 'known_av'))
        # )
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
    
    
    # ldf$VAR_IS_ROI_freesurferlabel %<>% str_replace_all(FREESURFER_ANAT_REPLACE_STRING)
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
    
    dipsaus::cat2(paste('assigning ', local_data$var_dependent, ' to y'), level='INFO')
    local_data$var_dependent_label = pretty_string(local_data$var_dependent)
    subset_data$y = subset_data[[local_data$var_dependent]]
    
    # creating ROI variable
    roi = paste(model_params$roi_variable)
    if(isTRUE(nchar(roi) > 1)) {
        dipsaus::cat2(paste('creating ROI ', roi , ' to ROI'), level='INFO')
        subset_data$ROI = subset_data[[RAVE_ROI_KEY %&% roi]]
        roi = 'ROI'
        
        lvls = model_params$roi_filter
        if(length(lvls) < 1) {
            showNotification('No ROI levels specified... ignoring ROI variable', type='warning', id='LME_NOTES')
        } else {
            # check if we need to filter the ROIs
            if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
                subset_data[[roi]] %<>% remove_gyrus_sulcus_labels
            }
            if(isTRUE(input$roi_ignore_hemisphere)) {
                subset_data[[roi]] %<>% remove_hemisphere_labels
            }
            #TODO should look into sample size issues here
            subset_data <- subset_data[subset_data[[roi]] %in% lvls,]
        }
    }
    
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
    collapse_formula = as.formula(sprintf('y ~ %s', re %?&% fe %?&% roi))
    
    collapsed_data <- aggregate(collapse_formula, data=subset_data, FUN=mean)
    
    # we need to factor time window to make sure the comparisons have the appropriate order (and baseline)
    # do this here before the data are saved into local_data
    if('TimeWindow' %in% names(collapsed_data)) {
        collapsed_data$TimeWindow %<>% factor(levels=window_names_in_order)
    }
    local_data$collapsed_data = collapsed_data
    local_data$agg_over_trial = aggregate(as.formula('y ~ Time + Subject + Electrode' %?&% fe %?&% roi),
                                          data = local_data$over_time_data, FUN=mean) %>%
                                do_aggregate(as.formula('y ~ Time' %?&% fe %?&% roi), .fast_mse)
    
    # for the per-electrode plots, we're going back to the over_time data because we need the individual trials
    if(isTRUE(input$multi_window_is_active)) {
        windows = lapply(multi_window, function(mw) unlist(mw$analysis_window))
    } else {
        windows = analysis_window
    }
    # fe
    by_el_data = local_data$over_time_data %>%
        subset((.)$Time %within_any% windows) %>% 
        do_aggregate(as.formula('y ~ Condition + Trial + Subject + Electrode + Project' %?&% fe %?&% roi), mean)
    
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
    
    ### let's clean up the doubled parens that can happen for Intercept tests
    names(by_el_results) %<>% stringr::str_replace_all(c('\\(\\('='(', '\\)\\)'=')'))
    
    by_el_results[names(by_el_results) %>% startsWith('p(')] %<>% lapply(round_pval)
    by_el_results[names(by_el_results) %>% startsWith('t(')] %<>% lapply(round_test_statistic)
    by_el_results[names(by_el_results) %>% startsWith('F(')] %<>% lapply(round_test_statistic)
    by_el_results[names(by_el_results) %>% startsWith('X2(')] %<>% lapply(round_test_statistic)
    by_el_results[names(by_el_results) %>% startsWith('m(')] %<>% lapply(round, 2)
    
    #de-factor the electrode label
    by_el_results$Electrode <-  as.numeric(as.character(by_el_results$Electrode))
    
    # create a jittered subject value that can be used for plotting
    by_el_results$`jitter(Subject)` = jitter(as.integer(by_el_results$Subject))
    
    yi <- which(startsWith(names(by_el_results), 'F('))
    if(any(yi)) {
        .yvar = names(by_el_results)[yi[1]]
    } else {
        .yvar = names(by_el_results)[min(ncol(by_el_results), 5)]
    }
    
    
    # udate the UI for post hoc plots
    updateSelectInput(session, 'post_hoc_plot_xvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = 'jitter(Subject)')
    updateSelectInput(session, 'post_hoc_plot_yvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = .yvar)
    updateSelectInput(session, 'post_hoc_plot_zvar',
                      choices = c('None', names(by_el_results), 'CUSTOM'))
    
    local_data$by_electrode_results = by_el_results
    
    fo <-  str_replace_all(input$model_formula, local_data$var_dependent, 'y')
    # fo <- "y ~ ConditionGroup*ROI + (1|Subject/Electrode)"
    
    # fo <- str_replace_all('Pct_Change_Power_1stWord ~ TimeWindow + (1|Subject/Electrode)', 'Pct_Change_Power_1stWord', 'y')
    # fo = 'y ~ freesurferlabel + (1|Subject)'
    # fo = 'y ~ ConditionGroup + (1|Subject/Electrode) + (0+ConditionGroup|Subject) + (1|Condition)'
    fo %<>% as.formula
    
    tryCatch({
        .rhs <- colnames(attr(terms(fo), 'factors'))
        n_fixed_effects = sum(stringr::str_detect(.rhs, '\\|', negate = T))
        
        # check factor sample sizes
        # for(fe in local_data$var_fixed_effects) {
        #     # fe = local_data$var_fixed_effects[1]            
        #     nl = length(unique(collapsed_data[[fe]]))
        #     to_keep = names(which(table(collapsed_data[[fe]]) >= 5))
        #     collapsed_data[[fe]] %<>% factor(levels = sort(to_keep))
        #     if(length(to_keep) < nl) {
        #         showNotification('Dropping ' %&% (nl - length(to_keep)) %&%
        #                          ' levels of ' %&% fe %&% ' because cell counts < 5',
        #                          type = 'warning')
        #     }
        # }
        if(n_fixed_effects > 0) {
            lmer_results = lmerTest::lmer(fo,
                                          data=collapsed_data, na.action=na.omit)
            # summary(lmer(fo, data=collapsed_data))
        } else if(local_data$number_of_subjects > 1) {
            ## no fixed effects, just checking if activity > 0
            # lmerTest::lmer(fo, data=collapsed_data)
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
        
        showNotification(p('Model finished with errors, check console....'), duration = 5,
                         type = 'error', id = ns('noti'))
        
    })
    
    if(exists('.__DEBUG__')) {
        assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)),
               envir = globalenv())
    }
    
})

observeEvent(input$lme_3dviewer_widget_mouse_dblclicked, {
    # mouse_event = input$power_3d__mouse_dblclicked$event
    # object = input$power_3d__mouse_dblclicked$object
    
    .data <- input$lme_3dviewer_widget_mouse_dblclicked
    sbj = .data$subject
    el = .data$electrode_number
    if(!is.null(local_data$by_electrode_results)) {
        ber = local_data$by_electrode_results
        selected_row = which(ber$Subject == sbj & ber$Electrode == el)
        dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
        DT::selectRows(dtp, selected_row)
        local_data$show_by_electrode_results_rows_selected = NULL
    }
    
    showNotification(p('Selected ', sbj, el), type = 'message', id = ns('lme_3dviewer_widget__mouse'))
})
