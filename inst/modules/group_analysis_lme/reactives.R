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

model_params <- fastmap::fastmap()
    
local_data %?<-% reactiveValues(
    analysis_data_raw = NULL,
    
    lmer_results=NULL,
    
    most_recent_second_factor_groups = NULL,
    
    omnibus_plots_color_palette = NULL,
    omnibus_plots_plot_aesthetics = NULL,
    omnibus_plots_time_range = c(0,1),
    omnibus_plots_legend_location = 'topleft',
    
    show_by_electrode_results_rows_selected = NULL
)

observeEvent(input$effect_overview_plot_click, {
    y <- input$effect_overview_plot_click$y
    
    rod = model_params$get('results_overview_data')
    
    # if(is.null(y)) {
    #     h <- estimate_height_of_eo_plot(components=TRUE)
    #     y <- input$effect_overview_plot_click$coords_css$y
    # 
    #     # print(input$effect_overview_plot_click$coords_css)
    #     
    #     # mai <- local_data$rod_plot_details$mai
    #     # fin <- local_data$rod_plot_details$fin
    #     
    #     # print(local_data$rod_plot_details)
    #     
    #     
    #     # does this value depend on screen resolution?
    #     # inch2pt <- 70.28569
    #     # upper_margin <- ceiling(mai[3]*inch2pt)
    #     # print('UM: ' %>% paste0(upper_margin))
    #     
    #     colobar <- 0.393701 * 2 * inch2pt
    #     
    #     if(y <= upper_margin) {
    #         print("in upper margin")
    #     } else (
    #         y = (y - upper_margin) / 25
    #     )
    #     
    #     y = NULL
    # }
    # 
    
    if(!is.null(y)) {
        rn = rownames(rod)[1 + (nrow(rod) - ceiling(y-0.49))]
        # print('Selected: |' %&% rn %&% '|')
        
        # see if they selected an ROI
        ber = model_params$get('by_electrode_results')
        if(stringr::str_trim(rn, side = 'right') %in% unique(ber$ROI)) {
            selected = which(ber$ROI == stringr::str_trim(rn, side = 'right'))
            shiny::showNotification(sprintf("ROI %s selected. Replacing subset plots", rn), id='ROD')
        } else {
            tokens <- stringr::str_split(rn, ' ', n=2)[[1]]
            sbj = str_remove_all(tokens[1], ' ')
            el = as.numeric(stringr::str_remove_all(tokens[2], ' '))
            selected = which(ber$Subject == sbj & ber$Electrode == el)
            shiny::showNotification(sprintf("Electrode %s selected. Replacing subset plots", rn), id='ROD')
        }
        
        dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
        DT::selectRows(dtp, NULL)
        # set the local copy to NULL, as the call to select will trigger an update
        local_data$show_by_electrode_results_rows_selected = NULL
        DT::selectRows(dtp, selected)
    }     
})

# double-click means add to the current selection
observeEvent(input$effect_overview_plot_dblclick, {
 
    y <- input$effect_overview_plot_dblclick$y
    
    # dipsaus::cat2('Clicked at: ', y, level = 'INFO')
    # print(str(input$effect_overview_plot_click))
    
    if(!is.null(y)) {
        rod = model_params$get('results_overview_data')
        
        rn = rownames(rod)[1 + (nrow(rod) - ceiling(y-0.49))]
        # print('Selected: |' %&% rn %&% '|')
        
        # see if they selected an ROI
        ber = model_params$get('by_electrode_results')
        if(stringr::str_trim(rn, side = 'right') %in% unique(ber$ROI)) {
            selected = which(ber$ROI == stringr::str_trim(rn, side = 'right'))
            shiny::showNotification(sprintf("ROI %s selected. Adding to subset plots", rn), id='ROD')
        } else {
            tokens <- stringr::str_split(rn, ' ', n=2)[[1]]
            sbj = str_remove_all(tokens[1], ' ')
            el = as.numeric(stringr::str_remove_all(tokens[2], ' '))
            selected = which(ber$Subject == sbj & ber$Electrode == el)
            shiny::showNotification(sprintf("Electrode %s selected. Adding to subset plots", rn), id='ROD')
        }
        
        dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
        DT::selectRows(dtp, NULL)
        selected = unique(c(local_data$show_by_electrode_results_rows_selected, selected))
        
        # set the local copy to NULL, as the call to select will trigger an update
        local_data$show_by_electrode_results_rows_selected = NULL
        DT::selectRows(dtp, selected)
    }    
})

# monitor the currently selected electrodes and let threeBrain know
observe({
    # sberrs <- local_data$show_by_electrode_results_rows_selected
    
    # if(length(sberrs)>0) {
        
        #TODO eventually this will uypdate teh value in 3dviewer with the "selected" column
        
        # sel = get_selected_subjel()
        # ctl <- brain_proxy$get_controllers()
        # 
        # 
        # rows = local_data$show_by_electrode_results_rows_selected
        # to_keep = model_params$get('by_electrode_results')[rows,c('Subject', 'Electrode')]
        # 
        # brain_proxy$set_focused_electrode(to_keep$Subject,to_keep$Electrode)
        
    
        # print(dput(to_keep))
        # # list('Focused Electrode' = list(
        # #     lbl='', sbjel_id
        # # )))
    # }
})


observeEvent(input$omnibus_plots_legend_location, {
    local_data$omnibus_plots_legend_location = input$omnibus_plots_legend_location
})

observeEvent(input$show_by_electrode_results_rows_selected, {
    # group_analysis_cat2t('Detected rows selected')
    local_data$show_by_electrode_results_rows_selected = input$show_by_electrode_results_rows_selected
})

observeEvent(input$omnibus_plots_color_palette, {
    local_data$omnibus_plots_color_palette = input$omnibus_plots_color_palette
})

observeEvent(input$omnibus_plots_plot_aesthetics, {
    local_data$omnibus_plots_plot_aesthetics = input$omnibus_plots_plot_aesthetics
})


# assignment IFF and value is not null
`safe_set<-` <- function(x, value) {
    if(!is.null(value)) {
        return (value)
    }
    
    return(x)
}

# this just scans the data, it does _not_ take into account the state of the GUI
get_available_raw_effects <- function() {
    if(! model_params$has('data_labels')) return (character(0))
        
    usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
    
    dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
    
    dl <- model_params$get('data_labels')
    
    startsWithAny <- function(x, prfx) any(startsWith(x, prfx))
    
    # dependent variables
    dvs = get_by(dl, function(x) sapply(x, startsWithAny, usual_dvs))
    
    # Fixed Effects
    
    # ROI vars available
    var_roi_avail <- get_by(dl, startsWith, RAVE_ROI_KEY)
    
    # IVs
    var_fe_avail <- dl[! dl %in% c(dvs, dnu, var_roi_avail)]

    # strip off the ROI key prefix for display purposes
    if(length(var_roi_avail)>1)
        var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
    
    return(
        list('dvs' = dvs,
        'var_roi_avail' = var_roi_avail,
        'var_fe_avail' = var_fe_avail
        )
    )
}

# when the raw data have changed, we need to update the GUI
observeEvent(local_data$analysis_data_raw, {
    if(is.null(local_data$analysis_data_raw$data)) return()
        
    try_ravebuiltins_debug(print_only=FALSE)
    
    raw <- local_data$analysis_data_raw
    
    RESERVED_VARS <- c('ROI', 'y')#, 'ConditionGroup', 'ConditionGroup2')
    
    if(any(RESERVED_VARS %in% raw$headers) ) {
      offenders <- get_by(RESERVED_VARS, `%in%`, raw$headers)
      
      showNotification("Data file contains reserved variable(s): %s. Please fix the data and try again (Reserved words are: %s).",
        str_collapse(offenders), str_collapse(RESERVED_VARS),
          type = 'error', duration = NULL, id='RWORD')
      return()
    } else {
        shiny::removeNotification(id='RWORD')
    }
    
    if(model_params$has('data')) {
      shiny::showNotification("You are trying to load new data. This will overwrite existing data."
      # (have you saved the results?). Proceed to overwrite existing data/results. Cancel to keep current data."
      , type = "warning", duration = 10)
    }

    # reset the model params and local_data here
    model_params$reset()
    # this is the main reactive trigger
    local_data$lmer_results <- NULL
    
    raw$data <- data.table::data.table(raw$data, key=c('Subject', 'Electrode'))
    
    nsub <- nrow(raw$data[, unique(Subject), by='Subject'])
    nelec <- nrow(raw$data[, unique(Electrode), by='Subject'])
        
    model_params$mset(
        'data' = raw$data,
        'data_labels' = raw$headers,
        "number_of_subjects" = nsub,
        "number_of_electrodes" = nelec
    )
    
    # configurations
    configs <- list()
    safe_set(configs) <- dipsaus::drop_nulls(raw$confs)[[1]]

    # first create some handy variables
    # DV
    usual_dvs = c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
    dnu = c('Time', 'uuid', 'Project', 'TrialIsOutlier')
    dvs = raw$headers[
        sapply(raw$headers, function(x) any(startsWith(x, usual_dvs)))
    ]
    
    # Random effects
    re_sel = c('Electrode')
    if(model_params$get("number_of_subjects", missing = 0) > 1) {
        re_sel %<>% c('Subject')
    }
    
    # ROI vars available
    var_roi_avail <- get_by(model_params$get('data_labels'), startsWith, RAVE_ROI_KEY)

    # IVs
    var_fe_avail <- with(raw, headers[! headers %in% c(dvs, dnu, var_roi_avail)])
    
    # strip off the ROI key prefix for display purposes
    var_roi_avail <- stringr::str_remove_all(var_roi_avail, RAVE_ROI_KEY)
    
    # Custom ROI Builder input panel
    rbv <- input$roi_builder_variable
    if(isTRUE(rbv %in% var_roi_avail)) {
        updateSelectInput(session, 'roi_builder_variable', choices = var_roi_avail, selected = rbv)
    }  else {
        updateSelectInput(session, 'roi_builder_variable', choices = var_roi_avail)
    }
    
    ### Model ROI selector
    
    # add in the CustomROI if viable
    if(any_custom_rois(input$roi_group)) {
        var_roi_avail = c("CustomROI", var_roi_avail)
    }
    
    # keep the current ROI choice if viable
    current_roi_selected = input$model_roi_variable
    if (!isTRUE(current_roi_selected %in% var_roi_avail)) {
        # print(paste('ROIs available: ', paste0(var_roi_avail, collapse='|'), "but you wanted", current_roi_selected))
        current_roi_selected = character(0)
    }
    updateSelectInput(session, 'model_roi_variable', selected = current_roi_selected, choices = var_roi_avail)
    
    # Custom ConditionGroup Builder input panel
    conditions = unique(raw$data$Condition)
    if(!length(conditions)){ conditions = '' }
    
    groups <- list()
    safe_set(groups) <- configs$GROUPS
    
    dipsaus::updateCompoundInput2(
        session, 'cond_group', ncomp = max(length(groups), 1), 
        initialization = list(
            group_conditions = list(choices = conditions )
        ), value = groups)
    
    ### model fixed effects depend on the group structure
    # on first load, if there is a ConditionGroup, assume they want it in the model
    if(length(groups)>1){
        updateSelectInput(session, 'model_fixed_effects',
            choices = c('ConditionGroup', var_fe_avail),
            selected = 'ConditionGroup')
    } else {
        dipsaus::cat2('No groups')
        updateSelectInput(session, 'model_fixed_effects', choices = var_fe_avail)
    }
    
    updateSelectInput(session, 'model_random_effects', choices = var_fe_avail, selected = re_sel)
    updateSelectInput(session, 'model_dependent', selected = dvs[1], choices = dvs)
    
    #### now set all the time variables
    time_range = range(raw$data$Time, na.rm = TRUE)
    analysis_window = c(max(0, min(time_range)), min(1, max(time_range)))
    safe_set(analysis_window) <- raw$confs[[1]]$ANALYSIS_WINDOW
    safe_set(analysis_window) <- raw$confs[[1]]$analysis_window
    
    updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
                      max=time_range[[2]], value=analysis_window)
    
    # here we need to check the number of groups that area assigned (> 1 means create a factor) 
    dipsaus::updateCompoundInput2(session, 'multi_window_analysis', 
        initialization = list(
            analysis_window = list(min = time_range[1], max = time_range[2], value = analysis_window),
            window_is_active = list(value=FALSE)
        ))
    
    # the plot time range can be configured without re-running the LME,
    # so it should be reactive
    local_data$omnibus_plots_time_range = time_range
    updateSliderInput(session, 'omnibus_plots_time_range', min = time_range[[1]], 
                      max=time_range[[2]], value=time_range)
  
    # we made it this far, open the appropriate input panels (and close the inappropriate ones!)
    sapply(c('Build ROIs','Build first grouping factor', 'Single time window analysis', 'Build model'),
        rave::open_tab, module_id = 'group_analysis_lme')
    
    sapply(c('Data import', 'Configure group plots', 'Plot post-hoc variables',
        'Configure post-hoc plot', 'Configure/Export electrode-level results', 'Export hi-res plot'),
      
        rave::close_tab, module_id = 'group_analysis_lme')
})

observe({

})

# when the ROI Column changes, we need to clear out the selected ROI Groupings and re-populate the choices. 
observeEvent(input$roi_builder_variable, {
    if(!model_params$has('data')) return()
    
    # try_ravebuiltins_debug(str="updating ROI builder var", tag='ROI', print_only=TRUE)
    
    roi_groups <- input$roi_group
    
    data <- model_params$get('data')
    varname <- RAVE_ROI_KEY %&% input$roi_builder_variable
        if(!is.factor(data[[varname]])) {
            data[[varname]] %<>% factor
            model_params$set('data', data)
        }
    lvls = levels(data[[varname]])
    
    # try_ravebuiltins_debug(str="available levels: " %&% paste0(collapse='|', lvls), tag='ROI', print_only=TRUE)
    
    # this will trigger a change to the ROI groupings, which we can observe and update the model statement as needed    
    dipsaus::updateCompoundInput2(
        session, 'roi_group', ncomp = max(length(roi_groups), 1), 
        initialization = list(
            roi_grouping = list(choices = lvls )
        ), value = roi_groups)
    
})

any_sublist_values <- function(l, sl) {
  res <- FALSE
  ii=1
  while(!res && ii <= length(l)) {
    res <- length(l[[ii]][[sl]]) > 0
    ii = ii+1
  }
  
  return(res)
}

any_custom_rois <- function(roi_group) {
    any_sublist_values(roi_group, 'roi_grouping')
    # sum(unlist(lapply(roi_group, function(g) length(g$roi_grouping) > 0))) > 0
}

# note that here we check > 1. It's not a comparison if #grp == 1
any_custom_condition_groups <- function(condition_grouping, sl='group_conditions') {
    sum(unlist(lapply(condition_grouping, function(cg) length(cg[[sl]]) > 0))) > 1
}

get_available_raw_rois <- function(data_labels) {
    data_labels %?<-% model_params$get('data_labels')
    
    stringr::str_remove_all(get_by(data_labels, startsWith, RAVE_ROI_KEY), RAVE_ROI_KEY)
}

observeEvent(input$roi_group, {
    if(! model_params$has('data')) return()
    
    var_roi_avail <- get_available_raw_rois()
    mrv <- input$model_roi_variable
    
    if(any_custom_rois(input$roi_group)) {
        # there is a custom ROI grouping available, add it to the ROI choices for modeling
        if(nchar(mrv) < 1) mrv = 'CustomROI'
        var_roi_avail = c('CustomROI', var_roi_avail)
        
    } else {
        if(isTRUE(mrv == "CustomROI")) mrv = character(0)
    }
    
    updateSelectInput(session, 'model_roi_variable', choices = var_roi_avail, selected = mrv)
    
    shiny::isolate(rebuild_filter_list())
})

observeEvent(input$single_analysis_window, {
    if(isTRUE(input$single_analysis_window)) {
        updateCheckboxInput(session, 'multi_window_is_active', value = FALSE)
        
        # we don't need to call this, multi_window event handler will
        # update_available_effects()
    }
})

# observeEvent(input$analysis_window, {
#     local_data$analysis_window = input$analysis_window
# })

observeEvent(input$omnibus_plots_time_range, {
    local_data$omnibus_plots_time_range = input$omnibus_plots_time_range
})

observeEvent(input$multi_window_is_active, {
    if(isTRUE(input$multi_window_is_active)) {
        updateCheckboxInput(session, 'single_analysis_window', value=FALSE)
    }   
    
    update_available_effects()
})

update_available_effects <- function(auto_select="") {
    if(! model_params$has('data_labels')) return()
    
    try_ravebuiltins_debug('updating effects', 'UAE')
    
    # Currently selected effects
    mre = input$model_random_effects
    mfe = input$model_fixed_effects
    
    # Get the fixed effects available in the raw data
    eff <- get_available_raw_effects()
    
    # we need to check if we should add in fixed effects of TimeWindow and ConditionGroup
    if(any_custom_condition_groups(input$cond_group)) {
        eff$var_fe_avail %<>% add_term('ConditionGroup')

        if('ConditionGroup' %in% auto_select) {
            # put CG1 at the beginning
            mfe <- c('ConditionGroup', mfe)
        }
    } else {
        eff$var_fe_avail %<>% remove_term('ConditionGroup')
    }
    
    if(any_custom_condition_groups(input$second_factor, 'grouping')) {
        eff$var_fe_avail %<>% add_term('ConditionGroup2')
        
        if('ConditionGroup2' %in% auto_select) {
            # put CG2 at the end
            mfe <- c(mfe, 'ConditionGroup2')
        }
    } else {
        eff$var_fe_avail %<>% remove_term('ConditionGroup2')
    }
    
    # if they've checked multi_window then we'll add the term
    # we can check later if they've given us bad input
    if(isTRUE(input$multi_window_is_active)) {
        eff$var_fe_avail %<>% add_term('TimeWindow')
        
        if('TimeWindow' %in% auto_select) {
            mfe <- c(mfe, 'TimeWindow')
        }
    } else {
        eff$var_fe_avail %<>% remove_term('TimeWindow')
    }
    
    # ensure required random effects are loaded
    mre %<>% add_term('Electrode')
    if(model_params$get("number_of_subjects", missing = 0) > 1) {
        mre %<>% add_term('Subject')
    } else {
        mre %<>% remove_term('Subject')
    }
    
    # if we're using ROI in the model, add it as am available fixed effect
    if(input$how_to_model_roi %in% RAVE_ROI_TYPES[1:2]) {
        eff$var_fe_avail %<>% c('ROI')
        
        if('ROI' %in% auto_select) {
            mfe %<>% add_term('ROI')
        }
    }
    
    updateSelectInput(session, 'model_random_effects',
                      choices = eff$var_fe_avail,
                      selected = mre[mre %in% eff$var_fe_avail])
    
    updateSelectInput(session, 'model_fixed_effects',
                      choices = eff$var_fe_avail,
                      selected = mfe[mfe %in% eff$var_fe_avail])
}

observeEvent(input$cond_group, ignoreNULL = FALSE, {
    if(!is.null(input$cond_group)) {
        update_available_effects(auto_select='ConditionGroup')
    }
    
    # if there are custom condition groups, 
    # then we can enable the second factor, else make sure it's empty
    shiny::isolate({
        second_factor <- input$second_factor
        
        if(!any_custom_condition_groups(input$cond_group)) {
            available_conditions <- character(0) 
        } else {
            available_conditions <- unlist(lapply(input$cond_group, function(cg) unlist(cg$group_conditions)))
        }
        dipsaus::updateCompoundInput2(
            session, 'second_factor', ncomp = max(length(second_factor), 1), 
            initialization = list(
                grouping = list(choices = unname(available_conditions))
            ), value = second_factor)
    })
    
})

observeEvent(input$second_factor, {
    second_factor <- input$second_factor
    
    # check if there was a change to a group
    # (no immediate needs if there is just a change to a label)
    if(is.null(local_data$most_recent_second_factor_groups)) {
        local_data$most_recent_second_factor_groups <- lapply(second_factor, function(sf) {
            unlist(sf$grouping)
        })

        if(any_custom_condition_groups(second_factor, 'grouping')) {
            # this is really unlikely
            update_available_effects(auto_select='ConditionGroup2')
        }
            
    } else {
        # check for equality between most recent SFG and the current SFG
        same <- (length(second_factor) == length(local_data$most_recent_second_factor_groups))
        ii <- 1
        while(same && ii <= length(second_factor)) {
            same <- setequal(
                unlist(second_factor[[ii]]$grouping),
                unlist(local_data$most_recent_second_factor_groups[[ii]])
            )
            
            ii <- ii+1
        }
        
        if(!same) {
            local_data$most_recent_second_factor_groups = lapply(second_factor, function(sf) {
                unlist(sf$grouping)
            })
            update_available_effects(auto_select='ConditionGroup2')
        }
    }
})

rebuild_model_formula <- function(..., exclude=NULL) {
    if(! all(model_params$has(c('number_of_subjects', 'data'))) ) {
        try_ravebuiltins_debug("requested model rebuild before data were loaded...")
        return (FALSE)
    } 
  
    dv = input$model_dependent
    iv = input$model_fixed_effects
    re = input$model_random_effects
    
    roi_var <- input$model_roi_variable
    roi_model <- input$how_to_model_roi
    
    # need isTRUE here because roi_var may be NULL
    if(length(iv) < 1 && isTRUE((roi_var %in% RAVE_ROI_TYPES[1:3]) )) {
        shiny::showNotification('ROI type: ' %&% roi_model %&% " not suported when there no fixed effects. Maybe you wanted ROI=Filter Only?")
        shiny::validate(shiny::need(FALSE, message='Improper ROI | fixed effect specification'))
    }
    
    # fixed effects are easy, except we need to make sure TimeWindow comes after ConditionGroup. This
    # is quickly done by just sorting
    iv %<>% sort
    
    # random effects are harder because of potential nesting of electrode in subject
    re_str = ''
    
    if(model_params$get('number_of_subjects') > 1) {
        re_str = '(1|Subject/Electrode)'
        
        # check ROI variable
        if(!is.null(roi_var)) {
            if(isTRUE(roi_model == RAVE_ROI_TYPES['ROI_TYPE_III'])) {
                re_str = '(1|Subject/ROI/Electrode)'
            } else if (isTRUE(roi_model %in% RAVE_ROI_TYPES['ROI_TYPE_IV'])) {
                re_str = '(1|Subject/ROI)'
            } else {
                #do nothing
            }
        }
        
        if(length(iv) < 1) {
            re_str <- stringr::str_remove(re_str,'/Electrode')
        }
        
        # if we're stratifying the result by ROI, then add it to the fixed-effects list
        if(isTRUE(roi_model %in% RAVE_ROI_TYPES[1:2])) {
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
    
    # let's also check if freesurfer label is in fe_str -- don't let it get in there
    # this should just be handled as an ROI
    
    fe_str = paste(iv, collapse='*')
    
    # if the only fixed effect is ROI, don't let ROI be a random effect, right?
    if(fe_str == 'ROI') {
        re_str <- stringr::str_remove(re_str,'/ROI')
    }
    
    # now build up the actual formula string, DV ~ FE + RE
    lme_string = dv
    if(isTRUE(nchar(fe_str) > 0)) {
        lme_string %<>% paste('~', fe_str)
    } else {
        lme_string %<>% paste('~ 1')
    }
    
    if(isTRUE(nchar(re_str) > 0)) {
        lme_string %<>% paste('+', re_str)
    }
    
    updateTextAreaInput(session, 'model_formula', value = lme_string)
}

observeEvent(input$model_random_effects, rebuild_model_formula(), ignoreNULL = FALSE)
observeEvent(input$model_fixed_effects, rebuild_model_formula(), ignoreNULL = FALSE)
observeEvent(input$model_dependent, rebuild_model_formula())

build_roi_levels <- function() {
    if(! model_params$has('data')) return (character(0))
    
    mrv = input$model_roi_variable
    
    lvls = character(0)
    
    if(nchar(mrv) > 0) {
        # if it's a custom ROI, grab the levels directly from the GUI
        if(mrv == 'CustomROI') {
            rg <- input$roi_group
            # assign('rg', rg, envir=globalenv())
            # fix the names if we need too
            for (gi in seq_along(rg)) {
                if(rg[[gi]]$roi_group_name == "") {
                    rg[[gi]]$roi_group_name = "ROI_" %&% LETTERS[gi]
                }
            }
            # g$roi_group_name is a list, so we're forcing it to string with paste
            lvls = paste(sapply(rg, '[[', 'roi_group_name'))
        } else {
            df = model_params$get('data')
            roi_var <- RAVE_ROI_KEY %&% mrv
            if(!is.factor(df[[roi_var]])) {
                df[[roi_var]] %<>% factor
                model_params$set('data', df)
            }
            
            lvls = levels(df[[RAVE_ROI_KEY %&% mrv]])
        }
        
        if(isTRUE(input$roi_ignore_hemisphere)) {
            lvls %<>% remove_hemisphere_labels
        }
        
        if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
            lvls %<>% remove_gyrus_sulcus_labels
        }
    }
    
    return(lvls)
}

rebuild_filter_list <- function() {
    lvls = build_roi_levels()
    updateSelectInput(session, 'filter_by_roi', choices = lvls, selected=lvls)
}

observeEvent(input$model_roi_variable, {
    rebuild_filter_list()
})


observeEvent(input$how_to_model_roi, {
    update_available_effects(auto_select = 'ROI')
    
    rebuild_model_formula()
})


observeEvent(input$roi_ignore_hemisphere, {
    sel = input$filter_by_roi
    
    if(isTRUE(input$roi_ignore_hemisphere)) {
        # we are currently NOT ignoring HEMI, but we'll start now
        sel <- unique(remove_hemisphere_labels(sel))
    } else {
        # this means we are currently ignoring HEMI, but we shall do so no longer
        new_lvls = build_roi_levels()
        sel = new_lvls[unlist(sapply(sel, function(s) which(endsWith(new_lvls, s))))]
    }
    updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
})

observeEvent(input$roi_ignore_gyrus_sulcus, {
    sel = input$filter_by_roi
    
    if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
        # we are currently NOT ignoring G/S, but we'll start now
        sel <- unique(remove_gyrus_sulcus_labels(sel))
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
    }
        updateSelectInput(session, 'filter_by_roi', choices=build_roi_levels(), selected = sel)
})
# 
fix_group_names %?<-% function(group, namevar, prfx) {
        nms <- paste(prfx, LETTERS[seq_along(group)])
        
        for (ii in seq_along(group)) {
            if(!isTRUE(nchar(group[[ii]][[namevar]]) > 0)) {
                group[[ii]][[namevar]] = nms[ii]
            } else {
                nms[ii] <- group[[ii]][[namevar]]
            }
        }
        
        return (list('group'=group, 'names' = nms))
    }

observeEvent(input$run_analysis, {
  if(! model_params$has('data')) return (character(0))
  
  if(exists('.__DEBUG__')) {
    # multi_window = ..rm_windows
    # local_data = ..local_data
    # model_params = ..model_params
    # cond_group = list(list('group_conditions' = 'Dynamic'), list('group_conditions' = 'Static'))
    # cond_group = list(list('group_conditions' = c('Dynamic', 'Static')))
    
    # cond_group = list(
    #     list(group_name = 'A-only', group_conditions = c('drive_a','last_a', 'meant_a', 'known_a')),
    #     list(group_name = 'V-only', group_conditions = c('drive_v','last_v', 'meant_v', 'known_v')),
    #     list(group_name = 'AV', group_conditions = c('drive_av','last_av', 'meant_av', 'known_av'))
    # )
    # assign('local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
    # assign('model_params', model_params, envir=globalenv())
    assign('cond_group', input$cond_group, envir=globalenv())
    
  }
    try_ravebuiltins_debug("Top of Run Analysis", print_only=FALSE)
  
    ### Things to check
    
    # 1. are there any fixed effects supplied?
    
    if(isTRUE(length(input$model_fixed_effects) < 1)) {
        showNotification("No fixed effects specified... I'll just testing whether mean == 0",
            type = 'warning')
    }
    
    # 2. Which trials should be included in the analysis? This is important even if ConditionGroup is not a Fixed Effect
    cond_group <- dipsaus::drop_nulls(lapply(input$cond_group, function(g){
      if(length(g$group_conditions) == 0) return( NULL )
      return(g)
    }))
    
    # if no cond_group is specified, use all available trials
    if(length(cond_group) < 1) {
      cond_group = list(list('group_conditions' = unique(model_params$get('data')$Condition)))
    }
    all_trial_types <- unique(model_params$get('data')$Condition)
    
    # the unlist here instead of sapply because the lengths may be unequal
    selected_trial_types = unlist(lapply(cond_group, getElement, 'group_conditions'))
    if(length(selected_trial_types) == 0) {
        selected_trial_types = all_trial_types
    }
    if(any(duplicated(selected_trial_types))) {
        showNotification(duration=NULL, "Conditions must not map to more than one level. Duplicate(s): " %&%
                str_collapse(get_by(selected_trial_types, duplicated)) %&%
                ". Stopping analysis.", id = 'DUP_COND'
            , type='error')
        return(FALSE)
    }else {
        # we're good to go, close that notification if it's still open
        shiny::removeNotification(id='DUP_COND')
    }
    
    # 2b. If we are using a second grouping factor, then we need to make sure the same conditions are used by both factors
    if('ConditionGroup2' %in% input$model_fixed_effects) {
        cg2 <- unlist(lapply(second_factor, function(sf) unlist(sf$grouping)))
        if(!setequal(selected_trial_types, cg2)) {
            # assign('cg2', cg2, envir = globalenv())
            
            missme <- setdiff(selected_trial_types, cg2)
            showNotification(duration = NULL,
                "For factorial analyses, the conditions selected must be identical across the factors." %&%
                    "Missing from 2nd factor: " %&% str_collapse(missme) %&%
                    ". Stopping analysis.", id = 'MISS_COND',
                type='error')
            return(FALSE)
        } else {
            # we're good to go, close that notification if it's still open
            shiny::removeNotification(id='MISS_COND')
        }
        
        if(any(duplicated(cg2))) {
            showNotification(duration=NULL, "Duplicates detected across factor levels in 2nd grouping variable: " %&%
                    str_collapse(get_by(cg2, duplicated)) %&%
                    ". Stopping analysis.", id = 'DUP_COND'
                , type='error')
            return(FALSE)
        }else {
            # we're good to go, close that notification if it's still open
            shiny::removeNotification(id='DUP_COND')
        }
        
    }
    
    data.subset_trials <- subset(model_params$get('data'),
        subset = Condition %in% selected_trial_types)
    
    # make sure all the groups have a name
    res <- fix_group_names(cond_group, 'group_name', 'ConditionGroup')
    cond_group = res$group
    
    # this is important so we can use it in output without fear of it changing based on user input
    names(cond_group) <-  res$names
    model_params$set('ConditionGroups', cond_group)
    
    # assign factor name
    model_params$set('first_factor_name', input$first_factor_name)
    
    # create a joint variable representing the Group as a factor
    # now assign names to the ConditionGroup. start by assign nm1 to all trials
    data.subset_trials$ConditionGroup = res$names[1]
    # now match the rest of the trials. We only need to do this if length(cond_group) > 1 (NB: the [-1])
    # don't be silly and try 2:length(...) !
    for(ii in seq_along(cond_group)[-1]) {
      data.subset_trials$ConditionGroup[data.subset_trials$Condition %in% cond_group[[ii]]$group_conditions] = res$names[ii]
    }
    data.subset_trials$ConditionGroup %<>% factor(levels = res$names)
    
    
    # check if we need to add a second grouping variable
    if('ConditionGroup2' %in% input$model_fixed_effects) {
        res <- fix_group_names(second_factor, 'name', 'ConditionGroup2')
        second_factor = res$group
        
        # this is important so we can use it in output without fear of it changing based on user input
        names(second_factor) <-  res$names
        model_params$set('ConditionGroup2', second_factor)
        
        data.subset_trials$ConditionGroup2 = res$names[1]
        # don't be silly and try 2:length(...) !
        for(ii in seq_along(second_factor)[-1]) {
            data.subset_trials$ConditionGroup2[data.subset_trials$Condition %in%
                    second_factor[[ii]]$grouping] = res$names[ii]
        }
        data.subset_trials$ConditionGroup2 %<>% factor(levels = res$names)
        
        # add in the second factore name
        model_params$set('second_factor_name', input$second_factor_name)
    }
    
    # straighten out the dependent variable situation
    .txt <- paste('assigning ', input$model_dependent, ' to y')
    dipsaus::cat2(.txt, level='INFO')
    showNotification(p(.txt), duration = NULL, type = 'default', id = ns('noti'))
    
    try_ravebuiltins_debug(print_only=FALSE)
    
    dv <- input$model_dependent
    dv.pretty <- pretty_string(dv)
    # dv <- 'z_score_Amplitude_Trial_Onset'
    model_params$set('var_dependent', dv)
    model_params$set('var_dependent_label', dv.pretty)

    usual_dvs <- c(format_unit_of_analysis_name(get_unit_of_analysis(names=TRUE)), 'Power')
        
    ev <- trimws(pretty_string(
        stringr::str_remove(dv, usual_dvs[which(stringr::str_detect(dv, usual_dvs))])
    ))
    
    model_params$set('var_dependent_event', ev)
    model_params$set('var_dependent_label_wo_event',
        stringr::str_remove(dv.pretty, ev)
    )
    
    data.subset_trials$y = data.subset_trials[[model_params$get('var_dependent')]]
    data.subset_trials[[model_params$get('var_dependent')]] <- NULL

    # creating ROI variable
    # check if there is an active ROI
    roi_var = paste(input$model_roi_variable)
    if(isTRUE(nchar(roi_var) > 1)) {
        dipsaus::cat2(paste('assigning ', roi_var , ' to ROI'), level='INFO')
        
        # ensure the ROI groups have usable names
        res <- fix_group_names(input$roi_group, 'roi_group_name', 'ROI')
        
        # if we're doing the CustomROI, we need to build it!
        if(roi_var == 'CustomROI') {
            rbv <- RAVE_ROI_KEY %&% input$roi_builder_variable
            # same process as for condition groups, except there may be ROI sub-regions unassigned, so
            # make the default RAVE_UNKNOWN_ROI_GROUP rather than names[1]
            # first ensure all the ROIs have names
            # fix_group_names()
            
            data.subset_trials$ROI = 'RAVE_UNKNOWN_ROI_GROUP' #res$names[1]
            for(ii in seq_along(res$names)) {
                data.subset_trials$ROI[data.subset_trials[[rbv]] %in% res$group[[ii]]$roi_grouping] = res$names[ii]
            }
        } else {
            data.subset_trials$ROI = data.subset_trials[[RAVE_ROI_KEY %&% roi_var]]
            data.subset_trials[[RAVE_ROI_KEY %&% roi_var]] <- NULL
        }
        
        lvls = input$filter_by_roi
        if(length(lvls) < 1) {
            roi_var=""
            showNotification('ROI selected, but no levels specified... ignoring ROI variable',
                type='warning', id='LME_NOTES')
        } else {
            roi_var="ROI"

            # check if we need to filter the ROIs
            if(isTRUE(input$roi_ignore_gyrus_sulcus)) {
                data.subset_trials$ROI %<>% remove_gyrus_sulcus_labels
            }
            if(isTRUE(input$roi_ignore_hemisphere)) {
                data.subset_trials$ROI %<>% remove_hemisphere_labels
            }
            
            #specify levels so we preserve the order the user gave us
            data.subset_trials$ROI %<>% factor(levels=lvls)
            
            #TODO should look into sample size issues here
            data.subset_trials <- data.subset_trials[!is.na(data.subset_trials$ROI),]
        }
    }
    analysis_window = input$analysis_window
    
    # if we're doing multi-window analysis, the analysis_window is the union of all the windows
    # this is potentially (likely) discontinuous. embrace the discontinuity as it should speed things
    # up at the cost of a bit of bookkeeping 
    window_names_in_order = c()
    if(isTRUE(input$multi_window_is_active)) {
        
        # fix the window names
        res <- fix_group_names(input$multi_window, 'window_name', 'Window')
        window_names_in_order <- res$names
        multi_window <- res$group
            
        # fix the time window in the subset data and then assign labels to the resulting windows
        .times = round(unique(data.subset_trials$Time), 7)
        
        # create a dummy window name, this can be stripped out latter
        data.subset_trials$TimeWindow = "ZZZ___"
        
        # build the time Union and assign labels to included time points
        included_times = rep(FALSE, length(.times))
        analysis_window <- vector("list", length(multi_window))
        for(ii in seq_along(multi_window)) {
            aw = unlist(multi_window[[ii]]$analysis_window)
            analysis_window[[ii]] = aw
            included_times = included_times | (.times %within% aw)
            data.subset_trials$TimeWindow[data.subset_trials$Time %within% aw] = multi_window[[ii]]$window_name
        }
        # factorize the Timewindow     
        data.subset_trials$TimeWindow %<>% factor(levels = window_names_in_order)
        # update the over time data to reflect the new TimeWindow       
    
        # fastest?
        data.analysis_times_only <- data.subset_trials[!is.na(data.subset_trials$TimeWindow), ]
            
        # be careful here about %in% (correct) vs. %within% (as used above, but not the thing to use here)
        # data.analysis_times_only <- data.subset_trials[data.subset_trials$Time %in% .times[included_times], ]
        # even slower: data.subset_trials %<>% subset(Time %in% .times[included_times])
        
        model_params$set('is_multi_window', TRUE)
        model_params$set('analysis_window', analysis_window)
    } else {
        data.analysis_times_only <- data.subset_trials[data.subset_trials$Time %within% analysis_window,]
        model_params$set('is_multi_window', FALSE)
        model_params$set('analysis_window', analysis_window)
    }
    
    model_params$set('over_time_data', data.subset_trials)

    fe = input$model_fixed_effects
    re = str_collapse(by="+", unique(c('Subject', 'Electrode', input$model_random_effects)))
    
    # not sure we should be keeping the non Subj/Elec random effects in the collapse
    # collapse_formula = as.formula(sprintf('y ~ %s', re %?&% fe %?&% roi_var))
    # system.time({
    #     collapsed_data <- aggregate(collapse_formula, data=data.subset_trials, FUN=mean)
    # })
    
    .by <- c(fe, 'Subject', "Electrode", roi_var)
    # we need the unique here because ROI might be in fe and roi_var
    .by <- unique(.by[nchar(.by)>0])
    collapsed_data <- data.subset_trials[,list(y=mean(y)), by = .by]
    
    model_params$set('collapsed_data', collapsed_data)
    
    # here we need to determine whether the data should be agg'd over ROI or not.
    # are we filtering on ROI or keeping it in the analysis?
    
    # system.time({
    #     aot <- aggregate(as.formula('y ~ Time + Subject + Electrode' %?&% fe %&% do_roi),
    #         data = model_params$get('over_time_data'), FUN=mean) %>%
    #         do_aggregate(as.formula('y ~ Time' %?&% fe %&% do_roi), .fast_mse)
    #     # model_params$set('agg_over_trial', aot)
    # })
    
    
    tmp <- data.table::as.data.table(model_params$get('over_time_data'))
    # get the "by" columns
    .by <- c('Time', 'Subject', 'Electrode', fe)
    .by <- .by[nchar(.by) > 0]
    
    # collapsing over trial
    tmp <- tmp[,list(y=mean(y)), keyby=.by]
    
    # collapsing over subject and electrode
    .by <- .by[!(.by %in% c("Subject", "Electrode"))]
    tmp <- tmp[, list(m=mean(y), se = rutabaga:::se(y)), by=.by]
    model_params$set('agg_over_trial', tmp)
    
    # all.equal(tmp[order(ConditionGroup, Time), 'm'], aot$y[,1], check.attributes = F)
    # all.equal(tmp[order(ConditionGroup, Time), 'se'], aot$y[,2], check.attributes = F)
    
    try_ravebuiltins_debug(paste("Done with selection and aggregation:", 
        format(nrow(model_params$get('data')), big.mark=','), "rows to",
        format(big.mark=',', nrow(collapsed_data)))
    )
    
    # for the per-electrode plots, we're going back to the over_time data because we need the individual trials
    if(isTRUE(input$multi_window_is_active)) {
        windows = lapply(multi_window, function(mw) unlist(mw$analysis_window))
    } else {
        windows = analysis_window
    }
    # fe <- c('ConditionGroup', 'ConditionGroup2')
    # roi_var <- ''
    try_ravebuiltins_debug("Building by-electrode data")
    # fe
    by_el_data = model_params$get('over_time_data') %>%
        subset((.)$Time %within_any% windows) %>% 
        do_aggregate(as.formula('y ~ Condition + Trial + Subject + Electrode + Project' %?&% str_collapse(fe, by='+') %?&% roi_var), mean)
    
    if(!is.null(by_el_data$TimeWindow)) {
        by_el_data$TimeWindow %<>% factor(levels = window_names_in_order)
    }
    
    if(!is.null(by_el_data$ConditionGroup)) {
        by_el_data$ConditionGroup %<>% factor
    }
    if(!is.null(by_el_data$ConditionGroup2)) {
        by_el_data$ConditionGroup2 %<>% factor
    }
    
    by_el_data$UUID = by_el_data %$% {Subject %&% Electrode}
    
    try_ravebuiltins_debug("Trying to fit univariate model")
    
    showNotification(p('Fitting univariate model...'), duration = NULL, type = 'default', id = ns('noti'))
    # note that we are only including fe here, not ROI because ROI is group of electrodes... 
    by_el_data %>% split((.)$UUID) %>%
        lapply_async3(analyze_single_electrode, fixed_effects = fe) %>%
        rbind_list %>% magrittr::set_rownames(NULL) ->
      by_el_results
    
    
    try_ravebuiltins_debug("done with univariate model")
    
    ### let's clean up the doubled parens that can happen for Intercept tests
    names(by_el_results) %<>% stringr::str_replace_all(c('\\(\\('='(', '\\)\\)'=')'))
    
    by_el_results[names(by_el_results) %>% startsWith('p(')] %<>% lapply(round_pval)

    # round t, F, and X2 test statistics    
    by_el_results[stringr::str_starts(names(by_el_results), '(t\\(|F\\(|X2\\()' )] %<>% lapply(round_test_statistic)
    
    # rounding to 2 places is pretty safe I'd think for display purposes.
    # When they export the data, just be sure to export the raw values
    by_el_results[names(by_el_results) %>% startsWith('m(')] %<>% lapply(round, 2)
    
    # de-factor the electrode label
    by_el_results$Electrode <-  as.numeric(as.character(by_el_results$Electrode))
    
    # create a jittered subject value that can be used for plotting
    by_el_results$`jitter(Subject)` = jitter(as.integer(factor(by_el_results$Subject)))
    
    yi <- which(startsWith(names(by_el_results), 'F('))
    if(any(yi)) {
        .yvar = names(by_el_results)[yi[1]]
    } else {
        .yvar = names(by_el_results)[min(ncol(by_el_results), 5)]
    }
    
    try_ravebuiltins_debug("update some UI elements")
    
    # udate the UI for post hoc plots
    updateSelectInput(session, 'post_hoc_plot_xvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = 'jitter(Subject)')
    updateSelectInput(session, 'post_hoc_plot_yvar',
                      choices = c(names(by_el_results), 'CUSTOM'),
                      selected = .yvar)
    updateSelectInput(session, 'post_hoc_plot_zvar',
                      choices = c('None', names(by_el_results), 'CUSTOM'))
    
    # local_data$by_electrode_results = by_el_results
    model_params$set('by_electrode_results', by_el_results)
    
    # input <- list(model_formula = "Power ~ 1 + (1|Subject)")
    fo <- stringr::str_replace_all(input$model_formula,
        model_params$get('var_dependent'), 'y')
    
    try_ravebuiltins_debug("create formula", fo)
    
    # fo <- "y ~ ConditionGroup*ROI + (1|Subject/Electrode)"
    # fo <- str_replace_all('Pct_Change_Power_1stWord ~ TimeWindow + (1|Subject/Electrode)', 'Pct_Change_Power_1stWord', 'y')
    fo %<>% as.formula
    tryCatch({
        .rhs <- colnames(attr(terms(fo), 'factors'))
        n_fixed_effects = sum(stringr::str_detect(.rhs, '\\|', negate = T))
        
        # check factor sample sizes. this is !! when ROI \in fixed effects
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
            try_ravebuiltins_debug("Fitting mixed-effects model...")
            showNotification(p('Fitting mixed-effects model...'),
                duration = NULL, type = 'default', id = ns('noti'))
            
            lmer_results = lmerTest::lmer(fo, data=collapsed_data, na.action=na.omit)
            
            # summary(lmer(fo, data=collapsed_data))
        } else if(model_params$get("number_of_subjects", missing = 0) > 1) {
            ## no fixed effects, just checking if activity > 0
            # lmerTest::lmer(fo, data=collapsed_data)
            showNotification(p('No fixed effects... running intercept-only model...'),
                duration = NULL, type = 'default', id = ns('noti'))
            lmer_results = lmerTest::lmer(y ~ 1 + (1|Subject),
                                          data=collapsed_data, na.action=na.omit)
        } else {
            showNotification(p('No random effects... failing back to linear regression model...'),
                duration = NULL, type = 'default', id = ns('noti'))
            lmer_results = lm(y ~ 1, data=collapsed_data, na.action=na.omit)
        }
        if(is.null(lmer_results)) {
            try_ravebuiltins_debug("results are null?!!", print_only=FALSE)
        }
        
        # this is reactive, so plots will know to update
        local_data$lmer_results <- lmer_results

        # also store it here to make it easy to write out the "state" of the model
        model_params$set('lmer_results', lmer_results)
        
        model_params$set('lmer_results_summary', summary(lmer_results))
        model_params$set('how_to_model_roi', input$how_to_model_roi)
        
        
        showNotification(p('Model finished! Updating plots...'), duration = 3,
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
        
        if(is.null(lmer_results)) {
            try_ravebuiltins_debug("inside the catch!")
        }
        
        
        local_data$lmer_results <- NULL
        model_params$remove('lmer_results')
        model_params$remove('lmer_results_summary')
        
        showNotification(p('Model finished with errors, check console....'), duration = 10,
                         type = 'error', id = ns('noti'))
    })

    try_ravebuiltins_debug("Done with run_analysis", print_only=FALSE)
    
    # update the options for the custom windowed plot
    fitted_effects <- unique(unlist(str_split(attr(terms(lmer_results), 'term.labels'), ':')))
    
    updateSelectInput(session, 'custom_plot_xvar', choices = fitted_effects)
    updateSelectInput(session, 'custom_plot_gvar', choices = c('none', fitted_effects),
        selected = ifelse('ConditionGroup2' %in% fitted_effects, 'ConditionGroup2', 'none'))
    
    updateSelectInput(session, 'custom_plot_panelvar', choices = c('none', fitted_effects), selected = 
            ifelse('ROI' %in% fitted_effects, 'ROI', 'none'))
    
    sapply(c('Build ROIs', 'Build first grouping factor', 'Build second grouping factor', 'Single time window analysis', 'Build Model'), rave::close_tab,
           module_id ='group_analysis_lme')

    sapply(c('Configure group plots', 'Plost post-hoc variables', 'Configure post-hoc plot', 'Configure/Export electrode-level results'), rave::open_tab,
           module_id ='group_analysis_lme')
})

observeEvent(input$lme_3dviewer_widget_mouse_dblclicked, {
    .data <- input$lme_3dviewer_widget_mouse_dblclicked
    sbj = .data$subject
    el = .data$electrode_number
    
    if(!is.null(local_data$lmer_results)) {
        ber = model_params$get('by_electrode_results')
        selected_row = which(ber$Subject == sbj & ber$Electrode == el)
        
        dtp = DT::dataTableProxy('show_by_electrode_results', deferUntilFlush = FALSE)
        local_data$show_by_electrode_results_rows_selected = NULL
        
        # call select AFTER the set to NULL. This select call should trigger an update to SBERRS
        DT::selectRows(dtp, selected_row)
        
        showNotification(p('Selected ', sbj, el), type = 'default', id = ns('lme_3dviewer_widget__mouse'))
    } else {
        showNotification(p('Selected ', sbj, el, ", but no data have been loaded"),
            type = 'default', id = ns('lme_3dviewer_widget__mouse'))
    }
    
})
