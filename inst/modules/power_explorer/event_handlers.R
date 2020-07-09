input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()
static_data = dipsaus::fastmap2()


local_data = reactiveValues(
    instruction_string = tags$ul(tags$li("Single-click for trial information"),
                                 tags$li("Double-click for outlier (de)selection (triggers re-calculate loop).", tags$br(), "Clicks made during
                                         re-calculate loop may not be detected.")),
    by_trial_heat_map_click_location = NULL,
    windowed_by_trial_click_location = NULL,
    click_info = NULL,
    calculate_flag = 0,
    sheth_special_include_contrasts = FALSE, 
    highlight_significant_results = FALSE,
    sheth_special_height = 10,
    sheth_special_width = 15,
    autocalc_disclaimer = "",
    plot_options = NULL,
    condition_stats = "",
    jitter_seed = sample(1:100,1),
    current_active_set = NULL
)

# this is non-reactive because rave will trigger re-render for us
ravebuiltins_power_explorer_plot_options <- build_plot_options()

brain_proxy =  threeBrain::brain_proxy('power_3d_widget', session = session)

synch_3dviewer_dv <- function() {
    if(shiny_is_running() && !is.null(local_data[['omnibus_results']])) {
        controllers = brain_proxy$get_controllers()
        
        varname = input$which_result_to_show_on_electrodes
        if(startsWith(varname, 'Omnibus Activity')) {
            varname = 't' #format_unit_of_analysis_name(input$unit_of_analysis) #colnames(local_data[['omnibus_results']])[1]
        } else {
            varname = 't_' %&% fix_name_for_js(varname)
        }
        # dipsaus::cat2("trying to set: " , varname, level='INFO')
        
        # controllers[['Display Data']] = varname
        brain_proxy$set_controllers(list('Display Data' = varname))
    }
}

observeEvent(input$synch_export_analysis_with_3dviewer, {
    if(isTRUE(input$synch_export_analysis_with_3dviewer)) {
        synch_3dviewer_dv()
    }
})

observeEvent(input$which_result_to_show_on_electrodes, {
    if(isTRUE(input$synch_export_analysis_with_3dviewer)) {
        synch_3dviewer_dv()
    }
})
# trigger 3d viewer rebuild
rebuild_3d_viewer <- function() {
    if(rave::rave_context()$context %in% c('rave_running', 'default')) {
        dipsaus::cat2("Updating 3D viewer from r3dv")
        btn_val = (input[['power_3d_btn']]) - 0.001
        dipsaus::set_shiny_input(session = session, inputId = 'power_3d_btn',
                                 value = btn_val, method = 'proxy', priority = 'event')
    }
}


    
###
observeEvent(input$heatmap_color_palette, {
    if(input$viewer_color_palette == 'Synch with heatmaps') {
        rebuild_3d_viewer()
    }
})
observeEvent(input$viewer_color_palette, {
    rebuild_3d_viewer()
})

# observeEvent(input$btn_save_analysis_settings, {
#     tstmp <- strftime(Sys.time(), format = '%Y-%h-%d')
#     
#     shiny::showModal(shiny::modalDialog(
#         title = 'Save Analysis Settings',
#         size = 's',
#         easyClose = TRUE,
#         textInput(ns('modal_analysis_settings_name'), label = 'Settings Name', value = paste0('power_explorer_settings_', tstmp)),
#         tags$small('Will overwrite settings with the same name currently in RAVE settings folder'),
#         footer = tagList(
#             actionButtonStyled(ns('btn_do_save_analysis'), 'Save'),
#             shiny::modalButton("Cancel")
#         )
#     ))
# })

rave_is_loaded <- function() {
    c('ECoGTensor') %in% class(power)
}


observeEvent(input$event_of_interest, {
    if(!rave_is_loaded()) {
        return()
    }
    
    #based on the chosen event of interest, we need to set the available plot range 
    # print(input$event_of_interest)
    available_time = range(power$dimnames$Time)
    # eot = tail(epoch_event_types,1)
    eot = input$event_of_interest
    # eot = '1stWord'
    max_range = available_time
    if(eot != epoch_event_types[1]) {
        max_range = determine_available_shift(eot, available_time,
                                              epoch_information = get_events_data(epoch_event_types))
    }
    
    current_window = input$plot_time_range
    
    if(length(current_window) != 2) {
        current_window = available_time
    }
    current_window %<>% clip_x(max_range)
    local_data$shifted_data_range = max_range
    # print('---NEW---')
    # print(current_window) 

    updateSliderInput(session = session, inputId = 'plot_time_range',
                      value = current_window, min=max_range[1], max = max_range[2])

    updateSliderInput(session = session, inputId = 'ANALYSIS_WINDOW',
                      value = clip_x(input$ANALYSIS_WINDOW, max_range), min=max_range[1], max = max_range[2])
})

# observeEvent(input$btn_do_save_analysis, {
#     # save
#     fname = input$modal_analysis_settings_name
#     fname = stringr::str_replace_all(fname, '[^a-zA-Z0-9]+', '_')
#     fname = paste0(fname, '.yaml')
#     save_dir = file.path(subject$dirs$subject_dir, '..', '_project_data', 'analysis_yamls')
#     dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
#     save_inputs(file.path(save_dir, fname))
#     shiny::removeModal()
# })
observeEvent(input$analysis_settings_load, {
    fdata = input$analysis_settings_load
    if(!is.list(fdata) || !length(fdata$files)){ return() }
    
    # assign('fdata', fdata, envir = globalenv())
    
    f_name = unlist(fdata$files); names(f_name) = NULL
    # attachDefaultDataRepository()
    roots = c('RAVE Home' = normalizePath(subject$dirs$data_dir), 'root' = '/')
    
    f_path = do.call(file.path, as.list(c(roots[[fdata$root]], f_name)))
    # f_path = '/Volumes/data/rave_data/ent_data/demo/_project_data/power_explorer/settings/power_explorer_settings_2019_Oct_18.yaml'
    conf = yaml::read_yaml(f_path)
    
    updateCheckboxInput(session, inputId = 'auto_calculate', value = FALSE)
    
    # bindings = dipsaus::getInputBinding('shiny::selectInput')
    # update_function = eval(str2lang(bindings$update_function))
    # formals(update_function)
    # as.call(list(
    #     str2lang(bindings$update_function),
    #     session = quote(session),
    #     inputId = 'asd',
    #     value = 
    # ))
    
    # Update group condition
    dipsaus::updateCompoundInput2(session, 'GROUPS', value = conf$GROUPS, ncomp = length(conf$GROUPS))
    
    # Update analysis parameters
    updateSliderInput(session, 'ANALYSIS_WINDOW', value = conf$ANALYSIS_WINDOW)
    updateSliderInput(session, 'BASELINE_WINDOW', value = conf$BASELINE_WINDOW)
    updateSliderInput(session, 'FREQUENCY', value = conf$FREQUENCY)
    updateSelectizeInput(session, 'unit_of_analysis', selected = conf$unit_of_analysis)
})


observeEvent(input$power_3d_widget_mouse_dblclicked, {
    # mouse_event = input$power_3d__mouse_dblclicked$event
    # object = input$power_3d__mouse_dblclicked$object
    
    .data <- input$power_3d_widget_mouse_dblclicked

    # print(input$power_3d_widget_mouse_dblclicked)
    print('-------')
    print(.data$is_electrode)
    if(isTRUE(.data$is_electrode)) {
        e <- .data$electrode_number
        if(e %in% preload_info$electrodes){
            updateTextInput(session, 'ELECTRODE_TEXT', value = e)
            showNotification(p('Switched to electrode ', e), type = 'message', id = ns('power_3d_widget__mouse'))
            if(shiny_is_running()) {
                trigger_recalculate()
            }
        }else{
            showNotification(p('Electrode ', e, ' is not loaded.'), type = 'warning', id = ns('power_3d_widget__mouse'))
        }
    }

    # # This is dirty, i think we can provide function to get which electrode chosen
    # if(mouse_event$action == 'dblclick' && isTRUE( object$is_electrode )){
    #     # Get object chosen, is it an electrode?
    #     # Use isTRUE() to validate since object$is_electrode could be NULL
    #     e = stringr::str_match(object$name, '^Electrode ([0-9]+)')[2]
    #     e = as.character(e)
    #     if(e %in% preload_info$electrodes){
    #         updateTextInput(session, 'ELECTRODE_TEXT', value = e)
    #         showNotification(p('Switched to electrode ', e), type = 'message', id = ns('power_3d_widget__mouse'))
    #     }else{
    #         showNotification(p('Electrode ', e, ' is not loaded.'), type = 'warning', id = ns('power_3d_widget__mouse'))
    #     }
    # }
})

observeEvent(input$trial_outliers_list, {
    enable_save_button()
})


observeEvent(input$synch_with_trial_selector, {
    GROUPS %?<-% NULL
    
    if(!is.null(GROUPS)) {
        # the lapply and then unlist handles the case
        # of empty group(s)
        updateSelectInput(session, 'trial_type_filter',
                          selected = unique(lapply(GROUPS, `[[`, 'group_conditions') %>% unlist)
        )
    }
})



### this ensures we don't get infinite callbacks from/to the category/numeric electrode selector
sync_shiny_inputs(
    input = input, session = session, inputIds = c(
        'ELECTRODE_TEXT', 'electrode_category_selector_choices'
    ), uniform = list(
        # ELECTRODE_TEXT to electrodes
        function(ELECTRODE_TEXT){
            return(parse_svec(ELECTRODE_TEXT, sort = TRUE))
        },
        # electrode_category_selector_choices to electrodes
        function(electrode_category_selector_choices){
            dipsaus::cat2('updating from ECSC: ', paste0(electrode_category_selector_choices, collapse='|'))
            
            electrodes_csv %?<-% NULL
            if(!is.data.frame(electrodes_csv)) { return(NULL) }
            
            # current_els <- parse_svec(input$ELECTRODE_TEXT, sort = TRUE)
            
            all_vals <- electrodes_csv[[input$electrode_category_selector]]
            
            # in case the choices have the electrode labels in them
            vals <- sapply(electrode_category_selector_choices, remove_count_from_label)
            dipsaus::cat2('Cleaned: ', paste0(vals, collapse='|'))
            
            ### if we've merged hemisphere labels, then the prefix will be wrong, fix it
            if(isTRUE(input$merge_hemisphere_labels)) {
                all_vals %<>% remove_hemisphere_labels
            }
            
            new_els <- sort(
                as.numeric(electrodes_csv$Electrode[all_vals %in% vals])
            )
            return(new_els)
        }
    ), updates = list(
        # update ELECTRODE_TEXT
        function(els){
            if(!is.null(els)) {updateTextInput(session, 'ELECTRODE_TEXT', value = deparse_svec(els))}
        },
        # update electrode_category_selector_choices
        function(els){
            update_electrode_category_select(els)
        }
    )
)

# observeEvent(input$ELECTRODE_TEXT, {
#     # be careful here so we don't trigger loops!
#     electrodes_csv %?<-% NULL
#     if(is.data.frame(electrodes_csv)) {
#         
#         # check if the electrode text matches the current electrode values
#         current_etext_els <- as.numeric(parse_svec(input$ELECTRODE_TEXT)  ) %>% sort
#         all_vals <- electrodes_csv[[input$electrode_category_selector]]
#         vals <- input$electrode_category_selector_choices
#         current_category_els <- as.numeric(electrodes_csv$Electrode[vals == all_vals]) %>% sort
#         
#         if(!all(current_etext_els == current_category_els)) {
#             .selected <- unique(all_vals[as.numeric(electrodes_csv$Electrode) %in%
#                                          current_etext_els])
#             
#             updateSelectInput(session, 'electrode_category_selector_choices',
#                               selected = .selected)
#         } else {
#             # no change
#         }
#         
#         
#     }
# })


observeEvent(input$merge_hemisphere_labels, {
    update_electrode_category_select()
})

update_electrode_category_select <- function(els) {
    electrodes_csv %?<-% NULL
    if(is.null(electrodes_csv)) return()
    
    col_name <- input$electrode_category_selector
    if(is.null(col_name) || nchar(col_name) < 1) {return()}
    dipsaus::cat2('Col name: ', col_name, level='INFO')
    
    if(missing(els)) {
        selected_electrode_numbers = as.numeric(parse_svec(input$ELECTRODE_TEXT))
    } else {
        dipsaus::cat2('updating from els: ', els, level='INFO', pal=list('INFO' = 'orangered'))
        selected_electrode_numbers = els
    }
    ecsv = subset(electrodes_csv,
                  select=c('Electrode', col_name))
    
    ecsv$values = ecsv[[col_name]]
    
    if(isTRUE(input$merge_hemisphere_labels)) {
        ecsv$values %<>% remove_hemisphere_labels
    }
    labels = make_label_with_count(ecsv$values)
    
    .selected = unique(
        attr(labels, 'map')[which(selected_electrode_numbers %in% ecsv$Electrode)]
    )
    
    assign('uecs', list(
        .selected, labels, col_name, electrodes_csv, selected_electrode_numbers, input$merge_hemisphere_labels
    ), envir = globalenv())
    
    if(length(.selected) < 1) 
        .selected = 1
    
    updateSelectInput(session, 'electrode_category_selector_choices',
                      selected = labels[.selected],
                      choices = labels)
}



observeEvent(input$electrode_category_selector, {
    update_electrode_category_select()
})


observeEvent(input$analysis_filter_variable, {
    electrodes_csv %?<-% NULL
    
    if(is.data.frame(electrodes_csv)) {
        col_name <- input$analysis_filter_variable
        if(col_name == 'none') {
            updateSelectInput(session, 'analysis_filter_elec', 
                              selected=character(0),
                              choices = character(0))
        } else {
            updateSelectInput(session, 'analysis_filter_elec', 
                              selected=unique(electrodes_csv[[col_name]]),
                              choices = unique(electrodes_csv[[col_name]]))
        }
    }
})

observeEvent(input$analysis_filter_variable_2, {
    electrodes_csv %?<-% NULL
    
    if(is.data.frame(electrodes_csv)) {
        col_name <- input$analysis_filter_variable_2
        
        if(col_name == 'none') {
            updateSelectInput(session, 'analysis_filter_elec_2', 
                              selected=character(0),
                              choices = character(0))
        } else {
            updateSelectInput(session, 'analysis_filter_elec_2', 
                              selected=unique(electrodes_csv[[col_name]]),
                              choices = unique(electrodes_csv[[col_name]]))
        }
        
    }
})

observeEvent(input$reset_electrode_selectors, {
    if(!is.null(electrodes_csv)) {
        if('freesurferlabel' %in% names(electrodes_csv)) {
            updateSelectInput(session, electrode_category_selector, selected = 'freesurferlabel')
        }else {
            updateSelectInput(session, electrode_category_selector, selected = 'Label')
        }
        updateTextInput(session, 'ELECTRODE_TEXT', value=electrodes_csv$Electrode[1])
    }
    
}, ignoreInit=TRUE)

observeEvent(input$select_good_electrodes, {
    if (!is.null(input$current_active_set)) {
        updateTextInput(session, 'ELECTRODE_TEXT',
                        value = input$current_active_set)
        
        local_data$current_active_set = dipsaus::parse_svec(input$current_active_set)
        
        if (!auto_recalculate()) {
            trigger_recalculate()
        }
    }
})
        
observeEvent(input$clear_outliers, {
    updateSelectInput(session, 'trial_outliers_list', selected=character(0))
    enable_save_button()
})

# save the outlier information to the current epoch file
observeEvent(input$save_new_epoch_file, {
    showNotification(p('Saving outlier data to epoch file: ', preload_info$epoch_name), type = 'message', id = ns('snef'))

    efile <- sprintf('%s/power_outliers_%s.csv', subject$dirs$meta_dir, preload_info$epoch_name)

    epoch_data <- module_tools$get_meta('trials')
    epoch_data$PowerOutlier <- FALSE
    epoch_data$PowerOutlier[epoch_data$Trial %in% trial_outliers_list] <- TRUE

    write.csv(epoch_data, file=efile, row.names = FALSE)

    disable_save_button()
})

enable_save_button <- function() {
    # Check if current options match the original trial outlier list
    updateActionButton(session, 'save_new_epoch_file', label=HTML("<b style='color:black'>Save Outliers</b>"))
}

disable_save_button <- function() {
    updateActionButton(session, 'save_new_epoch_file', label=HTML("<span style='color:#ddd'>Save Outliers</span>"))
}


observeEvent(input$do_quick_calculate_btn, {
    trigger_recalculate()
})

update_click_information <- function() {
    .loc <- local_data$windowed_by_trial_click_location
    
    # first we determine which group is being clicked, then we drill down
    # to determine the nearest point -- this should be faster than just looking
    # through all the points across all the groups, n'est-ce pas?
    .gi <- which.min(abs(.loc$x - sapply(scatter_bar_data, `[[`, 'xp')))
    
    # scaling the x-component distance as that should be the more discriminable of the two components?
    wX <- 10
    #TODO consider a minimum closeness value here?
    .ind <- which.min(abs(wX*(scatter_bar_data[[.gi]]$x - .loc$x)) +
                          abs(scatter_bar_data[[.gi]]$data - .loc$y))
    
    .trial <- scatter_bar_data[[.gi]]$Trial_num[.ind]
    .val <- round(scatter_bar_data[[.gi]]$data[.ind],
                  digits = abs(min(0, -1+floor(log10(max(abs(scatter_bar_data[[.gi]]$data)))))))
    
    .type <- scatter_bar_data[[.gi]]$trials[.ind]
    .click_info <- list('trial' = .trial, 'value' = .val, 'trial_type' = .type)
    
    # if(is.null(local_data$click_info)) {
        local_data$click_info <- list('trial' = .trial, 'value' = .val, 'trial_type' = .type)
    # } else {
    #     # are we clicking a new location same place?
    #     if(.trial != local_data$click_info$trial) {
    #         local_data$click_info <- .click_info
    #     }
    #     local_data$click_info <- NULL
    # }
}

update_trial_outlier_list <- function() {
    last_click <- local_data$click_info
    
    if(!is.null(last_click)) {
        .tol <- input$trial_outliers_list
        if(any(last_click$trial == .tol)) {
            .tol <-  .tol[.tol != last_click$trial]
        } else {
            .tol <- c(.tol, last_click$trial)
        }
        
        updateSelectInput(session, 'trial_outliers_list', selected = .tol)
    }
}

observeEvent(input$windowed_by_trial_click, {
    # dipsaus::cat2('Single click!', level='INFO')
    local_data$windowed_by_trial_click_location = input$windowed_by_trial_click
    update_click_information()
})

observeEvent(input$windowed_by_trial_dbl_click, {
    showNotification(p('Editing outlier list, please wait...'), type = 'warning', duration=3)
    
    local_data$windowed_by_trial_click_location = input$windowed_by_trial_dbl_click
    
    update_click_information()
    
    update_trial_outlier_list()
    
    trigger_recalculate()
})

observeEvent(input$synch_3d_viewer_bg, {
    if(isTRUE(input$synch_3d_viewer_bg)) {
        rebuild_3d_viewer()
    }
})

observeEvent(input$background_plot_color_hint, {
    if(isTRUE(input$synch_3d_viewer_bg)) {
        rebuild_3d_viewer()
    }
})

observeEvent(input$GROUPS, {
    # print('group_changed')
    choices = c('Omnibus Activity (across all active trial types)')
    grp = input$GROUPS
    
    gnames = build_group_names(grp)
    if(length(gnames) > 1) {
        print('building labels')
        lbls = build_group_contrast_labels(gnames)
        choices %<>% c(as.character(gnames), lbls)
    }
    selected = input$which_result_to_show_on_electrodes
    if(!(selected %in% choices)) {
        selected = choices[1]
    }
    
    updateSelectInput(session, 'which_result_to_show_on_electrodes',
                      choices=choices, selected=selected)
})

output$trial_click <- renderUI({
    .click <- local_data$click_info
    HTML(
        "<div style='margin-left: 5px; min-height:375px'>Trial #: " %&% .click$trial %&% '<br/>Value: ' %&% .click$value %&%
            '<br/>Label: ' %&% .click$trial_type %&%
            "<p style='margin-top:20px'>&mdash;<br/>" %&% local_data$condition_stats %&% '</p>' %&%
            "<p style='margin-top:20px'>&mdash;<br/>" %&% local_data$instruction_string %&% '</p>' %&%
            '</div>'
    )
})

compare_condition_results <- function(...) {
    shiny::validate(shiny::need(!is.null(local_data$summary_statistics), 'No results calculated'))
    
    htmltools::div(
        htmltable_mat(local_data$summary_statistics)
    )
}


click_output = function() {
    if(!is.null(local_data$windowed_by_trial_click_location)) {
        return(htmlOutput(ns('trial_click')))
    }
    
    .disc = ''
    if(!auto_recalculate()){
        .disc = local_data$autocalc_disclaimer
    }
    
    return(HTML("<div style='margin-left: 5px; min-height:360px'>" %&%
    "<p style='margin-top:5px'>" %&% local_data$instruction_string %&% '</p>' %&% .disc %&% '</div>'))
}

# here we're listening to a bunch of plot options and slinging them into ravebuiltins_power_explorer_plot_options so the current list 
# can stay up to date. this is needed for hi-res figure export
observe({
    if(exists('ravebuiltins_power_explorer_plot_options')){
        ..input <- shiny::reactiveValuesToList(input)
        nm = names(..input)
        nm <- nm[nm %in% ravebuiltins_power_explorer_plot_options$keys()]
        if(length(nm) > 0) {
            ravebuiltins_power_explorer_plot_options$mset(.list=..input[nm])
        }
    }
})

