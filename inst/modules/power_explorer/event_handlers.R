input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

local_data = reactiveValues(
    instruction_string = "Click on Activity by trial and condition plot for details." %&%
        "<ul><li>Single-click for trial information</li><li>Double-click for outlier (de)selection</li></ul>",
    by_trial_heat_map_click_location = NULL,
    windowed_by_trial_click_location = NULL,
    click_info = NULL,
    calculate_flag = 0
)


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
#             rave::actionButtonStyled(ns('btn_do_save_analysis'), 'Save'),
#             shiny::modalButton("Cancel")
#         )
#     ))
# })


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
    
    roots = c('RAVE Home' = normalizePath(subject$dirs$data_dir), 'root' = '/')
    
    f_path = do.call(file.path, as.list(c(roots[[fdata$root]], f_name)))
    conf = yaml::read_yaml(f_path)
    
    updateCheckboxInput(session, inputId = 'auto_calculate', value = FALSE)
    lapply(1:10, function(ii){
        gc_id = sprintf('GROUPS_group_conditions_%d', ii)
        gc = conf[[gc_id]]
        if(!length(gc)){ gc = character(0) }
        updateSelectInput(session, gc_id, selected = gc)
        
        gc_id = sprintf('GROUPS_group_names_%d', ii)
        gc = conf[[gc_id]]
        if(length(gc) != 1){ gc = '' }
        updateTextInput(session, gc_id, value = gc)
    })
})


observeEvent(input$power_3d_widget_mouse_dblclicked, {
    # mouse_event = input$power_3d__mouse_dblclicked$event
    # object = input$power_3d__mouse_dblclicked$object
    
    .data <- input$power_3d_widget_mouse_dblclicked

    # print(input$power_3d_widget_mouse_dblclicked)
    
    if(isTRUE(.data$is_electrode)) {
        e <- .data$electrode_number
        if(e %in% preload_info$electrodes){
            updateTextInput(session, 'ELECTRODE_TEXT', value = e)
            showNotification(p('Switched to electrode ', e), type = 'message', id = ns('power_3d_widget__mouse'))
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


rave::sync_shiny_inputs(
    input = input, session = session, inputIds = c(
        'ELECTRODE_TEXT', 'electrode_category_selector_choices'
    ), uniform = list(
        # ELECTRODE_TEXT to electrodes
        function(ELECTRODE_TEXT){
            return(parse_svec(ELECTRODE_TEXT, sort = TRUE))
        },
        # electrode_category_selector_choices to electrodes
        function(electrode_category_selector_choices){
            electrodes_csv %?<-% NULL
            if(!is.data.frame(electrodes_csv)) { return(NULL) }
            current_els <- parse_svec(input$ELECTRODE_TEXT, sort = TRUE)
            all_vals <- electrodes_csv[[input$electrode_category_selector]]
            vals <- electrode_category_selector_choices
            new_els <- as.numeric(electrodes_csv$Electrode[all_vals %in% vals]) %>% sort
            return(new_els)
        }
    ), updates = list(
        # update ELECTRODE_TEXT
        function(els){
            if(!is.null(els)) {updateTextInput(session, 'ELECTRODE_TEXT', value = deparse_svec(els))}
        },
        # update electrode_category_selector_choices
        function(els){
            electrodes_csv %?<-% NULL
            if(!is.data.frame(electrodes_csv)) { return(NULL) }

            all_vals <- electrodes_csv[[input$electrode_category_selector]]
            selected <- unique(all_vals[as.numeric(electrodes_csv$Electrode) %in% els])
            
            updateSelectInput(session, 'electrode_category_selector_choices',
                              selected = selected)
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


observeEvent(input$electrode_category_selector, {
    # be careful here so we don't trigger loops!
    
    electrodes_csv %?<-% NULL
    
    if(is.data.frame(electrodes_csv)) {
        col_name <- input$electrode_category_selector
        vals <- electrodes_csv[[col_name]]
        
        # to get the selected choices, we need to match with what ELECTRODE_TEXT currently provides
        .selected <- unique(vals[as.numeric(electrodes_csv$Electrode) %in%
                                     as.numeric(parse_svec(input$ELECTRODE_TEXT))])
        
        updateSelectInput(session, 'electrode_category_selector_choices',
                          selected = .selected,
                          choices = unique(vals))
    }
})

# observeEvent(input$electrode_category_selector_choices, {
#     # be careful here so we don't trigger loops!
#     
#     
#     electrodes_csv %?<-% NULL
#     if(is.data.frame(electrodes_csv)) {
#         current_els <- as.numeric(parse_svec(input$ELECTRODE_TEXT)  ) %>% sort
#         all_vals <- electrodes_csv[[input$electrode_category_selector]]
#         vals <- input$electrode_category_selector_choices
#         new_els <- as.numeric(electrodes_csv$Electrode[all_vals %in% vals]) %>% sort
#         
#         if(!all(new_els == current_els)) {
#             updateTextInput(session, 'ELECTRODE_TEXT',
#                             value = deparse_svec(new_els))
#         } else {
#             # no change
#         }
#     }
# })



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

observeEvent(input$select_good_electrodes, {
    if(!is.null(input$current_active_set)) {
        updateTextInput(session, 'ELECTRODE_TEXT', value = parse_svec(input$current_active_set))
        
        if(! input$auto_calculate) {
            print('a calc is off, auto click')
            shinyjs::click('do_calculate_btn')
        } else {
            print('a calc is on, no click')
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
    
    
    local_data$click_info <- list('trial' = .trial, 'value' = .val, 'trial_type' = .type)
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
    local_data$windowed_by_trial_click_location = input$windowed_by_trial_click
    update_click_information()
})

observeEvent(input$windowed_by_trial_dbl_click, {
    local_data$windowed_by_trial_click_location = input$windowed_by_trial_dbl_click
    update_click_information()
    update_trial_outlier_list()
})

output$trial_click <- renderUI({
    .click <- local_data$click_info
    # div(
    #     style='margin-left: 5px; min-height: 400px',
    #     'Nearest Trial: ', .click$trial, br(),
    #     'Value: ', .click$value, br(),
    #     'Trial Type: ', .click$trial_type,
    #     p(
    #         style='margin-top:20px',
    #         HTML('&mdash;'), br(),
    #         local_data$instruction_string
    #     )
    # )
    HTML("<div style='margin-left: 5px; min-height:375px'>Nearest Trial: " %&% .click$trial %&% '<br/> Value: ' %&% .click$value %&%
             '<br/> Trial Type: ' %&% .click$trial_type %&%
             "<p style='margin-top:20px'>&mdash;<br/>" %&% local_data$instruction_string %&% '</p></div>'
    )
})

click_output = function() {
    if(!is.null(local_data$windowed_by_trial_click_location)) {
        return(htmlOutput(ns('trial_click')))
    }
    
    return(HTML("<div style='margin-left: 5px; min-height:360px'>" %&%
    "<p style='margin-top:20px'>" %&% local_data$instruction_string %&% '</p></div>'))
}

