observeEvent(input$power_3d__mouse_event, {
    mouse_event = input$power_3d__mouse_event$event
    object = input$power_3d__mouse_event$object
    
    print(input$power_3d__mouse_event)
    
    # This is dirty, i think we can provide function to get which electrode chosen
    if(mouse_event$action == 'dblclick' && isTRUE( object$is_electrode )){
        # Get object chosen, is it an electrode?
        # Use isTRUE() to validate since object$is_electrode could be NULL
        e = stringr::str_match(object$name, '^Electrode ([0-9]+)')[2]
        e = as.integer(e)
        if(e %in% preload_info$electrodes){
            updateTextInput(session, 'ELECTRODE', value = e)
            showNotification(p('Switched to electrode ', e), type = 'message', id = ns('power_3d_widget__mouse'))
        }else{
            showNotification(p('Electrode ', e, ' is not loaded.'), type = 'warning', id = ns('power_3d_widget__mouse'))
        }
    }
})


input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

local_data = reactiveValues(
    by_trial_heat_map_click_location = NULL,
    windowed_by_trial_click_location = NULL,
    click_info = NULL
)

# observeEvent(input$by_trial_heat_map_click, {
#     local_data$by_trial_heat_map_click_location = input$by_trial_heat_map_click
# })


observeEvent(input$windowed_by_trial_click, {
    local_data$windowed_by_trial_click_location = input$windowed_by_trial_click
    .loc <- local_data$windowed_by_trial_click_location
    
    # first we determine which group is being clicked, then we drill down 
    # to determine the nearest point -- this should be faster than just looking
    # through all the points across all the groups, n'est-ce pas?
    .gi <- which.min(abs(.loc$x - sapply(scatter_bar_data, `[[`, 'xp')))
    .ind <- which.min(abs(scatter_bar_data[[.gi]]$x - .loc$x) + 
                         abs(scatter_bar_data[[.gi]]$data - .loc$y))
    
    .trial <- scatter_bar_data[[.gi]]$Trial_num[.ind]
    .val <- round(scatter_bar_data[[.gi]]$data[.ind],
                  digits = abs(min(0, -1+floor(log10(max(abs(scatter_bar_data[[.gi]]$data)))))))
    
    .tol <- input$trial_outliers_list
    # print(.tol)
    if(any(.trial == .tol)) {
        .tol <-  .tol[.tol != .trial]
    } else {
        .tol <- c(.tol, .trial)
    }
    
    updateSelectInput(session, 'trial_outliers_list', selected = .tol)
    local_data$click_info <- list('trial' = .trial, 'value' = .val)
})

output$trial_outlier_click <- renderUI({
    # loc <- local_data$by_trial_heat_map_click_location
    .click <- local_data$click_info

    HTML(
        'Nearest Trial: ' %&% .click$trial %&% '<br/> Value: ' %&% .click$value
    )
})

click_output = function() {
    # logger('click out...')
    # put analysis information in here
    if(!is.null(local_data$windowed_by_trial_click_location)) {
        return(htmlOutput(ns('trial_outlier_click')))
    }
    return('no trials clicked yet')
}

