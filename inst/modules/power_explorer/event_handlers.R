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
    click_location = NULL
)

# by_trial_heat_map_click <- function() {
#     print('click')
# }

observeEvent(input$by_trial_heat_map_click, {
    local_data$click_location = input$by_trial_heat_map_click
})

output$trial_heatmap_click <- renderUI({
    loc <- local_data$click_location

    print(loc)
        
    HTML(
        'Clicked: ' %&% loc$x %&% ', ' %&% loc$y
    )
})

click_output = function() {
    logger('click out...')
    # put analysis information in here
    if(!is.null(local_data$click_location)) {
        return(htmlOutput(ns('trial_heatmap_click')))
    }
    return('no trials clicked yet')
}

