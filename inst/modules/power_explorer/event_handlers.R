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
