# outputs

electrode_table_ui <- function(){
    # shiny::validate(
    #   shiny::need(exists('combined_table') && is.data.frame(combined_table),
    #               message = 'No table imported.')
    # )
    local_data$viewer_result
    
    if( !(exists('combined_table') && is.data.frame(combined_table)) ){
        return(div(
            class = "shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation",
            'No table imported'
        ))
    }
    
    div(
        style='min-height: 520px;',
        checkboxInput(ns('table_show_all'), 'Show all', value = cache_input('table_show_all', val = FALSE)),
        DT::dataTableOutput(ns('electrode_table'))
    )
}

output$electrode_table <- DT::renderDataTable({
    local_data$viewer_result
    shiny::validate(
        shiny::need(exists('combined_table') && is.data.frame(combined_table),
                    message = 'No table imported.')
    )
    
    tbl = combined_table
    click_info = input$viewer_result_out_mouse_dblclicked
    if( !isTRUE(input$table_show_all) && is.list(click_info) && isTRUE(click_info$is_electrode)){
        sub = click_info$subject
        elec = click_info$electrode_number
        
        tbl = combined_table[combined_table$Electrode %in% elec & combined_table$Subject %in% sub, ]
    }
    
    DT::datatable(tbl, options = list(
        scrollX = TRUE,
        lengthMenu = c(10, 25, 100)
    ))
})




viewer_result_fun <- function(...){
    
    # Assume brain is given
    shiny::validate(
        shiny::need(length(brain), message = 'Cannot find any Brain object')
    )
    
    client_size = get_client_size()
    side_width = ceiling((client_size$available_size[[2]] - 300) / 3)
    
    brain$plot(side_width = min(side_width, 300), debug = DEBUG)
}

viewer_result_out_ui <- function(){
    client_size = get_client_size()
    client_height = client_size$available_size[[2]] - 200
    client_height = sprintf('%.0fpx', client_height)
    htmltools::div(
        style = 'margin:-10px;',
        threeBrain::threejsBrainOutput(ns('viewer_result_out'), width = '100%', height = client_height)
    )
}

electrode_details <- function(){
    # listen to dblclick information
    
    click_info = input$viewer_result_out_mouse_dblclicked
    
    if( !isTRUE(click_info$is_electrode) ){
        return(div(
            class = "shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation",
            'Please double click an electrode'
        ))
    }
    
    local_data$click_info = click_info
    # This is a electrode, display the information
    keyframes = click_info$object$keyframes
    varnames = names(keyframes)
    varnames = varnames[!varnames %in% c(
        '[Hightlight]', '[Subject]', '[No Color]', 'X', 'Project', 'Subject'
    )]
    if( !length(varnames) ){
        return(div(
            class = "shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation",
            'No value found'
        ))
    }
    
    current_clip = shiny::isolate( local_data$detail_type )
    if( !length(current_clip) || !current_clip %in% varnames ){
        current_clip = varnames[1]
    }
    
    re = get_electrode_info(brain = brain, subject_code = click_info$subject, 
                            electrode = click_info$electrode_number, keyframe = '')
    
    re = tagList(
        selectInput(ns('detail_type'), sprintf('Variable - %s (%s)', re$name, re$group), choices = varnames, selected = current_clip),
        shiny::uiOutput(ns('detail_ui'))
    )
}


output$detail_ui <- renderUI({
    dtype = local_data$detail_type
    click_info = local_data$click_info
    
    re = get_electrode_info(brain = brain, subject_code = click_info$subject, 
                            electrode = click_info$electrode_number, keyframe = dtype)
    
    div(
        style = 'margin:0 -10px -10px -10px;',
        shiny::plotOutput(ns('detail_plot'), height = '460px')
    )
})

get_electrode_info <- function(brain, subject_code, electrode, keyframe){
    is_multiple = !is.null(brain$template_subject)
    if( is_multiple ){
        brain = brain$objects[[ subject_code ]]
    }
    if(is.null(brain)){ return() }
    el = brain$electrodes$objects[[electrode]]
    if( is.null(el) ){ return(NULL) }
    
    row = subject$electrodes[subject$electrodes$Electrode == electrode, ]
    
    kf = el$keyframes[[ keyframe ]]
    
    re = list(
        name = el$name,
        group = row$Group
    )
    
    if( !is.null(kf) ){
        re$is_continuous = kf$is_continuous
        re$time_range = kf$time_range
        re$value_names = kf$value_names
        re$value_range = kf$value_range
    }
    re
}

output$detail_plot <- renderPlot({
    dtype = local_data$detail_type
    click_info = local_data$click_info
    if( length(dtype) != 1 || !is.list(click_info) ){
        return()
    }
    assign('click_info', click_info, envir = globalenv())
    
    keyframes = click_info$object$keyframes
    plot_data = keyframes[[ dtype ]]
    
    if( is.null(plot_data) ){
        return()
    }
    
    
    time = unlist(plot_data$time)
    value = unlist(plot_data$value)
    
    if(plot_data$data_type != 'continuous'){
        info = get_electrode_info(brain = brain, subject_code = click_info$subject, electrode = click_info$electrode_number, keyframe = dtype)
        
        if( !is.factor(info$value_names) ){
            level = levels(as.factor(info$value_names))
        }else{
            level = levels(info$value_names)
        }
        
        value = factor(value, levels = level)
        rutabaga::plot_clean(time, as.numeric(value), xlab = 'Time', ylab = dtype,
                             main = click_info$name)
        points(time, value, type = 'p', pch = 16)
        rutabaga::ruta_axis(1, pretty(time))
        rutabaga::ruta_axis(2, value)
    }else{
        if( length(value) <= 20){
            tp = 'b'
        }else{
            tp = 'l'
        }
        rutabaga::plot_clean(time, value, xlab = 'Time', ylab = dtype,
                             main = click_info$name)
        points(time, value, type = tp, pch = 16)
        rutabaga::ruta_axis(1, pretty(time))
        rutabaga::ruta_axis(2, pretty(value))
    }
    
})


observeEvent(input$detail_type, {
    local_data$detail_type = input$detail_type
})
