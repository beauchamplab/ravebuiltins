# outputs


# --------------------------------- Viewer ---------------------------------

# set up proxy
# proxy = threeBrain::brain_proxy('brain_viewer', session = session)
# 
# brain_viewer <- function( ... ){
#     print(list(...))
#     viewer_result_fun( proxy )
# }
# viewer_result_fun <- function( proxy ){
#     # print(proxy$background)
#     # output_needs_update()
#     # Assume brain is given
#     shiny::validate(
#         shiny::need(length(brain), message = 'Cannot find any Brain object')
#     )
#     
#     client_size = get_client_size()
#     side_width = ceiling((client_size$available_size[[2]] - 300) / 3)
#     
#     brain$plot(side_width = min(side_width, 300))
# }
# 
# # prepare to open it in new window
# session$userData$cross_session_funcs[[ns('brain_viewer')]] = viewer_result_fun
proxy = threeBrain::brain_proxy('brain_viewer_widget', session)

brain_viewer_fun <- function( render_value, side_width, env, proxy ){
    shiny::validate(
        shiny::need(render_value, message = paste('Please press', sQuote('Generate viewer'), 'first.'))
    )
    if(exists('brain')){
        
    }else{
        shiny::validate(
            shiny::need(exists('brain') && length(brain), message = paste('Surface/Volume files not found. '))
        )
    }
    if(exists('brain') && inherits(brain, c('rave-brain', 'multi-rave-brain'))){
        generate_brain( brain, proxy )
    }else{
        NULL
    }
}

generate_brain <- function( brain, proxy ){
    set_palette('Beautiful Field')
    
    session = shiny::getDefaultReactiveDomain()
    k = sprintf('output_%s_height', ns('brain_viewer_widget'))
    side_width = (session$clientData[[k]] - 100) / 3
    
    rave_theme = rave::get_rave_theme()
    
    bgcolor = proxy$isolate('background')
    if(!length(bgcolor) || bgcolor %in% c('#000000', '#FFFFFF')){
        bgcolor = ifelse('dark' %in% rave_theme$themes, '#000000', '#FFFFFF')
    }
    camera = proxy$isolate('main_camera')
    camera$zoom %?<-% 1
    
    controllers = proxy$get_controllers()
    controllers[['Background Color']] = bgcolor
    controllers[['Subject']] = NULL
    
    pname <- input$heatmap_color_palette
    
    # check for character data, coerce to factor
    
    vt <- brain$electrodes$value_table
    
    # build pals only for numeric data
    nms <- names(vt)
    
    # don't create palette for string vars
    nms = nms[!sapply(vt, function(x) any(is.factor(x), is.character(x)))]
    
    pals <- build_3dv_palette(nms, pal_names = pname)
    
    brain$plot(side_width = side_width, background = bgcolor, 
               controllers = controllers, start_zoom = camera$zoom, 
               side_display = FALSE, palettes = pals)
}

download_ui <- function(){
    shiny::downloadButton(ns('download_btn'), style = 'display:block;width:100%')
}

output$download_btn <- shiny::downloadHandler(
    filename = function(){
        strftime(Sys.time(), 'RAVE-viewer-%Y%m%d-%H%M%S.zip')
    },
    content = function(con){
        f = tempfile(fileext = 'ravethreebrain')
        showNotification(p('Compressing... Please wait.'), type = 'message', id = 'download', duration = NULL)
        on.exit({
            removeNotification('download')
            unlink(f, recursive = TRUE)
        })
        # get current proxy
        
        wg = generate_brain(brain, proxy)
        
        threeBrain::save_brain(wg, directory = f, as_zip = TRUE)
        file.copy(file.path(f, 'compressed.zip'), to = con)
    }
)

# observeEvent({
#     session$clientData[[sprintf('output_%s_height', ns('brain_viewer_widget'))]]
# }, {
#     height = session$clientData[[sprintf('output_%s_height', ns('brain_viewer_widget'))]]
#     side_width = height - 100 / 3
#     proxy$set_controllers()
# })


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
    click_info = proxy$mouse_event_click
    if( !isTRUE(input$table_show_all) && is.list(click_info) && isTRUE(click_info$is_electrode)){
        sub = click_info$subject
        elec = click_info$electrode_number
        
        tbl = combined_table[combined_table$Electrode %in% elec & combined_table$Subject %in% sub, ]
    }
    
    dt = DT::datatable(tbl, options = list(
        scrollX = TRUE,
        lengthMenu = c(10, 25, 100)
    ), selection = 'none')
    DT::formatRound(dt, names(tbl), digits = 4)
})








electrode_details <- function(){
    # listen to dblclick information
    click_info = proxy$mouse_event_click
    
    if( !isTRUE(click_info$is_electrode) ){
        return(div(
            class = "shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation",
            'Please generate viewer, load data and click on an electrode'
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
    
    current_clip = shiny::isolate(proxy$display_variable)
    # current_clip = shiny::isolate( local_data$detail_type )
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
    set_rave_theme()
    dtype = local_data$detail_type
    click_info = local_data$click_info
    if( length(dtype) != 1 || !is.list(click_info) ){
        return()
    }
    # assign('click_info', click_info, envir = globalenv())
    
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
