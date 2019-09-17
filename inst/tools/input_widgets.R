define_input_3d_viewer_generator <- function(
  inputId, label = 'Open viewer in a new tab', button_types = c('primary', 'default'), 
  download_label = 'Download', download_btn = TRUE,
  download_filename = 'rave_viewer.zip', 
  reactive = 'input'
){
  input_ui = paste0(inputId, '_ui')
  input_fun = paste0(inputId, '_fun')
  input_download = paste0(inputId, '_download')
  outputId = paste0(inputId, '_out')
  quo = rlang::quo({
    define_input( definition = actionButtonStyled(
      inputId = !!inputId, label = !!label, width = '100%', type = !!button_types[[1]]
    ) )
    define_input( definition = rave::customizedUI(!!input_ui) )
    # This is actually an output
    load_scripts(rlang::quo({
      
      assign(!!input_ui, function(){
        btn_class = c(!!button_types, '')[2]
        
        if( btn_class != '' ){
          btn_class = paste0('btn-', btn_class)
        }
        if( !!download_btn ){
          shiny::downloadButton(outputId = ns(!!input_download), label = !!download_label, 
                                style='width:100%', class = btn_class)
        }else{
          shiny::downloadLink(outputId = ns(!!input_download), label = !!download_label, 
                              style='width:100%')
        }
        
      })
      
      local({
        input %?<-% getDefaultReactiveInput()
        if( !!reactive == 'input' ){
          react = input
        }else{
          react = get0(!!reactive, ifnotfound = shiny::reactiveValues())
        }
        ...widget_env = new.env(parent = emptyenv())
        
        # Generate 3D viewer render function
        ...fun = function(){
          re = NULL
          f = get0(!!input_fun, envir = ..runtime_env, ifnotfound = function(...){
            rutabaga::cat2('3D Viewer', !!outputId,  'cannot find function', !!input_fun, level = 'INFO')
          })
          tryCatch({
            re = f()
          }, error = function(e){
            rave::logger(e, level = 'ERROR')
          })
          re
        }
        
        render_func = function(){
          threeBrain::renderBrain({
            re = NULL
            # Render function
            if(length( react[[!!inputId]] )){
              re = ...fun()
            }
            
            if('R6' %in% class(re)){
              re = re$plot()
            }
            ...widget_env$widget = re
            re
          })
        }
        
        output %?<-% getDefaultReactiveOutput()
        
        output[[!!input_download]] = shiny::downloadHandler(
          filename = !!download_filename, content = function(con){
            if( !length(...widget_env$widget) ){
              re = ...fun()
            }else{
              re = ...widget_env$widget
            }
            showNotification(p('Generating 3D viewer. Please wait...'), duration = NULL,
                             type = 'default', id = '...save_brain_widget')
            tmp_dir = tempdir()
            finfo = threeBrain::save_brain(re, directory = tmp_dir, 
                                           title = 'RAVE Viewer', as_zip = TRUE)
            on.exit({ unlink( finfo$zipfile ) })
            
            showNotification(p('Done!'), type = 'message', id = '...save_brain_widget')
            
            file.copy(finfo$zipfile, to = con, overwrite = TRUE, recursive = FALSE)
          }
        )
        
        
        # Register render function
        
        # 1. main viewer (if exists)
        output[[!!outputId]] <- render_func()
        
        # 2. side viewers
        # Register cross-session function so that other sessions can register the same output widget
        session$userData$cross_session_funcs %?<-% list()
        # ns must be defined, but in get_module(..., local=T) will raise error
        # because we are not in shiny environment
        ns %?<-% function(x){x} 
        session$userData$cross_session_funcs[[ns(!!outputId)]] = render_func
        
        
        observeEvent(input[[!!inputId]], {
          
          rave_id = session$userData$rave_id
          if(is.null(rave_id)){ rave_id = '' }
          token = session$userData$token
          if(is.null(token)){ token = '' }
          globalId = ns(!!outputId)
          
          query_str = list(
            type = '3dviewer',
            globalId = htmltools::urlEncodePath(globalId),
            sessionId = htmltools::urlEncodePath(rave_id),
            token = token
          )
          url = paste(sprintf('%s=%s', names(query_str), as.vector(query_str)), collapse = '&')
          
          shinyjs::runjs(sprintf('window.open("/?%s");', url))
        })
      })
      
    }))
  })
  
  parent_frame = parent.frame()
  
  rave::eval_dirty(quo, env = parent_frame)
}


define_input_multiple_electrodes <- function(inputId, label = 'Electrodes'){
  quo = rlang::quo({
    define_input(
      definition = textInput(!!inputId, !!label, value = "", placeholder = '1-5,8,11-20'),
      init_args = c('label', 'value'),
      init_expr = {

        electrodes = preload_info$electrodes

        last_input = cache_input(!!inputId, val = as.character(electrodes[1]))
        e = rave::parse_selections(last_input)
        e = e[e %in% electrodes]
        if(!length(e)){
          e = electrodes[1]
        }
        value = rave::deparse_selections(e)
        label = 'Electrodes (' %&% rave::deparse_selections(electrodes) %&% ')'
      }
    )
  })

  parent_frame = parent.frame()
  rave::eval_dirty(quo, env = parent_frame)
}


define_input_single_electrode <- function(inputId, label = 'Electrode'){
  quo = rlang::quo({
    define_input(
      definition = selectInput(!!inputId, !!label, choices = '', selected = NULL, multiple = FALSE),
      init_args = c('choices', 'selected'),
      init_expr = {
        electrodes = preload_info$electrodes
        choices = as.character(electrodes)

        selected = cache_input(!!inputId, val = electrodes[1])
        selected = as.character(selected)

        if(length(selected) != 1 || !selected %in% choices){
          selected = choices[1]
        }
      }
    )
  })

  parent_frame = parent.frame()

  rave::eval_dirty(quo, env = parent_frame)
}



define_input_frequency <- function(inputId, label = 'Frequency', is_range = TRUE, round = -1, initial_value = NULL){

  if(is_range){
    v = c(1,200)
  }else{
    v = 1
  }

  quo = rlang::quo({
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 1, max = 200, value = !!v, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        freq_range = range(preload_info$frequencies)
        min = floor(freq_range[1])
        max = ceiling(freq_range[2])
        initial_value = !!initial_value
        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        
        value = cache_input(!!inputId, initial_value)
        if(length(value) == 1) {
          # the problem here is that it doesn't work for ranges...
          value = ..get_nearest_val(value, preload_info$frequencies)
        } else {
          v1 <- ..get_nearest_val(value[1], preload_info$frequencies)
          v2 <- ..get_nearest_val(value[2], preload_info$frequencies)
          value = c(v1,v2)
        }
      }
    )
  })

  parent_frame = parent.frame()

  rave::eval_dirty(quo, env = parent_frame)
}

define_srate_input_slider <- function(inputId, label) {
  quo = rlang::quo({
    define_input(
      definition = sliderInput(!!inputId, !!label, min = 1, max = 100, value = 100, step = 1, round = 1),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        min =1
        initial_value = module_tools$get_sample_rate(original = FALSE)
        max = min(initial_value * 2, module_tools$get_sample_rate(original = TRUE))
        value = cache_input(!!inputId, initial_value)
      }
    )
  })
  parent_frame = parent.frame()
  
  rave::eval_dirty(quo, env = parent_frame)
}


define_input_time <- function(inputId, label = 'Time Range', is_range = TRUE, round = -2, initial_value = NULL){
  if(is_range){
    v = c(0,1)
  }else{
    v = 0
  }

  quo = rlang::quo({

    define_input(
      definition = sliderInput(!!inputId, !!label, min = 0, max = 1, value = !!v, step = 0.01, round = !!round),
      init_args = c('min', 'max', 'value'),
      init_expr = {
        time_range = range(preload_info$time_points)

        min = min(time_range[1])
        max = max(time_range[2])
        initial_value = !!initial_value

        if(!!is_range){
          initial_value %?<-% c(min, max)
        }else{
          initial_value %?<-% min
        }
        value = cache_input(!!inputId, initial_value)
      }
    )
  })

  parent_frame = parent.frame()

  rave::eval_dirty(quo, env = parent_frame)
}

define_input_condition_groups <- function(inputId, label = 'Group', initial_groups = 1){
  quo = rlang::quo({

    define_input(
      definition = compoundInput(
        inputId = !!inputId, prefix= !!label, inital_ncomp = !!initial_groups, components = {
          textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
          selectInput('group_conditions', ' ', choices = '', multiple = TRUE)
        }),

      init_args = c('initialize', 'value'),

      init_expr = {
        cond = unique(preload_info$condition)

        initialize = list(
          group_conditions = list(
            choices = cond
          )
        )
        value = cache_input(!!inputId, list(
          list(
            group_name = 'All Conditions',
            group_conditions = list(cond)
          )
        ))
      }
    )
  })

  parent_frame = parent.frame()

  rave::eval_dirty(quo, env = parent_frame)

}
