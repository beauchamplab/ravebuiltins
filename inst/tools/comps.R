get_content <- function(content, keyword = c('Start', 'End'), env = parent.frame(), evaluate = TRUE){
  start_line = which(stringr::str_detect(content, paste0('^# >>>>>>>>>>>> ', keyword[1])))
  end_line = which(stringr::str_detect(content, paste0('^# <<<<<<<<<<<< ', keyword[2])))
  has_inputs = FALSE
  if(length(start_line)){
    start_line = start_line[1]
    end_line = end_line[end_line >= start_line]
    if(length(end_line)){
      end_line = end_line[1]
      has_inputs = TRUE
    }
  }
  if(!has_inputs){
    return(FALSE)
  }
  text = paste(content[start_line:end_line], collapse = '\n')
  expr = parse(text = text)
  if(evaluate){
    eval(expr, envir = env)
    return(TRUE)
  }else{
    return(expr)
  }
}

get_comp_env <- function(module_id){
  path = get_path('inst', 'modules', module_id, 'comp.R')
  content = readLines(path)
  input_env = new.env(parent = emptyenv())
  define_input <- function(definition, init_args, init_expr){
    definition = substitute(definition)
    definition = match.call(definition = eval(definition[[1]]), definition)
    inputId = definition[['inputId']]
    re = list(
      inputId = inputId,
      definition = definition
    )
    class(re) = c('comp_input', 'list')
    if(missing(init_args)){
      input_env[[inputId]] = re
      return(invisible(re))
    }
    init_expr = substitute(init_expr)
    initialization = rlang::quo(local({
      !!!init_expr
      sapply(!!init_args, get, envir = environment(), simplify = F, USE.NAMES = T)
    }))
    re[['initialization']] = initialization
    
    input_env[[inputId]] = re
    invisible(re)
  }
  mount_demo_subject <- function(...){}
  
  
  output_env = new.env(parent = emptyenv())
  define_output <- function(definition, title = '', width = 12L, order = Inf){
    definition = substitute(definition)
    definition = match.call(definition = eval(definition[[1]]), definition)
    outputId = definition[['outputId']]
    if(!is.null(outputId)){
      definition[['outputId']] = paste0('..', outputId)
    }else{
      outputId = definition[['inputId']]
      definition[['inputId']] = paste0('..', outputId)
    }
    
    # output width
    
    width %?<-% 12
    assertthat::assert_that(width %in% 1:12, msg = 'Output width Must be integer from 1 to 12.')
    definition[['width']] = width
    
    re = list(
      outputId = outputId,
      title = title,
      definition = definition,
      order = order
    )
    class(re) = c('comp_output', 'list')
    
    
    output_env[[outputId]] = re
    invisible(re)
  }
  
  init_env = new.env(parent = emptyenv())
  init_env[['init']] = FALSE
  define_initialization = function(definition){
    definition = substitute(definition)
    init_env[['init']] = definition
  }
  scripts = new.env(parent = emptyenv())
  load_scripts = function(...){
    fs = unlist(list(...))
    fs = sapply(fs, get_path)
    scripts[['source']] = fs
  }
  
  tmp_env = new.env()
  
  return(list(
    content = content,
    input_env = input_env,
    output_env = output_env,
    init_env = init_env,
    script_env = scripts,
    tmp_env = tmp_env
  ))
  
}


# Function to parse inputs
parse_components <- function(module_id){
  envs = get_comp_env(module_id)
  # Find input init (rave_updates)
  has_content = get_content(content = envs$content, env = envs$tmp_env)
  
  tmp_env = envs$tmp_env
  input_env = envs$input_env
  output_env = envs$output_env
  
  # Find inputs
  input_layout = tmp_env[['input_layout']]
  inputs = as.list(input_env)
  defs = lapply(inputs, '[[', 'definition')
  names(defs) = NULL
  if(is.null(input_layout)){
    rave_inputs_quo = rlang::quo(rave_inputs(!!!defs))
  }else{
    rave_inputs_quo = rlang::quo(rave_inputs(!!!defs, .input_panels = !!input_layout))
  }
  # Generate rave_updates
  init_expr = envs$init_env$init
  inits = lapply(inputs, '[[', 'initialization')
  names(inits) = names(inputs)
  rave_update_quo = rlang::quo(rave_updates({eval(!!init_expr)}, !!!inits))
  
  # outputs
  output_layout = tmp_env[['output_layout']]
  comps = as.list(output_env)
  defs = lapply(comps, '[[', 'definition')
  if(length(defs)){
    titles = sapply(comps, '[[', 'title')
    names(defs) = titles
    order = order(sapply(comps, '[[', 'order'))
    defs = defs[order]
    
    # generate temp functions
    output_functions = lapply(comps, function(comp){
      rlang::quo(function(...){
        result = environment()
        do.call(!!comp$outputId, c(list(result), list(...)))
      })
    })
    
    names(output_functions) = paste0('..', sapply(comps, '[[', 'outputId'))
    
  }else{
    output_functions = NULL
    # need to make sure at least one output
    defs = list('No Output' = quote(textOutput('do_nothing', width = 12L)))
  }
  
  if(is.null(output_layout)){
    rave_output_quo = rlang::quo(rave_outputs(!!!defs))
  }else{
    rave_output_quo = rlang::quo(rave_outputs(!!!defs, .output_tabsets = !!output_layout))
  }
  rave_output_quo
  
  return(list(
    rave_inputs_quo = rave_inputs_quo,
    rave_update_quo = rave_update_quo,
    rave_output_quo = rave_output_quo,
    output_functions = output_functions,
    additional_scripts = envs$script_env$source,
    env = environment()
  ))
}

# parse_components and wrap them in an environment
init_module <- function(module_id){
  # Make sure subject is loaded
  has_subject = rave::any_subject_loaded()
  
  if(!has_subject){
    cat2('Error: No subject found! Please load one subject', level = 'ERROR')
  }
  
  # Still try to run the rest
  data_env = rave::getDefaultDataRepository()
  
  # get components
  envs = get_comp_env(module_id = module_id)
  has_content = get_content(content = envs$content, env = envs$tmp_env)
  
  # Initialize env
  param_env = new.env(parent = data_env)
  param_env$rave_checks = function(...){}
  
  for(f in envs$script_env[['source']]){
    param_env$..scr = f
    with(param_env, {
      source(file = ..scr, local = T)
    })
  }
  
  # initialize global variables
  init_expr = envs$init_env$init
  base::eval(init_expr, envir = param_env)
  
  # Initialize inputs
  inputs = as.list(envs$input_env)
  lapply(inputs, function(input){
    if(is(input, 'comp_input')){
      inputId = input$inputId
      def = input$definition
      f = def[[1]]
      args = formals(eval(f))
      value = def[['value']]
      value %?<-% def[['selected']]
      value %?<-% args[['value']]
      value %?<-% args[['selected']]
      
      init = input$initialization
      
      if(!is.null(init)){
        updates = rave::eval_dirty(init, env = param_env)
        updated_value = updates[['value']]
        updated_value %?<-% updates[['selected']]
        if(!is.null(updated_value)){
          value = updated_value
        }
      }
      value = eval(value)
      param_env[[inputId]] = value
      
      cat2(inputId, '<- ', paste(capture.output(cat2(value)), collapse = '\n'))
      
    }
  })
  
  return(param_env)
  
}


# Function to extract rave_execute
get_main_function <- function(module_id){
  path = get_path('inst', 'modules', module_id, 'main.R')
  content = readLines(path)
  
  expr = get_content(content = content, evaluate = F)
  
  expr[[length(expr) + 1]] = quote(return(environment()))
  
  main_quos = rlang::quo({
    !!! as.list(expr)
  })
  main_quos
}
