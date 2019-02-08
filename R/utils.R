# Function to load all dev funtions and wrap them within an environment

cat2 <- function(..., end = '\n', level = 'DEBUG', print_level = FALSE, pal = list(
  'DEBUG' = 'grey60',
  'INFO' = '#1d9f34',
  'WARNING' = '#ec942c',
  'ERROR' = '#f02c2c',
  'FATAL' = '#763053',
  'DEFAULT' = '#000000'
)){
  if(system.file('', package = 'rutabaga') != ''){
    f = do.call('::', list(pkg = 'rutabaga', name = 'cat2'))
  }else{
    f = function(..., end = end, level = level, print_level = print_level, pal = pal){
      base::cat(...)
    }
  }
  f(..., end = end, level = level, print_level = print_level, pal = pal)
}

rave_dev_load <- function(local = TRUE){
  # Get package name
  
  .fs = list.files(system.file('tools', package = 'ravebuiltins'), pattern = '\\.R$', full.names = T)
  if(local){
    env = new.env()
    with(env, {
      for(.f in .fs){
        source(.f, local = T) 
      }
    })
    return(env)
  }else{
    for(.f in .fs){
      source(.f, local = F) 
    }
  }
  
  invisible()
}


#' @export
dev_ravebuiltins <- function(expose_functions = FALSE){
  env = rave_dev_load(local = !expose_functions)
  
  if(is.environment(env)){
    env$load_dev_env()
    return(invisible(env))
  }else{
    load_dev_env()
    return(invisible(globalenv()))
  }
}




# Function to run module
get_module <- function(module_id, interactive = FALSE, check_dependencies = TRUE, force_update_remote = FALSE){
  env = dev_ravebuiltins(expose_functions = F)
  
  # env$mount_demo_subject()
  
  # Need to load subject first
  has_subject = rave::any_subject_loaded()
  
  if(!has_subject){
    cat2('Error: No subject found! Please load one subject', level = 'ERROR')
  }
  
  if(has_subject && !'rave_data' %in% search()){
    rave::attachDefaultDataRepository()
  }
  
  param_env = env$init_module(module_id = module_id)
  
  runtime_env = new.env(parent = param_env)
  
  envs = env$get_comp_env(module_id = module_id)
  has_content = env$get_content(content = envs$content, env = envs$tmp_env)
  inputs = lapply(envs$input_env, function(comp){
    if(is(comp, 'comp_input')){
      return(comp$inputId)
    }else{
      NULL
    }
  })
  inputs = unlist(inputs); names(inputs) = NULL
  
  args = as.list(param_env)[inputs]
  
  main_quos = env$get_main_function(module_id)
  
  outputIds = lapply(envs$output_env, function(comp){
    if(is(comp, 'comp_output')){
      return(comp$outputId)
    }else{
      NULL
    }
  })
  outputIds = unlist(outputIds)
  
  
  FUN = function(){}
  
  environment(FUN) = runtime_env
  body(FUN) = rlang::quo_squash(rlang::quo({
    !!main_quos
    
    results = environment()
    ..re = sapply(!!outputIds, function(nm){
      ..f = get0(nm, envir = results, inherits = TRUE, ifnotfound = NULL)
      function(...){
        if(is.null(..f)){
          cat2('Function ', nm, ' is not available.', level = 'ERROR')
          return(invisible())
        }else{
          return(..f(results, ...))
        }
      }
    }, simplify = F, USE.NAMES = T)
    ..re$results = results
    return(..re)
  }))
  formals(FUN) = args
  
  return(FUN)
}
