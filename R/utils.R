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

#' @export
dev_ravebuiltins <- function(expose_functions = FALSE, reload = TRUE){
  .fs = list.files(system.file('tools', package = 'ravebuiltins'), pattern = '\\.R$', full.names = T)
  rave_dev_load <- function(local = TRUE){
    # Get package name
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
      return(globalenv())
    }
    
    invisible()
  }
  # Reload first 
  if(reload){
    env = rave_dev_load(local = T)
    env$reload_this_package(expose = FALSE, clear_env = FALSE)
  }
  
  env = rave_dev_load(local = !expose_functions)
  
  env$load_dev_env()
  
  return(invisible(env))
}




# Function to run module
get_module <- function(module_id, interactive = FALSE, check_dependencies = TRUE, force_update_remote = FALSE){
  
  env = dev_ravebuiltins(expose_functions = F, reload = FALSE)
  
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
  
  sel = names(main_quos) %in% c('async')
  normal_quos = main_quos[!sel]
  async_quo = main_quos[sel]
  async = length(async_quo)
  
  body(FUN) = rlang::quo_squash(rlang::quo({
    !!!normal_quos
    
    results = environment()
    ..env = list()
    
    ..env$results = new.env()
    
    ..tmp = new.env()
    
    ..tmp[['..async']] = FALSE
    
    if(!!async){
      ..tmp[['..async']] = TRUE
      ..tmp[['..async_quo']] = quote(!!!async_quo[[1]])
      ..tmp[['..async_var']] = NULL
      ..tmp[['..packages']] = str_match(search(), '^package:(.+)$')[,2]
      ..tmp[['..packages']] = unique(..tmp[['..packages']][!is.na(..tmp[['..packages']])])
      ..tmp[['..rave_future_obj']] = future::future({
        rave::eval_dirty(..async_quo)#, env = async_env)
        if(is.null(..async_var)){
          return(environment())
        }else{
          re = sapply(..async_var, get0, simplify = F, USE.NAMES = T)
          return(list2env(re))
        }
      }, packages = ..tmp[['..packages']], evaluator = future::multiprocess, 
      envir = ..tmp, gc = T)
    }
    
    
    ..env$results$get_value = function(key, ifNotFound = NULL){
      get0(key, envir = results, ifnotfound = ifNotFound)
    }
    ..env$results$async_value = function(key){
      if(!..tmp[['..async']]){
        stop('This module has no async part.')
      }else{
        if(future::resolved(..tmp[['..rave_future_obj']])){
          env = ..tmp[['..rave_future_env']]
          if(!is.environment(env)){
            env = ..tmp[['..rave_future_env']] = future::value(..tmp[['..rave_future_obj']])
          }
          get0(key, envir = env)
        }
      }
      
    }
    
    ..re = sapply(!!outputIds, function(nm){
      ..f = get0(nm, envir = results, inherits = TRUE, ifnotfound = NULL)
      if(!is.function(..f)){
        return(function(...){
          cat2('Function ', nm, ' is not available.', level = 'ERROR')
        })
      }else{
        fm = formals(..f)
        
        if(!length(fm)){
          # Case 1: fm is NULL, meaning this is temp function or customized output
          ..f
        }else{
          # Case 2: ..f is a package function
          fm = fm[-1]
          nms = names(fm)
          has_dots = '...' %in% nms
          nms = nms[!nms %in% c('', '...')]
          
          f = function(){
            args = sapply(nms, get0, inherits = FALSE, simplify = F, USE.NAMES = T)
            if(has_dots){
              args = c(list(..env$results), args, list(...))
            }else{
              args = c(list(..env$results), args)
            }
            
            do.call(..f, args)
          }
          formals(f) = fm
          f
        }
      }
      
      # eval(call("function", as.pairlist(fm), rhs), env, env)
      # call("function", as.pairlist(fm), rhs)
    }, simplify = F, USE.NAMES = T)
    
    return(c(..env, ..re))
  }))
  formals(FUN) = args
  
  return(FUN)
}
