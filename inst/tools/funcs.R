load_scripts <- function(...){
  src = c(...)
  
  for(s in src){
    cat2('Loading source - ', s, '\n', level = 'INFO', sep = '')
    source(get_path(s), local = F)
  }
}

define_initialization <- function(expr){
  expr = substitute(expr)
  eval(expr, envir = parent.frame())
}


define_input <- function(definition, init_args, init_expr){
  definition = substitute(definition)
  init_expr = substitute(init_expr)
  
  parser = rave:::comp_parser()
  
  comp = parser$parse_quo(rlang::quo(!!definition))
  
  def_text = stringr::str_trim(deparse(comp$expr))
  def_text = paste(def_text, collapse = ' ')
  input_id = comp$inputId
  
  f = eval(definition[[1]])
  env_name = environmentName(environment(f))
  if(env_name == ''){env_name = '<No Name>'}
  
  rutabaga::cat2('Input Definition - ', level = 'INFO')
  rutabaga::cat2(' ', def_text, level = 'INFO', pal = list('INFO' = 'blue'))
  
  rutabaga::cat2('Package/Environment - \t', level = 'INFO', end = '')
  rutabaga::cat2(env_name, level = 'INFO', pal = list('INFO' = 'blue'))
  
  val = comp$initial_value
  
  
  
  
  # Update info
  if(!missing(init_args)){
    rutabaga::cat2('Updating Input Parameter(s) - ', level = 'INFO')
    
    env = new.env(parent = parent.frame())
    eval(init_expr, envir = env)
    for(arg in init_args){
      v = env[[arg]]
      v = paste(stringr::str_trim(deparse(v)), collapse = ' ')
      rutabaga::cat2(' ', arg, '-', v, level = 'INFO', pal = list('INFO' = 'blue'))
    }
    
    if('value' %in% init_args){
      val = env[[arg]]
    }
    
  }
  
  v = paste(stringr::str_trim(deparse(val)), collapse = ' ')
  
  rutabaga::cat2('Input Value - \t', level = 'INFO', end = '')
  rutabaga::cat2(input_id, '=', v, level = 'INFO', pal = list('INFO' = 'blue'))
  
  assign(input_id, val, envir = parent.frame())
  invisible(val)
}


define_input(
  definition = textInput('electrode_text', 'Electrodes', value = "", placeholder = '1-5,8,11-20'),
  init_args = c('label', 'value'),
  init_expr = {
    last_input = cache_input('electrode_text', val = as.character(electrodes[1]))
    e = rutabaga::parse_svec(last_input)
    e = e[e %in% electrodes]
    if(!length(e)){
      e = electrodes[1]
    }
    value = rutabaga::deparse_svec(e)
    label = 'Electrodes (' %&% deparse_selections(electrodes) %&% ')'
  }
)


define_output <- function(definition, title, width, order){
  
  assertthat::assert_that(width %in% 1:12, msg = 'Width must be from 1 to 12')
  
  parser = rave:::comp_parser()
  definition = substitute(definition)
  
  comp = parser$parse_quo(rlang::quo(!!definition))
  
  f = eval(definition[[1]])
  env_name = environmentName(environment(f))
  if(env_name == ''){env_name = '<No Name>'}
  
  rutabaga::cat2('Title - \t\t', level = 'INFO', end = '')
  rutabaga::cat2(title, level = 'INFO', pal = list('INFO' = 'blue'))
  
  rutabaga::cat2('Definition - \t\t', level = 'INFO', end = '')
  rutabaga::cat2(paste(stringr::str_trim(deparse(comp$expr)), collapse = ' '), level = 'INFO', pal = list('INFO' = 'blue'))
  
  rutabaga::cat2('Package/Environment - \t', level = 'INFO', end = '')
  rutabaga::cat2(env_name, level = 'INFO', pal = list('INFO' = 'blue'))
  
  rutabaga::cat2('Width - \t\t', level = 'INFO', end = '')
  rutabaga::cat2(sprintf('%d (%.1f%% of output panel width)', width, width/12*100), level = 'INFO', pal = list('INFO' = 'blue'))
  
  rutabaga::cat2('Order - \t\t', level = 'INFO', end = '')
  rutabaga::cat2(order, level = 'INFO', pal = list('INFO' = 'blue'))
  
  # try to locate function

  output_id = comp$outputId
  
  pname = get_package_name()
  penv  = loadNamespace(pname)
  f = get0(output_id, envir = penv, ifnotfound = NULL, inherits = FALSE)
  
  if(is.function(f)){
    if(length(formals(f))){
      rutabaga::cat2('Output function `', output_id, '` found in package ', pname, '.', level = 'INFO', sep = '')
    }else{
      rutabaga::cat2('Output function `', output_id, '` MUST take in at least one argument(s)!', level = 'ERROR', sep = '')
    }
  }else{
    rutabaga::cat2('Cannot find output function `', output_id, '` in package ', pname, '!', level = 'ERROR', sep = '')
  }
  
}
