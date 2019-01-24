view_layout <- function(module_id, sidebar_width = 5, launch.browser = rstudio_viewer){ 
  quos = parse_components(module_id)
  
  
  src = sapply(quos$additional_scripts, function(f){
    quo = rlang::quo(source(!!f, local = TRUE))
    paste(deparse(rlang::quo_squash(quo)), collapse = '\n')
  })
  names(src) = NULL
  
  exec = rlang::quo(rave_execute({eval(!!get_main_function(module_id))})) 
  
  funs = sapply(names(quos$output_functions), function(nm){
    f = quos$output_functions[[nm]]
    
    s = paste(deparse(rlang::quo_squash(f)), collapse = '\n')
    s = paste(nm, '<-', s)
    s
  })
  
  s = c(
    src, 
    deparse(rlang::quo_squash(quos$rave_inputs_quo)),
    deparse(rlang::quo_squash(quos$rave_update_quo)),
    deparse(rlang::quo_squash(quos$rave_output_quo)),
    deparse(rlang::quo_squash(exec)),
    funs
  )
  tmpfile = tempfile(pattern = 'junk')
  writeLines(s, tmpfile)
  m = rave::ModuleEnvir$new(module_id = module_id, label_name = get_module_label(module_id),
                            script_path = tmpfile)
  m$sidebar_width = sidebar_width
  init_app(m, launch.browser = launch.browser, disable_sidebar = T, simplify_header = T)
  
}
