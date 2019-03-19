phase_3d_fun = function(brain){
  showNotification(p('Generating in progress, need some time...'))
  
  dat = NULL
  # rave::cache(key = list(
  #   list(BASELINE_WINDOW, preload_info)
  # ), val = get_summary())
  # # for each electrode, we want to test the different conditions
  # .FUN <- if(length(levels(dat$condition)) > 1) {
  #   
  #   if (length(levels(dat$condition)) == 2) {
  #     function(x) {
  #       res = get_t(power ~ condition, data=x)
  #       res = c(res[1] - res[2], res[3], res[4])
  #       res %>% set_names(c('b', 't', 'p'))
  #     }
  #   } else {
  #     function(x) {
  #       get_f(power ~ condition, data=x)
  #     }
  #   }
  # } else {
  #   function(x) {
  #     get_t(x$power) %>% set_names(c('b', 't', 'p'))
  #   }
  # }
  # 
  
  #for debugging
  # brain = get_brain()
  
  dat <- list(elec = preload_info$electrodes)
  values = sapply(unique(dat$elec), function(e){
    sub = NULL#dat[dat$elec == e, ]
    re = NULL#.FUN(sub)
    v = rnorm(10)#re[input$viewer_3d_type]
    brain$set_electrode_value(subject, e, value = v, time = 1:10)
    return(v)
  })
  
  brain$view(value_range = c(-1,1) * max(abs(values)))
}

observeEvent(input$phase_3d__mouse_event, {
  mouse_event = input$phase_3d__mouse_event$event
  object = input$phase_3d__mouse_event$object
  
  # print(input$phase_3d__mouse_event)
  
  # This is dirty, i think we can provide function to get which electrode chosen
  if(mouse_event$action == 'dblclick' && isTRUE( object$is_electrode )){
    # Get object chosen, is it an electrode?
    # Use isTRUE() to validate since object$is_electrode could be NULL
    e = stringr::str_match(object$name, '^Electrode ([0-9]+)')[2]
    e = as.integer(e)
    if(e %in% preload_info$electrodes){
      updateSelectInput(session, 'ELECTRODE', selected = e)
      showNotification(p('Switched to electrode ', e), type = 'message', id = ns('phase_3d_widget__mouse'))
    }else{
      showNotification(p('Electrode ', e, ' is not loaded.'), type = 'warning', id = ns('phase_3d_widget__mouse'))
    }
  }
})
