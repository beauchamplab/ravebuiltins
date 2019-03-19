input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()
local_data %?<-% reactiveValues()

get_input = function(key, default = NULL){
  v = isolate(local_data[[key]])
  if(!length(v)) v = default
  return(v)
}

observe({
  # move "EVERYTHING" from input to local_data
  input_inst = reactiveValuesToList(input)
  for(nm in names(input_inst)){
    local_data[[nm]] = input_inst[[nm]]
  }
}, priority = 998L)

subject_checks <- function(){
  local_data$force_update
  
  miss_f = local_data$miss_f
  miss_c = local_data$miss_c
  checks = local_data$file_checks
  has_surface = local_data$has_surface
  has_subject = local_data$has_subject
  
  if(!length(has_subject) || !has_subject){
    return(p(
      strong('Please select at least one subject!')
    ))
  }
  if(!has_surface){
    return(p(
      strong('Please select at least one surface type!')
    ))
  }
  
  ui1 = NULL
  if(miss_f){
    ui1 = p(
      strong('The following subject(s) have missing files:'),
      tags$ul(
        tagList(
          lapply(checks, '[[', 'missing_files')
        )
      )
    )
  }
  ui2 = NULL
  if(miss_c){
    ui2 = p(
      strong('The following subject(s) have missing cache/calculation:'),
      tags$ul(
        tagList(
          lapply(checks, '[[', 'missing_cache')
        )
      )
    )
  }
  
  if(miss_f){
    btn = tags$small('You can still generate 3D viewer with missing files. However, the viewer might not be complete.')
  }else if(miss_c){
    btn = tags$small("Don't worry, RAVE will calculate them for you")
  }else{
    btn = tags$small("You are good to go!")
  }
  
  tagList(
    ui1, ui2, p(btn)
  )
  
}




# assign values load mesh
# observeEvent(list(input$viewer_update_btn, input$gen_3d), {
#   re = update_data()
#   if(is.list(re)){
#     re$brain$view(value_range = re$value_range, time_range = re$time_range)
#   }else{
#     NULL
#   }
# })





viewer_wrapper = function(){
  threeBrain::threejsBrainOutput(ns('viewer'), height = '85vh')
}


output$viewer <- threeBrain::renderBrain({
  
  input$viewer_update_btn
  input$gen_3d
  
  tryCatch({
    re = isolate(update_data())
    local_data$last_brain = re$brain
    
    re$brain$view(value_range = re$value_range, time_range = re$time_range)
    # local_env$brain$view( value_range = value_range, time_range = time_range )
    
    
  }, error = function(e){
    msg = 'Please click button "generate 3D viewer" to start'
    cond = structure(list(message = msg), 
                     class = c("shiny.silent.error", "validation", "error", "condition"))
    local_data$last_brain = re$brain
    stop(cond)
  })
  
  
  
})





update_data = function(){
  # multiple_subject SUBJECTS SURFACE_TYPES
  has_subject = local_data$has_subject
  has_surface = local_data$has_surface

  if(!length(has_subject) || !has_subject || !has_surface){
    return()
  }
  checks = local_data$file_checks
  
  prog = rave::progress('Generating 3D Viewer', max = 3)
  
  if(multiple_subject){
    
    prog$inc('Check mappings')
    lapply(checks, function(x){
      # list(
      #   subject_code = s,
      #   result = res,
      #   missing_files = missing_files,
      #   missing_cache = missing_cache
      # )
      if( !x$result[5] ){
        # need to create cache for the subject (map to template brain)
        subject_id = paste0(project_name, '/', x$subject_code)
        brain = rave_brain2(surfaces = SURFACE_TYPES, multiple_subject = F)
        try({
          brain$calculate_template_brain_location(subject = subject_id)
        })
        
      }
    })
    
    prog$inc('Check/Download N27 brain')
    brain = rave_brain2(multiple_subject = multiple_subject, surfaces = SURFACE_TYPES)
  }else{
    prog$inc('Importing')
    brain = rave_brain2(multiple_subject = multiple_subject, surfaces = SURFACE_TYPES)
  }
  
  ts_data = local_data$ts_data
  dat = local_data$dat
  vars = attr(dat, 'col_vars')
  
  
  if(is.null(ts_data)){
    time_range = NULL
    col_sel = NULL
  }else if (ts_data){
    time_range = get_input('time', NULL)
    vars = as.numeric(vars)
    col_sel = rutabaga::is_within(vars, time_range)
  }else{
    var = get_input('var', NULL)
    var = as.character(vars)
    time_range = NULL
    col_sel = vars == var
  }
  
  value_range = c(
    get_input('value_range1', NULL),
    get_input('value_range2', NULL)
  )
  if(length(value_range) != 2 || any(is.na(value_range)) || value_range[1] >= value_range[2]){
    value_range = NULL
  }
  
  
  for(sub in SUBJECTS){
    try({
      subj = Subject$new(project_name = project_name, subject_code = sub, strict = F)
      brain$load_electrodes(subject = subj)
      brain$load_surfaces(subject = subj)
      
      # try to load subject data
      idx = which(col_sel) + 3
      subset = dat[dat$SubjectCode == sub, ]
      if(nrow(subset)){
        for(ii in seq_len(nrow(subset))){
          row = subset[ii, ]
          
          if(ts_data){
            value = as.numeric(row[, -(1:3)])
            time = vars
          }else{
            value = as.numeric(row[, idx])
            time = seq_along(value)
          }
          
          brain$set_electrode_value(subj, electrode = row$Electrode, value = value, time = time)
        }
      }
      
    }, silent = T)
  }
  
  
  prog$close()
  
  return(list(
    brain = brain,
    value_range = value_range,
    time_range = time_range
  ))
    
}



viewer_inputs3 = function(){
  if(is.null(local_data$last_brain)){
    return(p(
      'No valid brain widget found!'
    ))
  }
  tagList(
    textInput(ns('export_title'), 'Widget Title', value = get_input('export_title', '3D Viewer')),
    downloadLink(ns('export_btn'), 'Download Viewer')
  )
}


output$export_btn <- downloadHandler(
  filename = 'viewer.zip',
  content = function(file){
    tryCatch({
      showNotification(p('Generating in progress. This may take a while... '), duration = NULL, id = ns('viewer_export_noti'), type = 'message')
      brain = isolate(local_data$last_brain)
      title = isolate(input$export_title)
      tmp_dir = file.path(tempdir(), paste(sample(c(letters, 0:9), 17), collapse = ''))
      re = brain$save_brain(directory = tmp_dir, title = title, as_zip = T)
      file.copy(re$zipfile, file)
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
    }, error = function(e){
      cat2(e, level = 'WARNING')
    }, finally = {
      try({
        removeNotification(id = ns('viewer_export_noti'))
      })
      
    })
  }
)
