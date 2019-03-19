read_data = function(file){
  tryCatch({
    
    dat = read.csv(file, stringsAsFactors = FALSE)
    
    stopifnot(ncol(dat) >= 4)
    
    ii = which(names(dat) == 'ProjectName')
    
    if(ii > 1){
      dat = dat[, -(seq_len(ii-1))]
    }
    
    # columns: ProjectName SubjectCode Electrode ...(values)
    cols = names(dat)
    stopifnot(
      cols[1] == 'ProjectName' &&
        cols[2] == 'SubjectCode' &&
        cols[3] == 'Electrode'
    )
    
    s = read.csv(file, header = F, nrows = 1)[, -(1:3)]
    s = unlist(s)
    names(s) = NULL
    attr(dat, 'col_vars') = s
    dat
    
  }, error = function(e){
    FALSE
  })
}


check_subject = function(project_name, subject_code, surface_types){
  data_dir = rave_options('data_dir')
  subject_id = paste0(project_name, '/', subject_code)
  pretty_id = stringr::str_remove_all(paste0(project_name, subject_code), '[^a-zA-Z0-9]')
  viewer_dir = file.path(data_dir, project_name, subject_code, 'rave', 'viewer')
  suma_dir = file.path(data_dir, project_name, subject_code, 'rave', 'suma')
  
  # Check surface_cache
  fs = paste0(pretty_id, '_%sh_', surface_types, '.json')
  fs = as.vector(sapply(fs, function(f){sprintf(f, c('l','r'))}))
  surface_cache = all(file.exists(file.path(viewer_dir, fs)))
  
  # Check surface_raw (141 asc)
  fs = paste0('std.141.%sh.', surface_types, '.asc')
  fs = as.vector(sapply(fs, function(f){sprintf(f, c('l','r'))}))
  surface_raw = all(file.exists(file.path(suma_dir, fs)))
  
  # Check transform_cache
  transform_cache = file.exists(file.path(suma_dir, 'T1_to_freesurf.txt'))
  
  # check transform_raw
  transform_raw = file.exists(file.path(suma_dir, 'fs_SurfVol_Alnd_Exp+orig.HEAD'))
  
  # check mapping_cache
  tbl = load_meta('electrodes', project_name, subject_code)
  cols = c('VertexNumber', 'Subcortical', 'SurfaceType', 'Hemisphere')
  mapping_cache = all(cols %in% names(tbl)) && any(tbl$VertexNumber >= 0)
  
  return(
    c(surface_cache || !surface_raw, surface_raw, transform_cache || !transform_raw, transform_raw, mapping_cache)
  )
}


data_checks = function(){
  has_data_file = local_data$has_data_file
  valid_data = local_data$valid_data
  contains_data = local_data$contains_data
  
  if(!has_data_file){
    return()
  }
  
  if(!valid_data){
    
    tbl = data.frame(
      ProjectName = project_name,
      SubjectCode = c(rep(SUBJECTS[1], 3), '...'),
      Electrode = c('1','2','...', ''),
      ValueName1 = c('10.2', '2.4', '...', ''),
      stringsAsFactors = F
    )
    
    s = paste0(utils::capture.output({print(tbl)}), collapse = '\n')
    
    
    return(tagList(
      hr(),
      p(
        'Data file is invalid. It Must be a .csv file, with at least 4 columns and the first three column names need to be ', 
        strong(
          'ProjectName, SubjectCode, Electrode'
        ), " (case sensiitive). For example:"
      ),
      pre(s)
    ))
  }
  
  if(!contains_data){
    ss = input$SUBJECTS
    if(!length(ss)){
      return(tagList(
        hr(),
        p('No subject is to be loaded?') 
      ))
    }
    return(tagList(
      hr(),
      p('Cannot find any data that maches your selections. Either ', 
        strong('ProjectName'), ' has no ', sprintf('"%s"', project_name), ', or ', 
        strong('SubjectCode'), ' contains no ', paste(sprintf('"%s"', ss), collapse = ', '))
    ))
  }
  
  # Data is valid, show data parameters
  vars = attr(local_data$dat, 'col_vars')
  if(is.numeric(vars)){
    ts_data = TRUE
  }else{
    ts_data = FALSE
  }

  return(tagList(
    hr(),
    checkboxInput(ns('ts_data'), 'Time Series Data', value = ts_data),
    checkboxInput(ns('sym'), 'Symmetric Range', value = get_input('sym', TRUE)),
    div(
      class = 'rave-grid-inputs', 
      div(
        style = 'flex-basis: 50%',
        uiOutput(ns('data_variable'))
      ),
      div(
        style = 'flex-basis: 25%',
        numericInput(ns('value_range1'), 'Plot Range: From', value = 0)
      ),
      div(
        style = 'flex-basis: 25%',
        numericInput(ns('value_range2'), '- To', value = 0)
      )
    # TODO: add color picker or even have descrete color scheme
    )
    
  ))
}

observeEvent(input$sym, {
  if(isTRUE(input$sym)){
    v1 = input$value_range1;
    v2 = input$value_range2
    if(length(v1) && !is.na(v1)){
      updateNumericInput(session, 'value_range1', value = -abs(v1))
      updateNumericInput(session, 'value_range2', value = abs(v1))
    }else if(length(v2) && !is.na(v2)){
      updateNumericInput(session, 'value_range1', value = -abs(v2))
      updateNumericInput(session, 'value_range2', value = abs(v2))
    }
  }
})

observeEvent(input$value_range1, {
  v1 = input$value_range1;
  v2 = input$value_range2
  sym = input$sym
  if(isTRUE(sym) && length(v1) && !is.na(v1)){
    if(!length(v2) || is.na(v2)){
      updateNumericInput(session, 'value_range1', value = -abs(v1))
      updateNumericInput(session, 'value_range2', value = abs(v1))
      
    }else if(v1 + v2 != 0){
      updateNumericInput(session, 'value_range1', value = -abs(v1))
      updateNumericInput(session, 'value_range2', value = abs(v1))
    }
    
  }
})

observeEvent(input$value_range2, {
  v2 = input$value_range1
  v1 = input$value_range2
  sym = input$sym
  if(isTRUE(sym) && length(v1) && !is.na(v1)){
    if(!length(v2) || is.na(v2)){
      updateNumericInput(session, 'value_range1', value = -abs(v1))
      updateNumericInput(session, 'value_range2', value = abs(v1))
      
    }else if(v1 + v2 != 0){
      updateNumericInput(session, 'value_range1', value = -abs(v1))
      updateNumericInput(session, 'value_range2', value = abs(v1))
    }
    
  }
})

output$data_value_range <- renderUI({
  
  
  if(sym){
    div(
      style = 'flex-basis: 50%',
      numericInput(ns('value_range_up'), 'Plot Range', value = v_up, min = 0)
    )
  }else{
    div(
      style = 'flex-basis: 50%',
      numericInput(ns('value_range_up'), 'Plot Range', value = v_up, min = 0)
    )
  }
  
})



output$data_variable <- renderUI({
  vars = attr(local_data$dat, 'col_vars')
  ts_data = input$ts_data
  
  if(ts_data){
    vars = as.numeric(vars)
    max = max(vars)
    min = min(vars)
    var_ui = sliderInput(ns('time'), label = 'Time Range', max = max, min = min, 
                         value = get_input('time', c(min, max)))
  }else{
    vars = as.character(vars)
    var_ui = selectInput(ns('var'), 'Variable to Visualize', choices = vars, 
                         selected = get_input('var', NULL))
  }
  
  var_ui
})









