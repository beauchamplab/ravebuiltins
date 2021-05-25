# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()
# view_layout('overview_viewer_3d')
# init_module(module_id = 'overview_viewer_3d', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

# Step 1. collect variables needed
subject_codes = unique(c(subject$subject_code, subject_codes))
project_name = subject$project_name
local_data$current_subjecct = subject$subject_code
local_data$current_project = project_name
project_dir = normalizePath(file.path(subject$dirs$rave_dir, '../../'))

# Step 2. Read all csv files and combine them
local_env$tables = list()


selected_paths = lapply(data_files, function(p){
  path = normalizePath(file.path(project_dir, '_project_data', '3dviewer',  p), mustWork = FALSE)
  if(!file.exists(path)){ return() }
  
  # Read in path
  tryCatch({
    if(endsWith(path, 'csv')) {
      dat = read.csv( path , stringsAsFactors = FALSE )
    } else if (endsWith(path, 'fst')) {
      dat = fst::read_fst(path)
    }
    dipsaus::cat2('34')
    nms = names(dat)
    if( !nrow(dat) ){ return() }
    dipsaus::cat2('37')
    # 1. Electrode
    if( !'Electrode' %in% nms ){ return() }
    
    # 2. Subject
    if( !'Subject' %in% nms ){
      # try to guess subject code from p
      subcode = unlist(stringr::str_split(p, '[\\\\/_]'))[[1]]
      if( !subcode %in% subject_codes ){ return() }
      dat$Subject = subcode
    }
    
    # 3. project
    if( 'Project' %in% nms ){
      dat = dat[dat$Project %in% subject$project_name, ]
    }
    dat$Project = subject$project_name
    
    # 4. Time
    if( 'Time' %in% nms ){
      dat$Time = as.numeric(dat$Time)
      dat = dat[!is.na(dat$Time), ]
    }else{
      dat$Time = 0
    }
    
    local_env$tables[[ path ]] = dat
    return(path)
  }, error = function(e){
    print(e)
    NULL
  })
})

selected_paths = unlist( selected_paths )
# Find table names
table_headers = lapply(selected_paths, function(path){ names(local_env$tables[[ path ]]) })
table_headers = unique( unlist( table_headers ) )
table_headers = c('Project', 'Subject', 'Electrode', 'Time',
                  table_headers[!table_headers %in% c('Project', 'Subject', 'Electrode', 'Time')])

elec_value_table = structure(lapply(table_headers, function(x){NULL}), 
                             names = table_headers, class = 'data.frame')

# This can't be a file path
local_env$tables[['#$..^']] = elec_value_table
combined_table = do.call('rbind', lapply(c('#$..^', selected_paths), function(path){
  re = local_env$tables[[ path ]]
  if( !is.data.frame(re) || !nrow(re)){ return( NULL ) } # this cannot happen but just in case
  for( nm in table_headers[!table_headers %in% names(re)] ){
    re[[nm]] = NA
  }
  re
}))

# print(names(local_env$tables))

# Step 3. generate combined csv tables
combined_table = combined_table[, table_headers]


# Step 4. collect freesurfer data
progress = rave::progress('Importing from FreeSurfer files', max = length(subject_codes) + 1)
on.exit({ progress$close() })

progress$inc('Initializing...')
brain = lapply(subject_codes, function(subject_code){
  progress$inc(sprintf('Import %s (might take a while)', subject_code))
  check_result = rave:::check_subjects2(project_name = project_name, 
                                        subject_code = subject_code, quiet = TRUE)
  if( check_result$check$rave_dir ){
    sub = rave::Subject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
    return(rave::rave_brain2(sub, surfaces = surface_types))
  }
  return(NULL)
})

# Step 5. if template, use it
brain = dipsaus::drop_nulls(brain)
if( isTRUE(use_template) || length(brain) > 1 ){
  brain = threeBrain::merge_brain(.list = brain)
}else if(length(brain)){
  brain = brain[[1]]
}

if( 'R6' %in% class(brain) && is.data.frame(combined_table) && nrow(combined_table) ){
  brain$set_electrode_values(combined_table)
}


# Step 6. refresh UI
local_data$viewer_result = Sys.time()

# step 7. let viewer update
ctx = rave::rave_context()
print(ctx$context)
if(ctx$context %in% c('rave_running', 'rave_module_debug')) {
  dipsaus::cat2("Updating 3D viewer")
  dipsaus::set_shiny_input(session = session, inputId = 'brain_viewer_btn',
                           value = 1, method = 'proxy', priority = 'event')
}

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)
# devtools::document()
rave_tools = ravebuiltins::dev_ravebuiltins(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'overview_viewer_3d'
module = ravebuiltins:::debug_module('overview_viewer_3d')

result = module(analysis_window = 0)
result$phase_histogram()
result$itpc_plot()
result$itpc_time_plot()
result$phase_plot()

results = result$results

rave_tools$view_layout('overview_viewer_3d', sidebar_width = 3, launch.browser = T)


