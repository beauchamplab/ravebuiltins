# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'overview_viewer_3d', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

# Check all the data
mis_terms = c('Surface cache', 'Surface files', 'Transform matrix', 'Surface volume file', 'Mapping to template')

has_subject = length(SUBJECTS) > 0
multiple_subject = load_n27 || length(SUBJECTS) > 1

has_surface = length(SURFACE_TYPES) > 0

local_data$has_subject = has_subject
local_data$has_surface = has_surface
######'

checks = sapply(SUBJECTS, function(s){
  res = check_subject(project_name, s, SURFACE_TYPES)
  
  missing_files = paste(mis_terms[c(2,4)][!res[c(2,4)]], collapse = ', ')
  if(missing_files != ''){
    missing_files = tags$li(strong(s), missing_files)
  }else{
    missing_files = NULL
  }
  if(multiple_subject){
    missing_cache = paste(mis_terms[c(1,3,5)][!res[c(1,3,5)]], collapse = ', ')
  }else{
    missing_cache = paste(mis_terms[c(1,3)][!res[c(1,3)]], collapse = ', ')
  }
  if(missing_cache != ''){
    missing_cache = tags$li(strong(s), missing_cache)
  }else{
    missing_cache = NULL
  }
  
  list(
    subject_code = s,
    result = res,
    missing_files = missing_files,
    missing_cache = missing_cache
  )
}, USE.NAMES = T, simplify = F)

miss_f = any(vapply(checks, function(x){ !is.null(x$missing_files) }, FUN.VALUE = FALSE))
miss_c = any(vapply(checks, function(x){ !is.null(x$missing_cache) }, FUN.VALUE = FALSE))

local_data$file_checks = checks
local_data$miss_f = miss_f
local_data$miss_c = miss_c

######'

# load data
has_data_file = FALSE
valid_data = FALSE
contains_data = FALSE
dat = NULL

if(length(DATA_FILE) && length(DATA_FILE$datapath)){
  has_data_file = TRUE
  dat = read_data(DATA_FILE$datapath)
  if(is.data.frame(dat)){
    dat = dat[dat$ProjectName == project_name & dat$SubjectCode %in% SUBJECTS, ]
    valid_data = TRUE
    if(nrow(dat)){
      contains_data = TRUE
    }
  }
}

local_data$dat = dat
local_data$has_data_file = has_data_file
local_data$valid_data = valid_data
local_data$contains_data = contains_data

######'
local_data$force_update = Sys.time()



# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)
# devtools::document()
rave_tools = ravebuiltins::dev_ravebuiltins(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'overview_viewer_3d'
module = ravebuiltins:::debug_module('overview_viewer_3d')

result = module(ANALYSIS_WINDOW = 0)
result$phase_histogram()
result$itpc_plot()
result$itpc_time_plot()
result$phase_plot()

results = result$results

rave_tools$view_layout('overview_viewer_3d', sidebar_width = 3, launch.browser = T)


