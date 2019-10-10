# Module 1: file uploader


##### Step 1: Add the following into define_initialization ####

# project_name = subject$project_name
# project_dir = dirname(subject$dirs$subject_dir)
# subjects = get_subjects(project_name)
# 
# power_explorer_dir = file.path(project_dir, '_project_data', 'power_explorer')
# group_analysis_src = file.path(project_dir, '_project_data', 'group_analysis_lme', 'source')
# 
# rescan_source = function(update = TRUE, new_selected = NULL){
#     choices = c(
#         list.files(power_explorer_dir, pattern = '\\.[cC][sS][vV]$'),
#         list.files(group_analysis_src, pattern = '\\.[cC][sS][vV]$')
#     )
#     # Order file names by date-time (descending order)
#     dt = stringr::str_extract(choices, '[0-9]{8}-[0-9]{6}')
#     od = order(strptime(dt, '%Y%m%d-%H%M%S'), decreasing = TRUE)
#     choices = choices[od]
#     if(update && is_reactive_context()){
#         selected = c(source_files, new_selected)
#         updateSelectInput(session, 'source_files', choices = choices, selected = selected)
#     }
#     return(choices)
# }

##### Step 2:

