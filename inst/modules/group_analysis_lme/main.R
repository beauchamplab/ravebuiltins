# Main algorithm - rave_executes

# Initialize inputs
# devtools::document()
rm(list=ls(all.names = TRUE)); rstudioapi::restartSession()
ravebuiltins:::dev_ravebuiltins(T)
# mount_demo_subject()
 mount_demo_subject(subject_code = 'YCZ', 'Sentences', epoch='YCZ_gingko',
                electrodes=50:56, time_range=c(1.5, 4), force_reload_subject=TRUE)
# init_module(module_id = 'group_analysis_lme', debug = TRUE)
.__DEBUG__ = 1
view_layout('group_analysis_lme', sidebar_width = 3, launch.browser = T)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

# not really used
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)
# devtools::document()
ravebuiltins::dev_ravebuiltins(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'group_analysis_lme'
module = ravebuiltins:::debug_module('group_analysis_lme')

results = result$results

view_layout('group_analysis_lme', sidebar_width = 3, launch.browser = T)
