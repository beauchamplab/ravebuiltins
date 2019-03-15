# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'group_analysis_lme', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

# local_data$participants = participants

# Compromise, I'll just look at the first subject
r = lapply(participants, get_analysis); names(r) = participants
local_data$potential_analysis = r

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)
# devtools::document()
ravebuiltins::dev_ravebuiltins(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'group_analysis_lme'
module = ravebuiltins:::debug_module('group_analysis_lme')

result = module(ANALYSIS_WINDOW = 0)
result$phase_histogram()
result$itpc_plot()
result$itpc_time_plot()
result$phase_plot()

results = result$results

view_layout('group_analysis_lme', sidebar_width = 3, launch.browser = T)


