# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'overview_viewer_3d', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE


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


