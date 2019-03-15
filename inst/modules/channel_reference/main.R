# Main algorithm - rave_executes

# Initialize inputs
devtools::document()
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject()

init_module(module_id = 'channel_reference', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

# Part 0: load voltage data on the fly

# Part 1: Load or new reference scheme
load_reference()


# Part 2: show a specific group
local_data$refresh = Sys.time()

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
# require(ravebuiltins)
# devtools::document()
rave_tools = ravebuiltins::dev_ravebuiltins(expose_functions = T)
# reload_this_package(expose = T, clear_env = F)
mount_demo_subject(force_reload_subject = T)

# module_id = 'phase_explorer'
module = ravebuiltins:::debug_module('phase_explorer')

result = module(ANALYSIS_WINDOW = 0)
result$phase_histogram()
result$itpc_plot()
result$itpc_time_plot()
result$phase_plot()

results = result$results

rave_tools$view_layout('channel_reference', sidebar_width = 3, launch.browser = T)


