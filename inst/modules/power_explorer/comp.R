# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
  # 'inst/modules/power_explorer/3d_viewer.R',
  'inst/modules/power_explorer/exports.R'
)

define_initialization({
  ##
  ## Make sure power (referenced) exists
  ## with the following line, RAVE will pop up a dialogue if
  ## power is not loaded and ask users to load data
  ##
  rave_checks('power referenced')

  ##
  ## Get referenced power (Wavelet power)
  ##
  power = module_tools$get_power(referenced = TRUE)

  ## Shared variables
  frequencies = preload_info$frequencies
  time_points = preload_info$time_points
  electrodes = preload_info$electrodes
  epoch_data = module_tools$get_meta('trials')
})




#  ---------------------------------  Inputs -----------------------------------
# Define inputs

# Select from multiple choices,
define_input(
  definition = customizedUI(inputId = 'input_customized')
)

define_input_multiple_electrodes(inputId = 'ELECTRODE_TEXT')
define_input_frequency(inputId = 'FREQUENCY')
define_input_time(inputId = 'ANALYSIS_WINDOW', label='Analysis', initial_value = c(0,1))
define_input_time(inputId = 'BASELINE_WINDOW', label='Baseline', initial_value = c(-1,0))
define_input_condition_groups(inputId = 'GROUPS')

define_input(
  definition = selectInput('combine_method', 'Electrode Transforms',
                           choices = c('none', 'z-score', 'max-scale', '0-1 scale', 'rank'),
                           multiple = F, selected = 'none')
)

# define_input(
#   definition = selectInput('reference_type', 'Transform Reference',
#                            choices = c('Trial', 'Trial type', 'Active trial types', 'All trials'),
#                            selected='Trial')
# )
#
# define_input(
#   definition = selectInput('reference_group', 'Reference Group',
#                            choices = c('Per Electrode', 'All Electrodes'), selected = 'Per Electrode')
# )

define_input(
  definition = numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1)
)
define_input(
  definition = checkboxInput('log_scale', 'Log Freq (NI)')
)
define_input(
  definition = checkboxInput('sort_trials_by_type', 'Sort Trials')
)
define_input(
    definition = checkboxInput('collapse_using_median', 'Collapse w/ Median (NI)')
)

# Define layouts if exists
input_layout = list(
  '[#cccccc]Electrodes' = list(
    c('ELECTRODE_TEXT'),
    c('combine_method')#,
    #c('reference_type', 'reference_group')
  ),
  '[#99ccff]Trial Selector' = list(
    'GROUPS'
  ),
  'Analysis Settings' = list(
    'FREQUENCY',
    'BASELINE_WINDOW',
    'ANALYSIS_WINDOW'
  ),
  '[-][#aaaaaa]Export Options' = list(),
  '[-]Plotting' = list(
    c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
    c('max_zlim')
  )
)

# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
  definition = plotOutput(outputId = 'heat_map_plot'),
  title = 'Heat Map (Collapse trial)',
  width = 12,
  order = 1
)

define_output(
  definition = plotOutput('by_trial_heat_map'),
  title = 'Activity over time by trial (Collapse freq)',
  width = 12,
  order = 2
)

define_output(
  definition = plotOutput('over_time_plot'),
  title = 'Collapse freq+trial',
  width = 8,
  order = 3
)

define_output(
  definition = plotOutput(outputId = 'windowed_comparison_plot'),
  title = 'Collapse freq+time',
  width = 4,
  order = 4
)

# define_output(
#   definition = customizedUI('viewer_3d'),
#   title = '3D Viewer',
#   width = 12,
#   order = 5
# )

define_output_3d_viewer(
  outputId = 'power_3d',
  title = '3D Viewer for Power',
  surfaces = 'pial',
  multiple_subject = F,
  height = '70vh',
  order = 0,
  width = 12,
  additional_ui = tagList(
    selectInput(ns('viewer_3d_type'), 'Which statistics', choices = c('b', 't', 'p')),
    p(ns('blah'))
  )
)

# output_layout = list(
#   # 'Tabset One' = list(
#   #   'Multiple Output' = 'heat_map_plot'
#   # )
#   'Multiple Output' = 'heat_map_plot'
# )
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------




# -------------------------------- View layout ---------------------------------
quos = env$parse_components(module_id = 'power_explorer')

view_layout('power_explorer', launch.browser = T, sidebar_width = 3)
