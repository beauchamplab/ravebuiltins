# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()

env$view_layout('power_explorer')
# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
# TODO get_module doesn't handle this well
# load_scripts(
#   'inst/modules/power_explorer/3d_viewer.R'
# )

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
  definition = textInput('electrode_text', 'Electrodes', value = "", placeholder = '1-5,8,11-20'),
  init_args = c('label', 'value'),
  init_expr = {
    last_input = cache_input('electrode_text', val = as.character(electrodes[1]))
    e = rutabaga::parse_svec(last_input)
    e = e[e %in% electrodes]
    if(!length(e)){
      e = electrodes[1]
    }
    value = rutabaga::deparse_svec(e)
    label = 'Electrodes (' %&% deparse_selections(electrodes) %&% ')'
  }
)

define_input(
  definition = customizedUI(inputId = 'input_customized')
)

define_input(
  definition = sliderInput('FREQUENCY', 'Frequency', min = 1, max = 200, value = c(1,200), step = 1, round = 1),
  init_args = c('min', 'max', 'value'),
  init_expr = {
    min = floor(min(frequencies))
    max = ceiling(max(frequencies))
    value = cache_input('FREQUENCY', c(min, max))
  }
)

define_input(
  definition = sliderInput('TIME_RANGE', 'Analysis', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
  init_args = c('min', 'max', 'value'),
  init_expr = {
    min = min(time_points)
    max = max(time_points)
    value = cache_input('TIME_RANGE', c(0, max(time_points)))
  }
)

define_input(
  definition = sliderInput('BASELINE', 'Baseline', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
  init_args = c('min', 'max', 'value'),
  init_expr = {
    min = min(time_points)
    max = max(time_points)
    value = cache_input('BASELINE', c(min(time_points), 0))
  }
)

define_input(
  definition = selectInput('combine_method', 'Electrode Transforms',
                           choices = c('none', 'z-score', 'max-scale', '0-1 scale', 'rank'), 
                           multiple = F, selected = 'none')
)


define_input(
  definition = selectInput('reference_type', 'Transform Reference',
                           choices = c('Trial', 'Trial type', 'Active trial types', 'All trials'), 
                           selected='Trial')
)

define_input(
  definition = selectInput('reference_group', 'Reference Group',
                           choices = c('Per Electrode', 'All Electrodes'), selected = 'Per Electrode')
)

define_input(
  definition = numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1)
)
define_input(
  definition = checkboxInput('log_scale', 'Log Freq')
)
define_input(
  definition = checkboxInput('sort_trials_by_type', 'Sort Trials')
)



define_input(
  definition = compoundInput(
    inputId = 'GROUPS', prefix= 'Group', inital_ncomp = 1, components = {
      textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
      selectInput('GROUP', ' ', choices = '', multiple = TRUE)
    }),
  init_args = c('initialize', 'value'),
  init_expr = {
    trials = preload_info$condition
    initialize = list(
      GROUP = list(
        choices = unique(trials)
      )
    )
    value = cache_input('GROUPS', list(
      list(
        GROUP = list(trials),
        GROUP_NAME = 'All Conditions'
      )
    ))
  }
)


# Define layouts if exists
input_layout = list(
  '[#cccccc]Electrodes' = list(
    c('electrode_text'),
    c('combine_method'),
    c('reference_type', 'reference_group')
  ),
  '[#99ccff]Trial Selector' = list(
    'GROUPS'
  ),
  'Analysis Settings' = list(
    'FREQUENCY',
    'BASELINE',
    'TIME_RANGE'
  ),
  '[-][#33aaff]Export Options' = list(),
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
  order = 4
)

define_output(
  definition = plotOutput(outputId = 'windowed_comparison_plot'),
  title = 'Collapse time+freq',
  width = 4,
  order = 3
)

define_output(
  definition = customizedUI('viewer_3d'),
  title = '3D Viewer',
  width = 12,
  order = 5
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
env$view_layout('power_explorer')


m = env$to_module(module_id = 'power_explorer')
