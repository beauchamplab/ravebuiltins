# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
  'inst/modules/power_explorer/exports.R',
  'inst/modules/power_explorer/event_handlers.R', 
  asis = TRUE
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

# define_input_multiple_electrodes(inputId = 'ELECTRODE_TEXT')
define_input_single_electrode(inputId = 'ELECTRODE')
define_input_frequency(inputId = 'FREQUENCY')
define_input_time(inputId = 'analysis_window', label='Analysis', initial_value = c(0,1))
define_input_time(inputId = 'baseline_window', label='Baseline', initial_value = c(-1,0))
define_input_condition_groups(inputId = 'GROUPS')

define_input(
  definition = selectInput('combine_method', 'Electrode Transforms',
                           choices = c('none', 'amplitude', 'z-score', 'max-scale', '0-1 scale', 'rank'),
                           multiple = F, selected = 'none')
)

define_input(
  definition = selectInput('trial_outliers_list', 'Trials to Exclude',
                           choices = NULL,
                           selected = NULL, multiple = TRUE),
  init_args = 'choices',
  init_expr = {
    choices = c(epoch_data$Trial)
  }
)

define_input(
  definition = actionButton('clear_outliers', 'Trials to Exclude', icon = icon('trash'))
)
define_input(
  definition = actionButton('save_new_epoch_file', 'Save Epoch File', icon =icon('file-export'))
)

define_input(
  definition = selectInput('show_outliers_on_plots', 'Show outliers on plots',
                            choices=c('Yes', 'No'), selected = 'Yes')
)



define_input(
  definition = numericInput('max_zlim', 'Heatmap Max (0 means data range)', value = 0, min = 0, step = 1)
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


# let people decide how much information to include in the plots. It's up to the individual plot to actually make
# use of this information, probably through shared decorators
define_input(
  definition = selectInput(inputId = 'PLOT_TITLE', label = 'Plot Decorations', multiple=TRUE,
                           choices =c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'),
                           selected=c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'))
)

define_input(
  definition = selectInput(inputId = 'plots_to_export', label='Plots to Export', multiple=TRUE,
                           choices = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average'),
                           selected = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average'))
)

define_input(
  definition = selectInput(inputId = 'export_what',
                           label='Which electrodes should be exported?', multiple=FALSE,
                           choices = c('All Loaded', 'Current Selection'))
)

define_input(
  definition = checkboxInput('draw_decorator_labels', "Label Plot Decorations", value=TRUE)
)

# define_input(
#   definition = selectInput(inputId = 'color_palette', label='Color palette', multiple=FALSE, 
#                            choices = list('Matlab'=get_palette(get_palette_names = TRUE),
#                                           'RAVE'=c('redish', 'bluish'),
#                                           'RColorBrewer'=c('redish', 'bluish'),),
#                            selected = get_palette(get_palette_names = TRUE)[1])
# )



# define_input(
#   definition = selectInput(inputId = 'heatmap_color_palette', label='Heatmap Colors', multiple=FALSE, 
#                            choice=get_heatmap_palette(get_palette_names = TRUE),
#                            selected = get_heatmap_palette(get_palette_names = TRUE)[1]),
#   
#   # cache the color palette across data reloads. needs init_args and init_expr
#   init_args = c('selected'),
#   init_expr = {
#     selected = cache_input('heatmap_color_palette', val = get_heatmap_palette(get_palette_names = TRUE)[1])
#   }
# )



define_input(
  definition = selectInput(inputId = 'color_palette', label='Color palette', multiple=FALSE, 
                           choice=get_palette(get_palette_names = TRUE),
                           selected = get_palette(get_palette_names = TRUE)[1]),
  
  # cache the color palette across data reloads. needs init_args and init_expr
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('color_palette', val = get_palette(get_palette_names = TRUE)[1])
  }
)

define_input(
  definition = selectInput(inputId = 'background_plot_color_hint', label = 'Background color', multiple=FALSE,
                           choices = c('White', 'Black', 'Gray'))
)

define_input(
  definition = checkboxInput('invert_colors_in_palette', "Invert Palette Colors", value=FALSE)
)

define_input(
  definition = checkboxInput('reverse_colors_in_palette', "Reverse Palette Order", value=FALSE)
)

define_input(
  definition = customizedUI('graph_export')
)


# Define layouts if exists
input_layout = list(
  '[#cccccc]Electrodes' = list(
    c('ELECTRODE'),
    c('combine_method')#,
    #c('reference_type', 'reference_group')
  ),
  #[#99ccff]
  'Trial Selector' = list(
    'GROUPS'
  ),
  'Analysis Settings' = list(
    'FREQUENCY',
    'baseline_window',
    'analysis_window'
  ),
  '[-]Plot Options' = list(
    c('PLOT_TITLE'),
    'draw_decorator_labels',
    c('color_palette', 'background_plot_color_hint',
    'invert_colors_in_palette', 'reverse_colors_in_palette'),
    c('max_zlim'),
    # 'heatmap_color_palette',
    #FIXME collapse_using_median should be in Analysis Settings???
    c('log_scale', 'sort_trials_by_type', 'collapse_using_median')
  ),
  '[-]Trial Outliers' = list(
    'trial_outliers_list',
    'show_outliers_on_plots',
    c('clear_outliers', 'save_new_epoch_file')
  ),
  #[#aaaaaa]
  '[-]Export Plots' = list(
    c('plots_to_export'),
    c('export_what'),
    c('graph_export')
  ),
  '[-]Export Data/Results' = list(
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
                          # click = clickOpts(shiny::NS('power_explorer')('by_trial_heat_map_click'), clip = FALSE)),
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
  definition = plotOutput('windowed_comparison_plot',
                          click = clickOpts(shiny::NS('power_explorer')('windowed_by_trial_click'), clip = FALSE)),
  title = 'Collapse freq+time',
  width = 4,
  order = 4
)

define_output(
  definition = customizedUI('click_output'),
  title = 'Click Information',
  width=12, order=2.5
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
  order = 1e3,
  width = 12,
  additional_ui = tagList(
    selectInput(ns('viewer_3d_type'), 'Which statistics', choices = c('b', 't', 'p'))#,
    #p(ns('blah'))
  )
)


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------




# -------------------------------- View layout ---------------------------------
quos = env$parse_components(module_id = 'power_explorer')

view_layout('power_explorer', launch.browser = T, sidebar_width = 3)
