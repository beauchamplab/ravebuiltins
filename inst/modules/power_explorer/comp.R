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
  # here we're limiting the meta data to the electrodes that are currently loaded
  # we can't export unloaded electrodes
  electrodes_csv = module_tools$get_meta('electrodes') %>% subset((.)$Electrode %in% electrodes)
  elec_labels <- unique(electrodes_csv$Label)
  
  # figure out if there are any outliers to prepopulate the outlier list
  outlier_list <- character(0)
  efile <- sprintf('%s/power_outliers_%s.csv', subject$dirs$meta_dir, preload_info$epoch_name)
  if(file.exists(efile)) {
    outlier_data <- read.csv(efile)
    if(any(outlier_data$PowerOutlier)) {
        outlier_list = epoch_data$Trial[outlier_data$PowerOutlier]
    }
  }
  
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

# Select from multiple choices,
define_input(
  definition = customizedUI(inputId = 'input_customized')
)

define_input_multiple_electrodes(inputId = 'ELECTRODE_TEXT')
# define_input_single_electrode(inputId = 'ELECTRODE')
define_input_frequency(inputId = 'FREQUENCY', initial_value = c(70,150))
define_input_time(inputId = 'ANALYSIS_WINDOW', label='Analysis', initial_value = c(0,1))
define_input_time(inputId = 'BASELINE_WINDOW', label='Baseline', initial_value = c(-1,0))
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
  init_args = c('choices', 'selected'),
  init_expr = {
    choices = c(epoch_data$Trial)
    selected = outlier_list
  }
)

define_input(
  definition = actionButton('clear_outliers', 'Clear Outliers', icon = icon('trash'))
)

define_input(
  definition = actionButton('save_new_epoch_file',
                            label=HTML("Save Outliers"),
                            icon =icon('file-export')),
  init_args = 'label',
  init_expr = {
    label = "<span style='color:#ddd'>Save Outliers</span>"
  })

define_input(
  definition = checkboxInput('show_outliers_on_plots', 'Show outliers on plots', value = TRUE))

define_input(
  definition = numericInput('max_zlim', 'Heatmap Max (0 means data range)', value = 0, min = 0, step = 1))

define_input(
  definition = checkboxInput('log_scale', 'Log Freq (NI)'))

define_input(
  definition = checkboxInput('sort_trials_by_type', 'Sort Trials'))

define_input(
    definition = checkboxInput('collapse_using_median', 'Collapse w/ Median (NI)'))


# let people decide how much information to include in the plots. It's up to the individual plot to actually make
# use of this information, probably through shared decorators
define_input(
  definition = selectInput(inputId = 'PLOT_TITLE', label = 'Plot Decorations', multiple=TRUE,
                           choices =c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'),
                           selected=c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'))
)

define_input_time('plot_time_range', label='Plot Time Range', initial_value = c(-1e5,1e5))

define_input(
  definition = selectInput(inputId = 'plots_to_export', label='Plots to download', multiple=TRUE,
                           choices = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average'),
                           selected = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average')))

define_input(
  definition = selectInput(inputId = 'export_what',
                           label='Which electrodes should be included?', multiple=FALSE,
                           choices = c('All Loaded', 'Current Selection')))

define_input(
  definition = checkboxInput('draw_decorator_labels', "Label Plot Decorations", value=TRUE))


#
# Analysis Export options
#
{
  define_input(
    definition = textInput('analysis_prefix', value = 'power_by_condition',
                           label = HTML('<br/>Export filename (no spaces)')))
  define_input(
    definition = checkboxInput('analysis_mask_export',value = FALSE,
                               label = 'Export Electrode Mask'))
  
  define_input(
    definition = checkboxInput('filter_3d_viewer', "Filter 3D Viewer Results (requires viewer reload)", value=FALSE))
  
  define_input(
    definition = selectInput('trial_type_filter', label=HTML('<br/>Trials to include in export file'), choices=NULL, selected=NULL, multiple =TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
      choices = unique(preload_info$condition)
      selected = unique(preload_info$condition)
    })
  
  define_input(
    definition = actionButtonStyled(inputId = 'synch_with_trial_selector',
                                    label='Synch with trial selector', icon = shiny::icon('refresh')))

  define_input(
    definition = selectInput('analysis_filter_variable', label='Anatomical Filter 1', choices=NULL, selected=NULL)
    , init_args = c('choices', 'selected'),
    init_expr = {
      choices = c('none', names(electrodes_csv))
      selected = ifelse('FreeSurferLabel' %in% names(electrodes_csv), 'FreeSurferLabel', 'Label')
    })
  
  define_input(
    definition = selectInput('analysis_filter_variable_2', label='Anatomical Filter 2', choices=NULL, selected=NULL)
    , init_args = c('choices', 'selected'),
    init_expr = {
      choices = c('none', names(electrodes_csv))
      selected = ifelse('Hemisphere' %in% names(electrodes_csv), 'Hemisphere', 'none')
    }
  )
  
    
  define_input(
    definition = selectInput('analysis_filter_elec', label = 'Values to include',
                             choices=NULL, selected = NULL, multiple = TRUE
    ),
    init_args = c('choices', 'selected'),
    init_expr = {
      choices =  unique(elec_labels)
      selected = unique(elec_labels)
    }
  )
  define_input(
    definition = selectInput('analysis_filter_elec_2', label = 'Values to include',
                             choices=NULL, selected = NULL, multiple = TRUE))
  
  # export based on stats
  
  # filter 1: p-value filter
  define_input(
    definition = selectInput('pval_filter', label = HTML('Functional Filters<br/>p-value'),
                             choices=c('p', 'FDR(p)', 'Bonf(p)'),
                             selected = 'FDR(p)', multiple = FALSE))
  define_input(
    definition = selectInput('pval_operator', label = '',
                             choices=c('<', '>', '<=', '>='), selected = '<', multiple = FALSE))
  define_input(
    definition = textInput('pval_operand', label = '', value = 0.01))
  
  # stat filter for t-value
  define_input(
    definition = selectInput('tval_filter', label = 't-value',
                             choices=c('t', '|t|'), selected = 't', multiple = FALSE))
  define_input(
    definition = selectInput('tval_operator', label = ' ',
                             choices=c('<', '>', '<=', '>='), selected = '>', multiple = FALSE))
  define_input(
    definition = textInput('tval_operand', label= ' '))
  
  # stat filter for mean
  # this filter should be dynamic based on the contents of the statistical output
  define_input(
    definition = selectInput('mean_filter', label = 'mean response',
                             choices=c('b0', 'abs(b0)'), selected = 'b0', multiple = FALSE))
  define_input(
    definition = selectInput('mean_operator', label = ' ',
                             choices=c('<', '>', '<=', '>='), selected = '>', multiple = FALSE))
  define_input(
    definition = textInput('mean_operand', label= ' '))
  
  define_input(
    definition = actionButtonStyled('export_data', label='Export data for group analysis', icon=shiny::icon('download'),
                                    type = 'primary'))
  
  define_input(
    definition = actionButtonStyled('select_good_electrodes',label='Visualize Active Electrodes',
                                    icon=shiny::icon('magic'), type = 'default'))
  
  define_input(
    definition = textInput('current_active_set', label='Electrodes passing all functional and anatomical filters (read-only)', value=''))
  
  
  # define_input(
  #   definition = actionButtonStyled('export_plots_and_data', label='Export Plots and Data', icon=shiny::icon('download'),
  #                                   type = 'primary', width = '50%', style='margin-left: 25%; margin-right:25%')
  # )
  
}


### COLOR PALETTE
{
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
                           choices = c('White', 'Black', 'Gray'), selected = 'White')
)

define_input(
  definition = checkboxInput('invert_colors_in_palette', "Inverse Palette Colors", value=FALSE)
)

define_input(
  definition = checkboxInput('reverse_colors_in_palette', "Reverse Palette Order", value=FALSE)
)

define_input(
  definition = customizedUI('graph_export')
)

}

#
# deterime which varibles only need to trigger a render, not an exectute
render_inputs <- c(
  'sort_trials_by_type', 'draw_decorator_labels', 'PLOT_TITLE', 'plots_to_export', 'show_outliers_on_plots', 'background_plot_color_hint',
  'invert_colors_in_palette', 'reverse_colors_in_palette', 'color_palette', 'max_zlim','plot_time_range',
  'tval_filter', 'pval_filter', 'mean_filter',
  'tval_operator', 'pval_operator', 'mean_operator', 
  'tval_operand', 'pval_operand', 'mean_operand', 'analysis_filter_elec_2', 'analysis_filter_elec',
  'analysis_filter_variable', 'analysis_filter_variable_2'
)

#
# determine which variables only need to be set, not triggering rendering nor executing
manual_inputs <- c(
  'graph_export', 'filter_3d_viewer', 'trial_type_filter', 'synch_with_trial_selector',
  'export_what', 'analysis_prefix', 'analysis_mask_export', 'export_data', 'current_active_set'
)

# Define layouts if exists
input_layout = list(
  #'[#cccccc]
  'Select electrodes for analysis' = list(
    c('ELECTRODE_TEXT')
    #, c('combine_method'),
    #c('reference_type', 'reference_group')
  ),
  #[#99ccff]
  'Select trials for analysis' = list(
    'GROUPS'
  ),
  'Set analysis options' = list(
    'FREQUENCY',
    'BASELINE_WINDOW',
    'ANALYSIS_WINDOW'
  ),
  '[-]Set plot options' = list(
    'plot_time_range',
    c('PLOT_TITLE'),
    'draw_decorator_labels',
    c('color_palette', 'background_plot_color_hint',
    'invert_colors_in_palette', 'reverse_colors_in_palette'),
    c('max_zlim'),
    # 'heatmap_color_palette',
    c('sort_trials_by_type')
    #FIXME collapse_using_median should be in Analysis Settings???
    # c('log_scale', , 'collapse_using_median')
  ),
  '[-]Manage trial outliers' = list(
    'show_outliers_on_plots',
    'trial_outliers_list',
    'clear_outliers', 'save_new_epoch_file'
  ),
  #[#aaaaaa]
  '[-]Download plots and underlying data' = list(
    c('plots_to_export'),
    c('export_what'),
    # 'export_plots_and_data'#
    c('graph_export')
  ),
    # 'filter_3d_viewer',
    # 'analysis_mask_export',
  '[-]Export data from all electrodes for group analysis' = list(
    c('pval_filter', 'pval_operator', 'pval_operand'),
    c('tval_filter', 'tval_operator', 'tval_operand'),
    c('mean_filter', 'mean_operator', 'mean_operand'),
    c('analysis_filter_variable', 'analysis_filter_elec'),
    c('analysis_filter_variable_2', 'analysis_filter_elec_2'),
    'current_active_set',
    'select_good_electrodes',
    'trial_type_filter', 'synch_with_trial_selector',
    'analysis_prefix',
    'export_data'
  )
)

# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
  definition = plotOutput(outputId = 'heat_map_plot'),
  title = 'Activity over time by frequency',
  width = 12,
  order = 1
)

define_output(
  definition = plotOutput('by_trial_heat_map'),
                          # click = clickOpts(shiny::NS('power_explorer')('by_trial_heat_map_click'), clip = FALSE)),
  title = 'Activity over time by trial',
  width = 12,
  order = 2
)

define_output(
  definition = plotOutput('by_electrode_heat_map'),
  title = 'Activity over time by electrode',
  width = 12,
  order = 2.5
)

define_output(
  definition = plotOutput('over_time_plot'),
  title = 'Activty over time by condition',
  width = 7,
  order = 3
)

define_output(
  definition = plotOutput('windowed_comparison_plot',
                          click = clickOpts(shiny::NS('power_explorer')('windowed_by_trial_click'), clip = FALSE),
                          dblclick = clickOpts(shiny::NS('power_explorer')('windowed_by_trial_dbl_click'), clip = FALSE)),
  title = 'By-trial windowed response (across electrodes)',
  width = 3,
  order = 4
)

define_output(
  definition = customizedUI('click_output'),
  title = 'Last Click',
  width=2, order=4.1
)

define_output(
  definition = plotOutput('across_electrodes_f_histogram'),
  title = 't-value across all loaded electrodes',
  width = 4,
  order = 5.1
)

define_output(
  definition = plotOutput('across_electrodes_beta_histogram'),
  title = 'mean response across all loaded electrodes',
  width = 4,
  order = 5.2
)

define_output(
  definition = plotOutput('across_electrodes_corrected_pvalue'),
  title = 'p-value across all loaded electrodes',
  width = 4,
  order = 5
)




# define_output(
#   definition = customizedUI('viewer_3d'),
#   title = '3D Viewer',
#   width = 12,
#   order = 5
# )
# 
define_output_3d_viewer(
  outputId = 'power_3d',
  message = 'Click here to reload viewer',
  title = 'Statistical results by electrode',
  height = '70vh',
  order = 1e4
)


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------




# -------------------------------- View layout ---------------------------------
quos = env$parse_components(module_id = 'power_explorer')

view_layout('power_explorer', launch.browser = T, sidebar_width = 3)
