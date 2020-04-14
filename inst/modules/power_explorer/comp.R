# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
mount_demo_subject()

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

#  ----------------------  Initializing Global variables -----------------------
load_scripts(
  'inst/modules/power_explorer/exports.R',
  'inst/modules/power_explorer/event_handlers.R', 
  asis = TRUE
)

# split up the UI into multiple files to make it easier to manage
# source(..., local=TRUE)

define_initialization({
  # assign('aaa', session, envir = globalenv())
  #
  # Make sure power (referenced) exists
  # with the following line, RAVE will pop up a dialogue if
  # power is not loaded and ask users to load data
  #
  rave_checks('power referenced')

  #
  # Get referenced power (Wavelet power)
  #
  power = module_tools$get_power(referenced = TRUE)  # ,units = rave_options('default_power_unit'))

  # Shared variables
  frequencies = preload_info$frequencies
  time_points = preload_info$time_points
  electrodes = preload_info$electrodes
  epoch_data = module_tools$get_meta('trials')
  
  
  epoch_event_types = str_subset(colnames(epoch_data), 'Event_*')
  if(length(epoch_event_types) > 0) {
    epoch_event_types %<>% str_remove_all('Event_')
  }
  epoch_event_types <- c("Trial Onset", epoch_event_types)
  
  # here we're limiting the meta data to the electrodes that are currently loaded
  # we can't export unloaded electrodes
  electrodes_csv = module_tools$get_meta('electrodes') %>% subset((.)$Electrode %in% electrodes)
  
  elec_filter <- 'Label'
  elec_labels <- unique(electrodes_csv$Label)
  
  # if there is a FreeSurferLabel column let's go with that, otherwise we'll fall back to the Label column
  column <- which(tolower(names(electrodes_csv)) == 'freesurferlabel')
  if(length(column) > 0) {
    elec_filter <- names(electrodes_csv)[column]
    elec_labels <- unique(electrodes_csv[[column]])
  }
  
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

# , label='Download copy of electrode meta data')
# )

define_input_multiple_electrodes(inputId = 'ELECTRODE_TEXT', label = 'Select electrodes by number')
# define_input_single_electrode(inputId = 'ELECTRODE')

# we also want to be able to select electrodes categorically
define_input(
  definition = selectInput('electrode_category_selector', label='Select electrodes by category', choices=NULL, selected=NULL),
  init_args = c('choices', 'selected'),
  init_expr = {
    choices = names(electrodes_csv)
    selected = elec_filter
  })

define_input(
  definition = selectInput('electrode_category_selector_choices', label = 'Electrodes to display',
                           choices=NULL, selected = NULL, multiple = TRUE
  ),
  init_args = c('choices', 'selected'),
  init_expr = {
    choices =  unique(elec_labels)
    selected = unique(elec_labels)
  }
)

define_input_condition_groups(inputId = 'GROUPS')
define_input_frequency(inputId = 'FREQUENCY', initial_value = c(70,150))
define_input_time(inputId = 'ANALYSIS_WINDOW', label='Analysis time (relative to analysis event)', initial_value = c(0,1))
define_input_time(inputId = 'plot_time_range', label='Plot Time Range (relative to analysis event)')
define_input_time(inputId = 'BASELINE_WINDOW', label='Baseline time (relative to trial onset)', initial_value = c(-1,0))

define_input(definition = selectInput('event_of_interest', 'Analysis Event',
                                      choices=c('Trial Onset'), selected=c('Trial Onset'), multiple = FALSE),
             init_args = c('choices'),
             init_expr = {
               choices = epoch_event_types
             }
)


# define_input_analysis_file_chooser('analysis_settings', read_source = c('Analysis Settings' = 'analysis_yamls'))
define_input_analysis_yaml_chooser(
  'analysis_settings', name_prefix = 'power_explorer_settings_', 
  # Relative to project directory
  read_path = 'power_explorer/settings',
  labels = c('Save settings', 'Load settings')
)

define_input(
  definition = selectInput('unit_of_analysis', 'Electrode unit of analysis',
                           choices=NULL, selected=NULL, multiple=FALSE),
  init_args = c('choices', 'selected'),
  init_expr = {
    choices = names(get_unit_of_analysis())
    selected = names(get_unit_of_analysis())[[1]]
  })

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
  definition = checkboxInput('global_baseline', 'Use Global Baseline (across trials)', value = FALSE)
)

define_input(
  definition = actionButton('clear_outliers', 'Clear Outliers', icon = icon('trash'))
)

define_input(
  customizedUI('sheth_special')
)

define_input(
  customizedUI('custom_plot_download')
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
  definition = numericInput('max_zlim', 'Heat map range (0: data range)', value = 99, min = 0, step = 1))

define_input(
  definition = checkboxInput('percentile_range', 'Range is percentile', value=TRUE))

define_input(
  definition = checkboxInput('show_heatmap_range', 'Show data range on heat maps', value=TRUE))


define_input(
  definition = checkboxInput('synch_3d_viewer_bg',
                             'Override 3d viewer background', value=FALSE))

# define_input(
#   definition = checkboxInput('log_scale', 'Log Freq (NI)'))

define_input(
  definition = selectInput('sort_trials_by_type', 'How should trials be sorted?',
                           choices = NULL, selected = NULL), init_args = c('choices', 'selected'), init_expr = {
                             choices = c('Trial Number', 'Condition', epoch_event_types[-1])
                             selected = 'Trial Number'
                           })

# define_input(
#     definition = checkboxInput('collapse_using_median', 'Collapse w/ Median (NI)'))


# let people decide how much information to include in the plots. It's up to the individual plot to actually make
# use of this information, probably through shared decorators
define_input(
  definition = selectInput(inputId = 'plot_title_options', label = 'Plot Decorations', multiple=TRUE,
                           choices =c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'),
                           selected=c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'))
)

define_input(
  definition = selectInput(inputId = 'plots_to_export', label='Plots to download', multiple=TRUE,
                           choices = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average', 'Over Time by Electrode'),
                           selected = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average', 'Over Time by Electrode'))
  )

define_input(
  definition = selectInput(inputId = 'export_what',
                           label='Which electrodes should be included?', multiple=FALSE,
                           choices = c('All Loaded', 'Current Selection')))


define_input(
  definition = selectInput(inputId = 'movie_export_trials',
                           label='How should power over time be exported?', multiple=FALSE,
                           choices = c('Per trial type', 'Averaged over trials within a condition'),
                           selected = c('Per trial type'))
)

define_input(
  definition = checkboxInput('draw_decorator_labels', "Label Plot Decorations", value=TRUE))


#
# Analysis Export options
#
{
  define_input(
    definition = textInput('analysis_prefix', value = 'power_by_condition',
                           label = HTML('<br/>Export filename (no spaces)')),
    init_args = 'value',
    init_expr = {
      value = sprintf('%s_pow_by_cond', subject$subject_code)
    })
  # define_input(
  #   definition = checkboxInput('analysis_mask_export',value = FALSE,
  #                              label = 'Export Electrode Mask'))
  
  # define_input_time(inputId = 'export_time_window', label='Export time window')
  
  define_input(
    definition = checkboxInput('include_outliers_in_export', "Include outliers in export", value=FALSE)
  )
  
    define_input(
    definition = selectInput('trial_type_filter', label=HTML('<br/>Trials to include in export file'), choices=NULL, selected=NULL, multiple =TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
      choices = unique(preload_info$condition)
      selected = unique(preload_info$condition)
    })
  
  define_input(
    definition = actionButtonStyled(inputId = 'synch_with_trial_selector',
                                    label='Synch from trial selector', icon = shiny::icon('refresh')))

  define_input(
    definition = selectInput('analysis_filter_variable', label='Anatomical Filter 1', choices=NULL, selected=NULL)
    , init_args = c('choices', 'selected'),
    init_expr = {
      choices = c('none', names(electrodes_csv))
      selected = 'none'
    })
  
  define_input(
    definition = selectInput('analysis_filter_variable_2', label='Anatomical Filter 2', choices=NULL, selected=NULL)
    , init_args = c('choices', 'selected'),
    init_expr = {
      choices = c('none', names(electrodes_csv))
      selected = 'none'
    }
  )
    
  define_input(
    definition = selectInput('analysis_filter_elec', label = 'Values to include',
                             choices=NULL, selected = NULL, multiple = TRUE
    )
  )
  define_input(
    definition = selectInput('analysis_filter_elec_2', label = 'Values to include',
                             choices=NULL, selected = NULL, multiple = TRUE))
  
  # export based on stats
  
  # filter 1: p-value filter
  define_input(
    definition = selectInput('p_filter', label = HTML('Functional Filters<br/>p-value'),
                             choices=c('p', 'FDR(p)', 'Bonf(p)'),
                             selected = 'FDR(p)', multiple = FALSE))
  define_input(
    definition = selectInput('p_operator', label = '',
                             choices=c('<', '>', '<=', '>='), selected = '<', multiple = FALSE))
  define_input(
    definition = textInput('p_operand', label = '', value = 0.01))
  
  # stat filter for t-value
  define_input(
    definition = selectInput('t_filter', label = 't-value',
                             choices=c('t', 'abs(t)'), selected = 't', multiple = FALSE))
  define_input(
    definition = selectInput('t_operator', label = ' ',
                             choices=c('<', '>', '<=', '>='), selected = '>', multiple = FALSE))
  define_input(
    definition = textInput('t_operand', label= ' '))
  
  # stat filter for mean
  # this filter should be dynamic based on the contents of the statistical output
  define_input(
    definition = selectInput('mean_filter', label = 'mean response',
                             choices=c('mean', 'abs(mean)'), selected = 'mean', multiple = FALSE))
  define_input(
    definition = selectInput('mean_operator', label = ' ',
                             choices=c('<', '>', '<=', '>='), selected = '>', multiple = FALSE))
  define_input(
    definition = textInput('mean_operand', label= ' '))
  
  define_input(
    definition = customizedUI('write_out_data_ui')
  )
  define_input(
    definition = checkboxInput('export_also_download', 'Also download data', value = FALSE)
  )
  # define_input(
  #   definition = actionButtonStyled('export_data', label='Export data for group analysis', icon=shiny::icon('download'),
  #                                   type = 'primary'))
  
  define_input(
    definition = actionButtonStyled('select_good_electrodes',label='Visualize Active Electrodes',
                                    icon=shiny::icon('magic'), type = 'default'))
  
  define_input(
    definition = textInput('current_active_set',
                           label='Electrodes passing all functional and anatomical filters (read-only)', value=''))
  
  
  # define_input(
  #   definition = actionButtonStyled('export_plots_and_data', label='Export Plots and Data', icon=shiny::icon('download'),
  #                                   type = 'primary', width = '50%', style='margin-left: 25%; margin-right:25%')
  # )
  
}


### COLOR PALETTE
{
define_input(
  definition = selectInput(inputId = 'color_palette', label='Lines color palette', multiple=FALSE, 
                           choice=get_palette(get_palette_names = TRUE),
                           selected = get_palette(get_palette_names = TRUE)[1]),
  
  # cache the color palette across data reloads. needs init_args and init_expr
  init_args = c('selected'),
  init_expr = {
    selected = cache_input('color_palette', val = get_palette(get_palette_names = TRUE)[1])
    print(selected)
  }
)
  define_input(
    definition = checkboxInput('invert_colors_in_palette', "Invert Colors", value=FALSE)
  )
  
  define_input(
    definition = checkboxInput('reverse_colors_in_palette', "Reverse Palette", value=FALSE)
  )
  
  define_input(
    definition = selectInput(inputId = 'heatmap_color_palette', label='Heatmap color palette', multiple=FALSE, 
                             choices = get_heatmap_palette(get_palette_names = TRUE),
                             selected = get_heatmap_palette(get_palette_names = TRUE)[1]),
    
    # cache the color palette across data reloads. needs init_args and init_expr
    init_args = c('selected'),
    init_expr = {
      selected = cache_input('heatmap_color_palette', val = get_heatmap_palette(get_palette_names = TRUE)[1])
    }
  )
  
  define_input(
    definition = selectInput(inputId = 'viewer_color_palette', label='3dViewer color palette', multiple=FALSE, 
                             choices = c('Synch with heatmaps', get_heatmap_palette(get_palette_names = TRUE)),
                             selected = 'Synch with heatmaps'),
    
    # cache the color palette across data reloads. needs init_args and init_expr
    init_args = c('selected'),
    init_expr = {
      selected = cache_input('3dviewer_heatmap_color_palette', val = 'Synch with heatmaps')
    }
  )
  
  
  define_input(
    definition = numericInput(inputId = 'heatmap_number_color_values', label='Unique Color Values',
                              value = 101, min=2, max=1001)
  )
  
  define_input(
    definition = checkboxInput('invert_colors_in_heatmap_palette', "Invert Colors", value=FALSE)
  )
  
  define_input(
    definition = checkboxInput('reverse_colors_in_heatmap_palette', "Reverse Palette", value=FALSE)
  )
  
  
  
  # 
  # define_input(
  #   definition = numericInput(inputId = 'heatmap_truncate_less_than', label='Heat map min (0: data range)', min=0)
  # )
  
  # define_input(
  #   definition = numericInput('max_zlim', 'Heat map max (0: data range)', value = 0, min = 0, step = 1))
  # 
  # 
  # define_input(
  #   definition = numericInput('heatmap_truncate_greater_than', 'Heat map max (0: data range)', value = 0, min = 0, step = 1))
  # 

define_input(
  definition = selectInput(inputId = 'background_plot_color_hint', label = 'Plot background color', multiple=FALSE,
                           choices = c('white', 'black', 'gray'), selected = 'white')
)

define_input(
  definition = checkboxInput(inputId = 'show_result_densities', label='Show frequency plots beside results output', value=TRUE)
)

define_input(
  definition = selectInput(inputId = 'which_result_to_show_on_electrodes', label = 'Across electrodes results to show', multiple=FALSE,
                           choices = c('Omnibus Activity (across all active trial types)'), selected = 'Omnibus Activity (across all active trial types)')
)


#TODO think about the right way to do this...
# define_input(
#   definition = checkboxInput(inputId = 'synch_to_3dviewer',
#                              label = 'Synch display variable to 3d Viewer', value=TRUE)
# )

define_input(
  definition = customizedUI('download_all_graphs')
)

}

define_input(
  definition = customizedUI('download_electrodes_csv')
)


#
# deterime which varibles only need to trigger a render, not an exectute
render_inputs <- c(
  'which_result_to_show_on_electrodes', 'synch_to_3dviewer',
  'sort_trials_by_type', 'draw_decorator_labels', 'plot_title_options', 'show_outliers_on_plots', 'background_plot_color_hint',
  'invert_colors_in_palette', 'reverse_colors_in_palette', 'color_palette', 'heatmap_color_palette', 'heatmap_number_color_values',
  'max_zlim','plot_time_range', 'invert_colors_in_heatmap_palette', 'reverse_colors_in_heatmap_palette', 'percentile_range',
  # 'heatmap_truncate_less_than',
  't_filter', 'p_filter', 'mean_filter', 'show_result_densities',
  't_operator', 'p_operator', 'mean_operator', 
  't_operand', 'p_operand', 'mean_operand', 'analysis_filter_elec_2', 'analysis_filter_elec',
  'analysis_filter_variable', 'analysis_filter_variable_2', 'show_heatmap_range'
)


define_input_auto_recalculate(
  inputId = 'auto_calculate', label = 'Automatically recalculate analysis', 
  type = 'checkbox', default_on = FALSE
)

define_input_auto_recalculate(
  inputId = 'do_calculate_btn', label = 'Recalculate everything', 
  type = 'button', button_type = 'primary'
)


# this is hard because we need to figure out which pieces of data are need for quick calculation vs. full calculation
# define_input_auto_recalculate(
#   inputId = 'do_quick_calculate_btn', label = 'Recalculate across-electrode stats only', 
#   type = 'button', button_type = 'success'
# )




#
# determine which variables only need to be set, not triggering rendering nor executing
manual_inputs <- c(
  'synch_3d_viewer_bg', 'viewer_color_palette', 'graph_export', 'trial_type_filter', 'synch_with_trial_selector', 'download_electrodes_csv', 'movie_export_trials', 'plots_to_export',
  'btn_save_analysis_settings', 'btn_load_analysis_settings', 'include_outliers_in_export',
  'export_what', 'analysis_prefix', 'export_data', 'current_active_set', 'export_also_download', 'export_time_window', 'sheth_special'
)

# Define layouts if exists
input_layout = list(
  # '[#cccccc]
  'Configure analysis' = list(
    c('electrode_category_selector', 'electrode_category_selector_choices'),
    'ELECTRODE_TEXT',
    'download_electrodes_csv',
    'FREQUENCY',
     c('unit_of_analysis'),
    'BASELINE_WINDOW',
    'global_baseline',
    'ANALYSIS_WINDOW',
    'plot_time_range',
    c('event_of_interest', 'sort_trials_by_type'),
    'do_calculate_btn', 'auto_calculate', 'do_quick_calculate_btn'
  ),
  '[-]Create condition contrasts' = list(
    'GROUPS',
    'analysis_settings'
  ),
  '[-]Find + Export active electrodes' = list(
    c('show_result_densities'),
    c('which_result_to_show_on_electrodes'), 
    c('synch_to_3dviewer'),
    c('p_filter', 'p_operator', 'p_operand'),
    c('t_filter', 't_operator', 't_operand'),
    c('mean_filter', 'mean_operator', 'mean_operand'),
    c('analysis_filter_variable', 'analysis_filter_elec'),
    c('analysis_filter_variable_2', 'analysis_filter_elec_2'),
    'current_active_set',
    'select_good_electrodes',
    'trial_type_filter', 'synch_with_trial_selector',
    'export_time_window',
    'include_outliers_in_export',
    'analysis_prefix',
    # 'export_data'
    'write_out_data_ui',
    'export_also_download'
  ),
  '[-]Configure plots' = list(
    c('plot_title_options'),
    'draw_decorator_labels',
    c('color_palette',
    'reverse_colors_in_palette', 'invert_colors_in_palette'),
    c('heatmap_color_palette', 'heatmap_number_color_values',
      'reverse_colors_in_heatmap_palette', 'invert_colors_in_heatmap_palette'),
      c('max_zlim','percentile_range'),
        # 'heatmap_truncate_less_than',
        'show_heatmap_range',
    c('viewer_color_palette'),
    c('background_plot_color_hint', 'synch_3d_viewer_bg')
    #FIXME collapse_using_median should be in Analysis Settings???
    # c('log_scale', , 'collapse_using_median')
  ),
  #[#aaaaaa]
  '[-]Download plots + underlying data' = list(
    c('plots_to_export'),
    c('export_what'),
    c('movie_export_trials'),
    # 'export_plots_and_data'#
    c('download_all_graphs')
  ),'[-]Manage trial outliers' = list(
    'show_outliers_on_plots',
    'trial_outliers_list',
    'clear_outliers', 'save_new_epoch_file'
  ), '[-]Download hi-res single figure' = list(
    'custom_plot_download'
  ), '[-]Download stat heatmaps' = list(
    'sheth_special'
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
  definition = plotOutput('by_trial_heat_map_plot'),
                          # click = clickOpts(shiny::NS('power_explorer')('by_trial_heat_map_click'), clip = FALSE)),
  title = 'Activity over time by trial',
  width = 12,
  order = 2
)

define_output(
  definition = plotOutput('by_electrode_heat_map_plot'),
  title = 'Activity over time by electrode',
  width = 12,
  order = -1
)

define_output(
  definition = plotOutput('over_time_plot'),
  title = 'Activty over time by condition',
  width = 7,
  order = 3
)

define_output(
  definition = plotOutput('windowed_comparison_plot',
                          click = clickOpts(shiny::NS('power_explorer')('windowed_by_trial_click'), clip = T),
                          dblclick = clickOpts(shiny::NS('power_explorer')('windowed_by_trial_dbl_click'), clip = T)),
  title = 'Per trial, averaged across electrodes',
  width = 3,
  order = 4
)

define_output(
  definition = customizedUI('click_output'),
  title = 'Click Info',
  width=2, order=4.1
)


# define_output(
#   definition = plotOutput('across_electrodes_f_histogram'),
#   title = 't-value across all loaded electrodes',
#   width = 4,
#   order = 5.1
# )
# 
# define_output(
#   definition = plotOutput('across_electrodes_beta_histogram'),
#   title = 'mean response across all loaded electrodes',
#   width = 4,
#   order = 5.2
# )
# 
# define_output(
#   definition = plotOutput('across_electrodes_corrected_pvalue'),
#   title = 'p-value across all loaded electrodes',
#   width = 4,
#   order = 5
# )

define_output(
  definition = plotOutput('across_electrode_statistics_plot'),
  title = 'Per electrode statistical tests [Filled circles pass all filters, open circles do not]',
  width = 12,
  order = -1#,
  # alt_text = 'This does...'
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
  title = 'Results on surface',
  height = '50vh',
  order = -1e4
)


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------




# -------------------------------- View layout ---------------------------------
quos = rave:::parse_components(module_id = 'power_explorer', parse_context = 'rave_running_local')

view_layout('power_explorer')
