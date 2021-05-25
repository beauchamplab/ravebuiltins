# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

dev_ravebuiltins(T)

## Load subject for debugging
mount_demo_subject()

module_id <- 'group_analysis_lme'

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

#  ----------------------  Initializing Global variables -----------------------
load_scripts(
    get_path('inst/modules/group_analysis_lme/reactives.R'),
    get_path('inst/modules/group_analysis_lme/common.R'), 
    get_path('inst/modules/group_analysis_lme/outputs.R'), 
    asis = TRUE
)

define_initialization({
    project_name = subject$project_name
    project_dir = dirname(subject$dirs$subject_dir)
    subjects = get_subjects(project_name)
    
    power_explorer_dir = file.path(project_dir, '_project_data', 'power_explorer')
    group_analysis_src = file.path(project_dir, '_project_data', 'group_analysis_lme', 'source')
    
    rescan_source = function(update = TRUE, new_selected = NULL){
        choices = c(
            # list.files(power_explorer_dir, pattern = '\\.[cC][sS][vV]$'),
            # list.files(group_analysis_src, pattern = '\\.[cC][sS][vV]$')
          list.files(power_explorer_dir, pattern = '\\.(fst|csv)$'),
          list.files(group_analysis_src, pattern = '\\.(fst|csv)$')
        )
        # Order file names by date-time (descending order)
        dt = stringr::str_extract(choices, '[0-9]{8}-[0-9]{6}')
        od = order(strptime(dt, '%Y%m%d-%H%M%S'), decreasing = TRUE)
        choices = choices[od]
        if(update && is_reactive_context()){
            selected = c(source_files, new_selected)
            updateSelectInput(session, 'source_files', choices = choices, selected = selected)
        }
        return(choices)
    }
    
})

#  ---------------------------------  Inputs -----------------------------------
define_input_analysis_data_csv(
    inputId= 'analysis_data', label = "Click in this box to access available datasets", 
    paths = c('_project_data/group_analysis_lme/source', '_project_data/power_explorer/exports'),
    reactive_target = 'local_data$analysis_data_raw', try_load_yaml = TRUE
)

# define_input(
#     customizedUI('var_sel')
# )
# define_input_table_filters('var_sel', label = 'Filter', watch_target = 'local_data$analysis_data',
#                            reactive_target = 'local_data[["analysis_data_filtered"]]')

data_dir = rave_options('data_dir'); names(data_dir) = NULL
# define_input_analysis_file_chooser(
#   'lmer_yaml', labels = c('Save settings', 'Load settings'),
#   name_prefix = 'lmer_settings_', 
#   read_source = c('Analysis Settings' = '', 'RAVE Home' = '../..'),
#   write_source = 'group_analysis_lme',
#   default_path = 'power_explorer'
# )

load_scripts(rlang::quo({
  observeEvent(input$lmer_yaml_load, {
    fdata = input$lmer_yaml_load
    if(!is.list(fdata) || !length(fdata$files)){ return() }
    assign('fdata', fdata, envir = globalenv())
    f_name = unlist(fdata$files); names(f_name) = NULL
    
    read_source = list(
      'Power Explorer Analysis' = 'power_explorer',
      'LME Analysis' = 'group_analysis_lme'
    )
    
    f_name = c(subject$dirs$subject_dir, '..', '_project_data', read_source[[fdata$root]], f_name)
    f_path = do.call(file.path, as.list(f_name))
    print(f_path)
    conf = yaml::read_yaml(f_path)
    print(conf)
  })
}))



define_input(definition = checkboxInput('omnibus_plots_use_common_range',
                                        label='Use common scale for ROI subplots', value=TRUE)
)

define_input(definition = checkboxInput('omnibus_plots_roi_as_lattice',
                                        label='Trellis-style ROI plot', value=FALSE)
)



define_input(definition = selectInput('omnibus_plots_color_palette', label="ConditionGroup color palette",
                                      choices = get_palette(get_palette_names = TRUE),
                                      selected = 'Beautiful Field'))

define_input(selectInput('omnibus_plots_legend_location', label='Legend Location',
                         choices = c('topleft', 'top', 'topright', 'left', 'center', 'right', "bottomleft", "bottom", "bottomright"),
                         selected='top')
)

define_input(selectInput('omnibus_plots_plot_aesthetics', label='Plot aesthetic',
                         choices = c('border', 'jittered points', 'points', 'filled', 'connect means', 'show means', 'connect points'), multiple = TRUE,
                         selected=c('border', 'jittered points'))
)

define_input(
  sliderInput('omnibus_plots_time_range', label = 'Plot time window', min = 0, max = 1, value = 0:1, step = 0.01, round = -2)
)

define_input(
    selectInput('model_dependent', 'Dependent variable', choices = '', selected = character(0))
)

define_input(
  selectInput('model_roi_variable', 'ROI variable', choices = '', selected = character(0), multiple = FALSE)
)

define_input(
  selectInput('filter_by_roi', 'Regions included (add/remove to filter in/out)', choices = '',
              selected = character(0), multiple = TRUE)
)

define_input(
  checkboxInput('roi_ignore_hemisphere', 'Collapse L/R hemisphere', value =FALSE)
)

define_input(
  checkboxInput('roi_ignore_gyrus_sulcus', 'Collapse gyrus/sulcus', value = FALSE)
)


define_input(
  selectInput('how_to_model_roi', 'ROI analysis type',
              choices = c('Stratify (Random+Fixed)',  'All possible ITX (Random+Fixed)',  'Random effect only', 'Average electrodes w/n ROI', 'Filter Only'),
              selected = 'Filter Only', multiple = FALSE)
)

define_input(
    selectInput('model_fixed_effects', 'Fixed effects (select 0, 1, or more)',
                choices = '', selected = character(0), multiple = TRUE)
)
define_input(
    selectInput('model_random_effects', 'Random effects (Subject/Electrode required for multi-subject)',
                choices = '', selected = character(0), multiple = TRUE)
)

define_input(
    textAreaInput('model_formula', 'Formula', placeholder = 'y ~ X1 + X2 + (1|Subject/Electrode)')
)

define_input(
    actionButtonStyled('run_analysis', 'Run Analysis', type = 'primary', width = '100%')
)

define_input(definition = customizedUI('download_all_results'))


define_input(definition = customizedUI('hide_everything_but_post_hoc_plot'))

define_input(definition = selectInput('post_hoc_plot_xvar', label = 'X variable', multiple = FALSE, choices=NULL))
define_input(definition = textInput('post_hoc_plot_xvar_custom', label = 'Custom X variable', placeholder = 'm(Group_A) - m(Group_B)'))
define_input(definition = textInput('post_hoc_plot_xlim', label='X range', value = ''))


define_input(definition = selectInput('post_hoc_plot_yvar', label = 'Y variable', multiple = FALSE, choices=NULL))
define_input(definition = textInput('post_hoc_plot_yvar_custom', label = 'Custom Y variable', placeholder = 'm(Group_C) - m(Group_D)'))
define_input(definition = textInput('post_hoc_plot_ylim', label='Y range', value = ''))


define_input(definition = selectInput('post_hoc_plot_zvar', label = 'Partial/Regress out Z', multiple = FALSE, choices=c('None')))
define_input(definition = textInput('post_hoc_plot_zvar_custom', label = 'Custom Z variable', placeholder = 'm(Group_E)'))


define_input(definition = selectInput('post_hoc_plot_highlight_subject', label="Highlight subjects' data", multiple=TRUE,
                                      choices = c('Separate Colors', 'Separate Plots'), selected = 'Separate Colors'))

define_input(definition = selectInput('post_hoc_plot_highlight_subject_color_palette', label="Subject color palette",
                                      choices = get_palette(get_palette_names = TRUE),
                                      selected = 'Set1'))

define_input(definition = checkboxInput('post_hoc_plot_regression_line', label = 'Add regression line', value=FALSE))
define_input(definition = checkboxInput('post_hoc_plot_equality_line', label = 'Add line of equality', value=FALSE))

define_input(
  selectInput('post_hoc_plot_show_stats', label = 'Show stats on graph', multiple = TRUE,
              choices = c('Correlation (Pearson)', 'R2', 'Correlation (Spearman)', 'Difference test (t)', 'Difference test (Wilcoxon)'))
)

define_input(selectInput('post_hoc_plot_legend_location', label='Legend Location',
                         choices = c('topleft', 'top', 'topright', 'left', 'center', 'right', "bottomleft", "bottom", "bottomright"),
                         selected='topleft'
))

define_input(definition = selectInput('post_hoc_plot_vertical_reference_line',
                                      label = 'Vertical reference', choices=c('None', '0', 'Mean', '0%', '25%', '50%', '75%', '100%')))

define_input(definition = selectInput('post_hoc_plot_horizontal_reference_line',
                                      label = 'Horizontal reference', choices=c('None', '0', 'Mean', '0%', '25%', '50%', '75%', '100%')))

define_input(definition = textInput('post_hoc_plot_vertical_reference_line_custom', label = 'Custom Vertical',
                                    placeholder = '0, 2:3'))

define_input(definition = textInput('post_hoc_plot_horizontal_reference_line_custom', label = 'Custom Horizontal',
                                    placeholder = '0.05, 0.01'))


define_input(numericInput('post_hoc_plot_width_hint', label='Plot Width', value = 17, min = 1, max=85, step=0.5))


define_input(numericInput('post_hoc_plot_column_count', label='# Columns', value = 4, min = 1, max=10, step=1))


define_input_condition_groups(
  inputId = 'cond_group', label = 'Condition Group', initial_groups = 2,
  max_group = 20, min_group = 2, label_color = 'grey40', 
  init_args = NULL, init_expr = NULL
)

define_input(
  definition = sliderInput('analysis_window', 'Analysis Window',
                           min = 0, max = 1, value=c(0,1), round=-2, step=0.01)
)
define_input(definition = checkboxInput('single_analysis_window', 'Active', value=TRUE))


# define_input(customizedUI('repeated_measures_note'))
define_input(
  definition = dipsaus::compoundInput2(
    inputId = 'multi_window_analysis', label = 'Time Window', inital_ncomp = 2, min_ncomp = 2, max_ncomp = 5,
    label_color = 'grey40', 
    components = htmltools::div(
      textInput('window_name', 'Name', value = '', placeholder = 'window name'),
      sliderInput('analysis_window', ' ', value =0:1, post = 's', min=-2, max=2, step = 0.01),
      # checkboxInput('window_is_active', 'Active', value = FALSE)
    )
  )
)

define_input(
  definition = selectInput('how_to_model_multi_window', label = 'Time Window analysis type', 
                           choices = c('Stratify',  'All possible ITX'),
                           selected = 'Stratify')
)

# figure export
define_input(
  customizedUI('custom_plot_download')
)


define_input(definition = checkboxInput('multi_window_is_active', 'Active', value=FALSE))


#Download per electrode stats
define_input(
  definition = selectInput('pes_scaling_type', label = 'Data scaling', 
                           choices = c('No scaling',
                                       'z per ROI (across condition and electrode)',
                                       'z per electrode (across condition)',
                                       'z per condition (across electrode)'),
                           selected = 'No scaling')
)

define_input(
  definition = selectInput('pes_separate_colorbar', label = 'Color bar scaling (means and contrasts are always separate)', 
                           choices = c('Global',
                                       'Per ROI',
                                       'Per electrode',
                                       'Per condition'),
                           selected = 'Global')
)


define_input(
  definition = selectInput('pes_means_heatmap_palette', label = 'Means palette', 
                           choices = get_heatmap_palette(get_palette_names = TRUE),
                           selected = 'BlueWhiteRed')
)
define_input(
  definition = selectInput('pes_contrasts_heatmap_palette', label = 'Contrasts palette', 
                           choices = get_heatmap_palette(get_palette_names = TRUE),
                           selected = 'PurpleWhiteGreen')
)


define_input(
  definition = checkboxInput('pes_group_by_roi', label = 'Group electrodes by ROI', value = TRUE)
)

define_input(
  definition = selectInput('pes_sort_by_metric', label = 'How should electrodes be sorted?',
                           choices = c('Clustering', 'Lexical', 'Anatomical'), selected='Lexical')
)

define_input(
  definition = numericInput('pes_max_for_means', 'Plot max for condition means (0: data range)',
                            min=0, max=1e3, step = 1, value = 99)
)

define_input(
  definition = checkboxInput('pes_max_for_means_is_percentile', 'Max is %',
                             value = TRUE))

define_input(
  definition = numericInput('pes_max_for_contrasts', 'Plot max for condition contrasts (0: data range)',
                            min=0, max=1e3, step = 1, value = 99)
)
define_input(
  definition = checkboxInput('pes_max_for_contrasts_is_percentile', 'Max is %',
                             value = TRUE))

define_input(definition = customizedUI('download_pes'))


manual_inputs = c('source_files', 'csv_file', 'load_csvs', 'analysis_window', 'single_analysis_window',
                  'post_hoc_plot_xlim', 'post_hoc_plot_ylim', 'multi_window_is_active', 'model_dependent',
                  'model_fixed_effects', 'model_random_effects', 'model_splinetime',
                  'model_formula', 'model_embedsubject', 'run_analysis','download_all_results',
                  'model_roi_variable', 'filter_by_roi', 'how_to_model_roi', 'roi_ignore_hemisphere', 'roi_ignore_gyrus_sulcus',
                  
                  'pes_scaling_type', 'pes_group_by_roi', 'pes_sort_by_metric', 'pes_separate_colorbar',
                  'pes_max_for_means', 'pes_max_for_means_is_percentile',
                  'pes_max_for_contrasts', 'pes_max_for_contrasts_is_percentile', 'pes_contrasts_heatmap_palette', 'pes_means_heatmap_palette')

input_layout = list(
    'Data import' = list(
        'analysis_data'
    ),
    'Build condition groups' = list(
      'cond_group',
      'cond_group_ui'
      ),
    'Single time window analysis' = list(
      'analysis_window',
      'single_analysis_window'
    ),
    '[-]Multiple time window analysis' = list(
      'multi_window_is_active', 
      'multi_window_analysis'
    ),
    'Build model' = list(
        c('model_dependent'),
        c('model_roi_variable'),
        c('how_to_model_roi'),
        c('filter_by_roi'),
        c('roi_ignore_hemisphere', 'roi_ignore_gyrus_sulcus'),
        c('model_fixed_effects'),
        c('model_random_effects'),
        'model_formula',
        'run_analysis',
        'download_all_results'
    ),
    '[-]Configure group plots' = list(
      'omnibus_plots_time_range',
      'omnibus_plots_plot_aesthetics',
      'omnibus_plots_color_palette',
      'omnibus_plots_legend_location',
      c('omnibus_plots_use_common_range',
        'omnibus_plots_roi_as_lattice')
    ),
    '[-]Plot post-hoc variables' = list(
      'hide_everything_but_post_hoc_plot',
      'post_hoc_plot_xvar',
      'post_hoc_plot_xvar_custom',
      'post_hoc_plot_yvar',
      'post_hoc_plot_yvar_custom',
      'post_hoc_plot_zvar',
      'post_hoc_plot_zvar_custom'
      ),
    '[-]Configure post-hoc plot' = list(
      c('post_hoc_plot_xlim', 'post_hoc_plot_ylim', 'post_hoc_plot_width_hint', 'post_hoc_plot_column_count'),
      'post_hoc_plot_highlight_subject',
      'post_hoc_plot_highlight_subject_color_palette',
      c('post_hoc_plot_vertical_reference_line', 'post_hoc_plot_horizontal_reference_line',
        'post_hoc_plot_vertical_reference_line_custom', 'post_hoc_plot_horizontal_reference_line_custom'),
      c('post_hoc_plot_regression_line', 'post_hoc_plot_equality_line'),
        'post_hoc_plot_show_stats', 'post_hoc_plot_legend_location'
    ),
    '[-]Download per-electrode statistics' = list(
      c('pes_sort_by_metric'),
        'pes_group_by_roi', 
      'pes_scaling_type',
      c('pes_max_for_means', 'pes_max_for_means_is_percentile'),
      c('pes_max_for_contrasts', 'pes_max_for_contrasts_is_percentile'),
      'pes_separate_colorbar',
      c('pes_means_heatmap_palette', 'pes_contrasts_heatmap_palette'),
      
      c('download_pes')
      
    ),
    '[-]Export hi-res plot' = list(
      c('custom_plot_download')
    )
)



# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs

# define_output(
#     definition = customizedUI('src_data_snapshot', style='min-height:500px'),
#     title = 'Data Description',
#     width = 5,
#     order = 1e2
# )

define_output(
    definition = customizedUI('lme_out', style='min-height:200px'),
    title = 'Overall results',
    width = 12,
    order = 100
)


define_output(
  definition = customizedUI('multiple_comparisons', style='min-height:200px'),
  title = 'Contrast results',
  width = 12,
  order = 200
)

define_output(
  definition = customizedUI('regression_output', style='min-height:200px'),
  title = 'Full model output',
  width = 12,
  order = 200
)



define_output(
  definition = plotOutput('power_over_time', height='500px'),
  title = 'Activity over time',
  width = 8,
  order = 60
)

define_output(
  definition = plotOutput('windowed_activity', height='500px'),
  title = 'Mean activity in analysis window',
  width = 4,
  order = 61
)

define_output(
  definition = customizedUI('mass_univariate_results', style='min-height:500px'),
  title = 'Univariate stat output',
  width = 12, order=20
)

define_output(
  customizedUI('effect_overview_plot_ui'),
  title = 'Results overview heatmap',
  width=12,
  order=-1e5
)

define_output(
  definition = plotOutput('electrode_inspector_time_series', height='500px'),
  title = 'Subset time series',
  width = 6, order=51
)

define_output(
  definition = plotOutput('electrode_inspector_trial_heat_map_plot', height='500px'),
  title = 'Subset by-trial plot',
  width = 6, order=50
)

define_output(
  definition = plotOutput('post_hoc_plot', height='500px'),
  title = 'Compare post-hoc variables',
  width = 12, order=70
)

define_output_3d_viewer(
    outputId = 'lme_3dviewer',
    message = 'Reload Viewer',
    title = 'Statistical results by electrode',
    height = '500px', 
    additional_ui = htmltools::tagList(' | ', downloadLink(ns('download_3dv_colobar'), 'Download color bar')),
    order = -1
)

# 'Multiple Comparisons' = c('multiple_comparisons'),


# this is for the multiple-tabbed layout
# output_layout = list(
#   'Model Results' = list(
#     'Results across electrodes' = c('power_over_time', 'windowed_activity', 'lme_out'),
#     'Results by electrode' = c('lme_3dviewer', 'mass_univariate_results',
#                                'electrode_inspector_time_series',
#                                'electrode_inspector_barplot')#,
#     # 'Data description' = c('src_data_snapshot')
#   )
#   # 'Multiple Output' = 'src_data_snapshot'
# )

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------


# -------------------------------- View layout ---------------------------------
module_id <- 'group_analysis_lme'
quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T, sidebar_width = 3)
