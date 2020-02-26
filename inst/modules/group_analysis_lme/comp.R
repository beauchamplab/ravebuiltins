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
            list.files(power_explorer_dir, pattern = '\\.[cC][sS][vV]$'),
            list.files(group_analysis_src, pattern = '\\.[cC][sS][vV]$')
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
# Define inputs

# Part 1: data selector:

# define_input(
#     selectInput('source_files', 'Data files', choices = '', selected = NULL, multiple = TRUE),
#     init_args = c('choices', 'selected'),
#     init_expr = {
#         # Check csv files in project/_project_data/power_explorer and project/_project_data/group_analysis_lme/source
#         choices = rescan_source(update = FALSE)
#         selected = cache_input('source_files', val = character(0))
#     }
# )
# define_input(
#     definition = shiny::fileInput('csv_file', label = 'Upload a csv Data File', accept = 'text/csv', multiple = FALSE)
# )
# # define_input(definition = customizedUI('file_check', width = '100%'))
# define_input(
#     definition = actionButtonStyled('load_csvs', 'Load analysis tables', type = 'primary')
# )

define_input_analysis_data_csv(
    inputId= 'analysis_data', label = "Data files located in this project's RAVE directory", 
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
    
    # updateCheckboxInput(session, inputId = 'auto_calculate', value = FALSE)
    # lapply(1:10, function(ii){
    #   gc_id = sprintf('GROUPS_group_conditions_%d', ii)
    #   gc = conf[[gc_id]]
    #   if(!length(gc)){ gc = character(0) }
    #   print(paste(ii, c(gc)))
    #   updateSelectInput(session, gc_id, selected = gc)
    # })
  })
}))


define_input(
    selectInput('model_dependent', 'Dependent', choices = '', selected = character(0))
)
# define_input(
#     selectInput('model_fixed_effects', 'Fixed effects', choices = '', selected = character(0), multiple = TRUE)
# )
# define_input(
#     selectInput('model_random_effects', 'Random effects', choices = '', selected = character(0), multiple = TRUE)
# )
define_input(
    textInput('model_formula', 'Formula', value = 'Power ~ Group + (1|Subject/Electrode)')
)
# define_input(
#     checkboxInput('model_embedsubject', HTML('Embed subject into electrode <small style="color:#a1a1a1">[only if both Subject and Electrode are selected as random effect]</small>'), value = TRUE)
# )
# define_input(
#     checkboxInput('model_splinetime', HTML('Wrap <span style="font-style:italic">Time</span> with Splines <small style="color:#a1a1a1">[use splines::bs(Time)]</small>'), value = TRUE)
# )

define_input(
    actionButtonStyled('run_analysis', 'Run Analysis', type = 'primary', width = '100%')
)

define_input(
  definition = customizedUI('download_all_results')
)


# We can't use define_input_condition_groups as it defaults to preload_info$condition
# In fact every project might have different stimulus for each subjects, then condition is not
# the same sometime
# define_input(
#     definition = compoundInput(
#         inputId = 'cond_group', prefix= 'Condition Group', inital_ncomp = 1, components = {
#             textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
#             selectInput('group_conditions', ' ', choices = '', multiple = TRUE, selected = character(0))
#         }, max_ncomp = 20)
# )

# define_input(
#   definition = customizedUI('cond_group_ui')
# )

define_input_condition_groups(
  inputId = 'cond_group', label = 'Condition Group', initial_groups = 2,
  max_group = 20, min_group = 2, label_color = 'grey40', 
  init_args = NULL, init_expr = NULL
)
# dipsaus::compoundInput2(
  #         inputId = ns('cond_group'), prefix= 'Condition Group', inital_ncomp = 1, components = {
  #             textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
  #             selectInput('group_conditions', ' ', choices = '', multiple = TRUE, selected = character(0))
  #         }, max_ncomp = 20)


define_input(
  definition = sliderInput('analysis_window', 'Analysis Window',
                           min = 0, max = 1, value=c(0,1), round=-2, step=0.01)
)


define_input(customizedUI('repeated_measures_note'))


define_input(
  definition = dipsaus::compoundInput2(
    inputId = 'multi_window_analysis', label = 'Time Window', inital_ncomp = 2, min_ncomp = 2, max_ncomp = 5,
    label_color = 'grey40', 
    components = htmltools::div(
      textInput('window_name', 'Name', value = '', placeholder = 'window name'),
      sliderInput('analysis_window', ' ', value =0:1, post = 's', min=-2, max=2, step = 0.01),
      checkboxInput('window_is_active', 'Active', value = FALSE)
    )
  )
  # init_args = !!init_args,
  
  # init_expr = !!init_expr
)


manual_inputs = c('source_files', 'csv_file', 'load_csvs', 'analysis_window',
                  'model_dependent', 'model_fixed_effects', 'model_random_effects', 'model_splinetime',
                  'model_formula', 'model_embedsubject', 'run_analysis','download_all_results'
                  )

# # selectInput('electrode', 'Electrode', choices = '', multiple = F),
# textInput('electrode_text', 'Electrodes', value = "", placeholder = '1-5,8,11-20'),
#
# selectInput('combine_method', 'Electrode Transforms',
#             choices = c('none', 'z-score', 'max-scale', '0-1 scale', 'rank'), multiple = F, selected = 'none'),
#
input_layout = list(
    '[#cccccc]Data Import' = list(
        # c('participants'),
        # c('analysis_name_ui')
        # 'source_files', 'csv_file', 'load_csvs'
        'analysis_data'
    ),
    'Choose Conditions' = list(
      'cond_group',
      'cond_group_ui'
      ),
    'Single time window analysis' = list(
      'analysis_window'
    ),
    '[-]Multiple time window analysis' = list(
      'repeated_measures_note', 
      'multi_window_analysis'
    ),
    # 'Filter Data' = list(
    #   'var_sel'
    # ),
    # 'Feature Selection' = list(
    #     c('omnibus_f', 'fcutoff')
    # ),
    'Build Model' = list(
        c('model_dependent'),
        # c('model_fixed_effects', 'model_random_effects'),
        # 'model_embedsubject',
        # 'model_splinetime',
        'model_formula',
        'run_analysis',
        'download_all_results'
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
    definition = customizedUI('lme_out', style='min-height:500px'),
    title = 'Model Output',
    width = 12,
    order = 2
)

define_output(
  definition = plotOutput('power_over_time', height='500px'),
  title = 'Activity over time',
  width = 8,
  order = 1
)

define_output(
  definition = plotOutput('windowed_activity', height='500px'),
  title = 'Mean activity in analysis window',
  width = 4,
  order = 1
)

define_output(
  definition = customizedUI('mass_univariate_results', style='min-height:500px'),
  title = ' ',
  width = 12, order=5
)

define_output_3d_viewer(
    outputId = 'lme_3dviewer',
    message = 'Reload Viewer',
    title = 'Statistical results by electrode',
    order = 4
)

# 'Multiple Comparisons' = c('multiple_comparisons'),

output_layout = list(
  'Model Results' = list(
    'Results across electrodes' = c('power_over_time', 'windowed_activity', 'lme_out'),
    'Results by electrode' = c('lme_3dviewer', 'mass_univariate_results')#,
    # 'Data description' = c('src_data_snapshot')
  )
  # 'Multiple Output' = 'src_data_snapshot'
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------


# -------------------------------- View layout ---------------------------------
module_id <- 'group_analysis_lme'
quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T, sidebar_width = 3)
