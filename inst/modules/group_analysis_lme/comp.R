# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()

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
    inputId= 'analysis_data', label = 'Data files', paths = c('_project_data/group_analysis_lme/source', '_project_data/power_explorer'),
    reactive_target = 'local_data$analysis_data_raw'
)

# define_input(
#     customizedUI('var_sel')
# )
define_input_table_filters('var_sel', label = 'Filter', watch_target = 'local_data$analysis_data',
                           reactive_target = 'local_data[["analysis_data_filtered"]]')



#### Define model

define_input(
    selectInput('model_dependent', 'Dependent', choices = '', selected = character(0))
)
define_input(
    selectInput('model_fixed_effects', 'Fixed effects', choices = '', selected = character(0), multiple = TRUE)
)
define_input(
    selectInput('model_random_effects', 'Random effects', choices = '', selected = character(0), multiple = TRUE)
)
define_input(
    textInput('model_formula', 'Formula', value = '')
)
define_input(
    checkboxInput('model_embedsubject', HTML('Embed subject into electrode <small style="color:#a1a1a1">[only if both Subject and Electrode are selected as random effect]</small>'), value = TRUE)
)
define_input(
    checkboxInput('model_splinetime', HTML('Wrap <span style="font-style:italic">Time</span> with Splines <small style="color:#a1a1a1">[use splines::bs(Time)]</small>'), value = TRUE)
)

define_input(
    actionButtonStyled('run_analysis', 'Run Analysis', type = 'primary', width = '100%')
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

define_input(
  definition = customizedUI('cond_group_ui')
)


manual_inputs = c('source_files', 'csv_file', 'load_csvs', 'model_dependent', 
                  'model_fixed_effects', 'model_random_effects', 'model_splinetime',
                  'model_formula', 'model_embedsubject', 'run_analysis'
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
        'analysis_data',
        'cond_group_ui'
    ),
    '[-]Data Filter' = list(
      'var_sel'
    ),
    # 'Feature Selection' = list(
    #     c('omnibus_f', 'fcutoff')
    # ),
    'Model Building' = list(
        c('model_dependent'),
        c('model_fixed_effects', 'model_random_effects'),
        'model_embedsubject',
        'model_splinetime',
        'model_formula',
        'run_analysis'
        
        # 'var_dependent_ui',
        # 'var_fixed_effects_ui',
        # 'var_rand_effects_ui',
        # 'var_formula_ui',
        # 'nested_electrode'
    )
)



# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs

define_output(
    definition = customizedUI('src_data_snapshot', style='min-height:500px'),
    title = 'Data Snapshot',
    width = 5,
    order = 2
)

define_output(
    definition = customizedUI('lme_out', width = 12, style='min-height:300px'),
    title = 'LME Output',
    width = 12,
    order = 1
)

define_output(
    definition = plotOutput('lmer_diagnosis', height = '520px'),
    title = 'Diagnostic Plots',
    width = 7,
    order = 3
)


# hr(),
# h4('Diagnostic Plots'),
# shiny::plotOutput(ns('lmer_diagnosis'))
define_output_3d_viewer(
    outputId = 'lme_3dviewer',
    message = 'Reload Viewer',
    title = 'Statistical results by electrode',
    order = 1e4
)


output_layout = list(
  'Tabset One' = list(
    'Multiple Output' = c('lme_out'),
    '3D Visualization' = c('lme_3dviewer')
  )
  # 'Multiple Output' = 'src_data_snapshot'
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------






# -------------------------------- View layout ---------------------------------
module_id <- 'group_analysis_lme'
quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T, sidebar_width = 3)
