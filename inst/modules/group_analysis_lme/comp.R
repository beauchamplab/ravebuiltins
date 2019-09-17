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
    get_path('inst/modules/group_analysis_lme/reactive_main.R'),
    get_path('inst/modules/group_analysis_lme/common.R'), 
    asis = TRUE
)

define_initialization({
    project_name = subject$project_name
    project_dir = dirname(subject$dirs$subject_dir)
    subjects = get_subjects(project_name)
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

define_input(
    selectInput('participants', 'Participants', choices = '', selected = NULL, multiple = T),
    init_args = c('choices', 'selected'),
    init_expr = {
        choices = subjects
        selected=unique(subjects)
    }
)

define_input(
    customizedUI('analysis_name_ui')
)

define_input(
    customizedUI('var_dependent_ui')
)

define_input(
    customizedUI('var_fixed_effects_ui')
)

define_input(
    customizedUI('var_rand_effects_ui')
)

define_input(
    customizedUI('var_formula_ui')
)

define_input(
    checkboxInput('nested_electrode', 'Nest Electrode in Subject', value = T)
)

define_input(
    customizedUI('collapse_trial', 'Collapse over trials (not recommended)', value = F)
)

define_input(
    customizedUI('do_btn_ui')
)


# customizedUI('f1var_ui'),customizedUI('f1op_ui'),customizedUI('f1val_ui'),
# customizedUI('f2var_ui'),customizedUI('f2op_ui'),customizedUI('f2val_ui'),

define_input(
    customizedUI('var_sel')
)


# # selectInput('electrode', 'Electrode', choices = '', multiple = F),
# textInput('electrode_text', 'Electrodes', value = "", placeholder = '1-5,8,11-20'),
#
# selectInput('combine_method', 'Electrode Transforms',
#             choices = c('none', 'z-score', 'max-scale', '0-1 scale', 'rank'), multiple = F, selected = 'none'),
#
input_layout = list(
    '[#cccccc]Dataset' = list(
        c('participants'),
        c('analysis_name_ui')
    ),
    'Feature Selection' = list(
        c('omnibus_f', 'fcutoff'),
        # c('f1var_ui', 'f1op_ui', 'f1val_ui', ''),
        # c('f2var_ui', 'f2op_ui', 'f2val_ui', '')
        c('var_sel')
    ),
    'Model Building' = list(
        'var_dependent_ui',
        'var_fixed_effects_ui',
        'var_rand_effects_ui',
        'var_formula_ui',
        'nested_electrode'
    ),
    'Model Running' = list(
        'do_btn_ui'
    )
)



# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
    definition = customizedUI('lme_out', width = 12),
    title = 'LME Output',
    width=12,
    order = 1
)
# rave_outputs(
#     'LME Output' = customizedUI('lme_out', width = 12)
#     # 'Activity over time by trial (Collapse freq)' = plotOutput('by_trial_heat_map', width = 12),
#     # 'Activity over time (Collapse freq + trial)' = plotOutput('over_time_plot', width = 8),
#     # 'Windowed Comparison (Collapse time + freq)' = plotOutput('windowed_comparison_plot', width = 4),
#     # 'Side Message' = textOutput('msg_out', width = 4),
#     # 'Async Message' = textOutput('async_out', width = 4),
#     # '3D Viewer' = customizedUI('viewer_3d')
# )

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------






# -------------------------------- View layout ---------------------------------
module_id <- 'group_analysis_lme'
quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T)
