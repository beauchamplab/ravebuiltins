#' @author Zhengjia Wang
#' This is UI component especially designed for 3D viewers which uses `threebrain`
#' to visualize electrodes as well as surfaces via webgl.
#' 
#' If you are looking for basic examples to learn how to build a RAVE package, 
#' this might NOT be the place you want to be at. i used lots of advanced r-shiny
#' components to control the workflow. 
#' 
#' However, if you have already learned basic RAVE components and want to 
#' have advanced/flexible modules, this might be the place you want to have 
#' a look.
#' 
NULL
# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()

module_id <- 'overview_viewer_3d'



# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

load_scripts(rlang::quo({ DEBUG = FALSE }),
             'inst/modules/overview_viewer_3d/reactives.R', 
             'inst/modules/overview_viewer_3d/outputs.R', 
             asis = TRUE)


define_initialization({
    project_name = subject$project_name
    current_subject_code = subject$subject_code
    all_subject_code = rave::get_subjects( project_name )
    project_dir = normalizePath(file.path(subject$dirs$rave_dir, '../../'))
})




#  ---------------------------------  Inputs -----------------------------------
define_input(
    definition = shiny::selectInput(
        inputId = 'subject_codes', label = 'Subject', choices = '', 
        selected = character(0), multiple = TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
        choices = all_subject_code
        selected = cache_input('subject_codes', current_subject_code)
    }
)

define_input(
    definition = shiny::selectInput(
        inputId = 'surface_types', label = 'Additional Surface Types',
        choices = c('white', 'smoothwm', 'inf_200', 'pial-outer-smoothed', 'sphere', 'inflated'), 
        selected = character(0), multiple = TRUE),
    init_args = c('selected'),
    init_expr = {
        selected = cache_input('surface_types', character(0))
    }
)

define_input(definition = shiny::checkboxInput(inputId = 'use_template', label = 'Use Template Brain', value = FALSE))

define_input(definition = rave::actionButtonStyled(inputId = 'viewer_result_btn2', type = 'success', 'Update Viewer', width = '100%'))


# Add csv file
define_input(
    definition = shiny::fileInput('csv_file', label = 'Upload a csv Data File', accept = 'text/csv', multiple = TRUE)
)
define_input(definition = customizedUI('file_check', width = '100%'))

define_input(
    definition = shiny::selectInput('data_files', label = 'Data Files', choices = NULL, selected = character(0), multiple = TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
        # Find all csvs
        choices = find_csv( project_dir, NULL )
        local_env$csv_files = choices
        selected = cache_input('data_files', character(0))
    }
)


define_input_3d_viewer_generator('viewer_result', label = 'Open viewer in a new tab', reactive = 'local_data')



input_layout = list(
    'Subject & Surfaces' = list(
        c('subject_codes'),
        c('surface_types'),
        c('use_template'),
        'viewer_result_btn2'
    ),
    'Data Source' = list(
        c('data_files'),
        c('csv_file'),
        c('file_check')
    ),
    'Misc' = list(
        c('viewer_result', 'viewer_result_ui')
    )
)
manual_inputs = c('subject_codes', 'surface_types', 'use_template', 'csv_file', 'viewer_result_download')



# End of input
# ----------------------------------  Outputs ----------------------------------
#' Define Outputs

# define_output_3d_viewer(outputId = 'viewer_result', title = 'Embedded Viewer',
#                         order = 1, width = 12, hide_btn = TRUE, height = '82vh')

define_output(
    definition = rave::customizedUI('viewer_result_out_ui'),
    title = 'Viewer',
    width = 12L,
    order = 1
)

define_output(
    definition = rave::customizedUI('electrode_details'),
    title = 'Details',
    width = 4L,
    order = 3
)

define_output(
    definition = rave::customizedUI('electrode_table_ui'), #DT::dataTableOutput('electrode_table'),
    title = 'Combined Data File',
    width = 8,
    order = 2
)

# output_layout = list(
#   width = 12L,
#   'Outputs' =list(
#     'Data Table' = list(
#       'electrode_details', 'viewer_result_out_ui'
#     ),
#     '3D Viewer' = list(
#       'electrode_table'
#     )
#   )
# )


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------


# -------------------------------- View layout ---------------------------------
module_id <- 'overview_viewer_3d'
# quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T, sidebar_width = 3)
