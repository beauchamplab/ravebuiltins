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

dev_ravebuiltins(T, reload = TRUE)

module_id <- 'overview_viewer_3d'



# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

load_scripts(rlang::quo({
    DEBUG = FALSE
    eval_when_ready(function(...){
        auto_recalculate( FALSE )
    })
}),
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

define_input(
    definition = shiny::checkboxInput(inputId = 'use_template', 
                                      label = 'Use Template Brain', 
                                      value = FALSE)
)

define_input_auto_recalculate(
    'viewer_result_btn2', label = 'Update data', type = 'button',
    default_on = FALSE, button_type = 'success'
)

define_input_auto_recalculate(
    'viewer_result_btn1', label = 'Generate viewer', type = 'button',
    default_on = FALSE, button_type = 'primary'
)


# Add csv file
define_input(
    definition = shiny::fileInput('csv_file', label = 'Upload fst or csv data files', 
                                  accept = c('.csv', '.fst'), multiple = TRUE)
)
define_input(definition = customizedUI('file_check', width = '100%'))

define_input(
    definition = shiny::selectInput('data_files', label = 'Data Files', 
                                    choices = NULL, selected = character(0), 
                                    multiple = TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
        # Find all csvs
        choices = find_csv( project_dir, NULL )
        local_env$csv_files = choices
        selected = cache_input('data_files', character(0))
    }
)


define_input(
    rave::customizedUI('download_ui')
)

define_input(
    rave::customizedUI('electrode_details')
)

define_input(
    definition = selectInput(inputId = 'heatmap_color_palette', label='Heatmap color palette', multiple=FALSE, 
                             choices = get_heatmap_palette(get_palette_names = TRUE),
                             selected = get_heatmap_palette(get_palette_names = TRUE)[1]),
    
    # cache the color palette across data reloads. needs init_args and init_expr
    init_args = c('selected'),
    init_expr = {
        selected = cache_input('heatmap_color_palette',
                               val = get_heatmap_palette(get_palette_names = TRUE)[1])
    }
)



input_layout = list(
    'Subject & Surfaces' = list(
        c('subject_codes', 'surface_types'),
        'use_template',
        c('viewer_result_btn1', 'download_ui')
    ),
    'Data Source' = list(
        c('data_files'),
        c('csv_file'),
        c('file_check'),
        c('viewer_result_btn2')
    ),
    'Viewer Controls' = list(
        'heatmap_color_palette'
    ),
    'Data Inspector' = list(
        'electrode_details'
    )
)

manual_inputs = c('viewer_result_download')



# End of input
# ----------------------------------  Outputs ----------------------------------
#' Define Outputs

# define_output_3d_viewer(outputId = 'viewer_result', title = 'Embedded Viewer',
#                         order = 1, width = 12, hide_btn = TRUE, height = '82vh')

define_output_3d_viewer(
    outputId = 'brain_viewer', 
    title = 'Viewer',
    width = 12L,
    order = 1,
    hide_btn = TRUE,
    height = '85vh'
)

# define_output(
#     definition = rave::customizedUI('electrode_details'),
#     title = 'Details',
#     width = 4L,
#     order = 3
# )

define_output(
    definition = rave::customizedUI('electrode_table_ui'), #DT::dataTableOutput('electrode_table'),
    title = 'Combined Data File',
    width = 12,
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

view_layout(module_id)


m = to_module(module_id = module_id, sidebar_width = 3L, parse_context = 'rave_running_local')

init_app(m, test.mode=TRUE)
exec_env = m$private$exec_env$xD2CvLYFr1BNt9eOusVG
exec_env$static_env$viewer_brain()
e = environment(exec_env$static_env$viewer_brain)
parent.env(e)$.__rave_context__.
