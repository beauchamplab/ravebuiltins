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


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
    get_path('inst/modules/overview_viewer_3d/reactive_inputs.R'),
    get_path('inst/modules/overview_viewer_3d/io.R'),
    rlang::quo({
        
        
        # observe({
        #     if(length(local_data$gen_3d)){
        #         local_data$update_viewer_btn
        #         isolate(update_data())
        #         print(local_data$gen_3d)
        #         local_data[['__update']] = Sys.time()
        #     }
        #     
        # })
        
    })
    
)

define_initialization({
    project_name = subject$project_name
    current_subject = subject$subject_code
    all_subjects = rave::get_subjects(project_name)
    
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

define_input(
    definition = selectInput(
        inputId = 'SURFACE_TYPES', label = 'Surface Type(s)', 
        choices = c('pial', 'white', 'smoothwm'),
        multiple = TRUE, selected = 'pial'),
    init_args = 'selected',
    init_expr = {
        selected = 'pial'
    }
)

define_input(
    definition = selectInput(inputId = 'SUBJECTS', label = 'Subject(s)', choices = '', multiple = TRUE),
    init_args = c('choices', 'selected'),
    init_expr = {
        choices = all_subjects
        selected = current_subject
    }
)

define_input(
    definition = checkboxInput(inputId = 'load_n27', label = 'Force to load N27 brain.', value = FALSE)
)

define_input(
    definition = customizedUI('subject_checks')
)

define_input(
    definition = rave::actionButtonStyled('gen_3d', label = 'Generate 3D viewer', type = 'success')
)




define_input(
    definition = fileInput('DATA_FILE', label = 'Data File', multiple = FALSE)
)

define_input(
    definition = customizedUI('data_checks')
)

define_input(
    definition = actionButtonStyled('viewer_update_btn', 'Update Viewer', type = 'info')
)

define_input(
    definition = customizedUI('viewer_inputs3')
)

input_layout = list(
    'Surfaces' = list(
        c('SURFACE_TYPES', 'SUBJECTS'),
        'load_n27',
        'subject_checks',
        'gen_3d'
    ),
    'Data' = list(
        'DATA_FILE',
        'data_checks',
        'viewer_update_btn'
    ),
    'Share' = list('viewer_inputs3')
)


# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs


define_output(
    definition = customizedUI('viewer_wrapper'),
    title = 'Viewer',
    width = 12L,
    order = 1
)


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------


# -------------------------------- View layout ---------------------------------
module_id <- 'overview_viewer_3d'
# quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T, sidebar_width = 3)
