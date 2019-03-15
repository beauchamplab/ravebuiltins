# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()

module_id <- 'overview_viewer_3d'



# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
    get_path('inst/modules/overview_viewer_3d/import.R'),
    get_path('inst/modules/overview_viewer_3d/utils.R'),
    get_path('inst/modules/overview_viewer_3d/reactives.R')
)

define_initialization({

    .module_path = 'Viewer3D'
    .module_id = 'viewer_3d'
    .preserved = c('Voltage, Referenced', 'Power, Referenced', 'Phase, Raw')


    masks = module_tools$get_subject_data(
        name = 'file_list',
        path = .module_path,
        check_cache = T,
        default = new.env(parent = baseenv()),
        try_open = T
    )
    if(!is.environment(masks)){
        masks = new.env(parent = baseenv())
    }

    for(p in .preserved){
        masks[[p]] = NULL
    }
    env$masks = masks
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs




# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
    definition = threejsr::threejsOutput('viewer', height = '80vh'),
    title = '3D Viewer',
    width=12,
    order = 1
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------






# -------------------------------- View layout ---------------------------------
module_id <- 'overview_viewer_3d'
quos = env$parse_components(module_id)

view_layout(module_id, launch.browser = T)
