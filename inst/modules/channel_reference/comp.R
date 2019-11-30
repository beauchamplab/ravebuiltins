# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(rave)
require(ravebuiltins)

dev_ravebuiltins(T)

## Load subject for debugging
mount_demo_subject()

module_id <- 'channel_reference'



# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
    get_path('inst/modules/channel_reference/reactive_main.R'),
    get_path('inst/modules/channel_reference/common.R'),
    asis = TRUE
)

define_initialization({
    rave_checks('voltage full')
    clear_all = FALSE

    if(length(env$subject_code) != 1 || env$subject_code != subject$subject_code || env$project_name != subject$project_name){
        clear_all = T
    }

    if(clear_all){
        # clean all caches
        nms = ls(env, all.names = T)
        if(length(nms)){
            rm(list = nms, envir = env)
        }
        env$switched = T


        # Trick 1, update all information at the first component
        env$dirs = module_tools$get_subject_dirs()
        env$existing_refs = list.files(env$dirs$meta_dir, pattern = '^reference_.*\\.[cC][sS][vV]$')
        env$ref_dir = file.path(env$dirs$channel_dir, 'reference')
        env$last_import = 'new..'
        env$subject_code = subject$subject_code
        env$project_name = subject$project_name
    }

    check_load_volt()
    brain = rave::rave_brain2(subject)
    # Load current brain
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

define_input(
    definition = selectInput('ref_name_alt', 'Import From', choices = 'new..', selected = 'new..'),
    init_args = c('choices', 'selected'),
    init_expr = {
        choices = unique(c('new..', env$existing_refs))
        selected = env$last_import
    }
)

define_input(
    definition = dipsaus::compoundInput2(
        'ref_group', label = 'Reference Group', max_ncomp = 20, min_ncomp = 1, 
        components = tagList(
            textInput('rg_name', 'Name', value = ''),
            selectInput('rg_type', 'Type', choices = c(
                'Common Average Reference', 'Bipolar Reference',
                'White Matter Reference', 'No Reference'), selected = 'No Reference'),
            textInput('rg_electrodes', 'Electrodes', value = '', placeholder = 'e.g. 1-12,14')
        )
    )
)


define_input(
    definition = selectInput('cur_group', 'Group Number', choices = 1:20, selected = NULL)
)

define_input(
    definition = customizedUI('elec_loc_ui')
)

define_input(
    definition = customizedUI('cur_group_ui')
)


define_input(
    textInput('ref_electrodes', label = 'Electrodes', value = '', 
              placeholder = 'e.g. 1-3,5'), 
    update_level = 0
)

define_input(
    actionButton('ref_calc', label = 'Generate Reference', width = '100%'), 
    update_level = 0
)


input_layout = list(
    'Overall' = list('ref_name',
                     'ref_name_alt',
                     'ref_group'),
    '[-] Group Inspection' = list('cur_group',
                                  'elec_loc_ui',
                                  'cur_group_ui'),
    '[-] Reference Generator' = list(
        'ref_electrodes',
        'ref_calc'
    )
)



# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
    definition = customizedUI('parallel_plot_ui'),
    title = 'Voltage Plot',
    width=12,
    order = 1
)


define_output(
    definition = customizedUI('electrode_plot_ui'),
    title = 'Electrode Statistics',
    width = 12,
    order = 2
)

output_layout = list(
    width = 12L,
    # Tabset name
    'Visualization' = list(
        # Tab name 1
        'Group Inspection' = list(
            'parallel_plot_ui'
        ),

        # Tab name 2
        'Electrode Inspection' = list(
            'electrode_plot_ui'
        )
    )
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------






# -------------------------------- View layout ---------------------------------
module_id <- 'channel_reference'

view_layout(module_id, launch.browser = T, sidebar_width = 3)
