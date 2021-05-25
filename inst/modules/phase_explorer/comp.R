# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
env$mount_demo_subject()

module_id <- 'phase_explorer'



# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
    'inst/modules/phase_explorer/exports.R',
    'inst/modules/phase_explorer/event_handlers.R', 
    asis = TRUE
)

define_initialization({
    ##
    ## Make sure power (referenced) exists
    ## with the following line, RAVE will pop up a dialogue if
    ## power is not loaded and ask users to load data
    ##
    rave_checks('phase referenced')

    phase = module_tools$get_phase()
    print(dim(phase$get_data()))
    electrodes = preload_info$electrodes
    frequencies = (preload_info$frequencies)#, module_tools$get_sample_rate()/2)
    time_points = round(preload_info$time_points, 5)
    trials = preload_info$condition
    epoch_data = module_tools$get_meta('trials')
    epoch_tbl = epoch_data
    freq_tbl = module_tools$get_meta('frequencies')
    phase_max_freq = max(freq_tbl$Frequency) / 2

    frange <- c(0, 20)
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

# Select from multiple choices,


define_input(
    definition = customizedUI(inputId = 'input_customized')
)

define_input_condition_groups(inputId = 'GROUPS')
define_input_single_electrode(inputId = 'ELECTRODE')
define_input_frequency(inputId = 'FREQUENCY', is_range = FALSE)
define_input_time(inputId = 'analysis_window', label='Analysis Time', initial_value = 0, is_range = FALSE)

define_input(
    definition = numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1)
)


define_input(
    definition = numericInput('max_phase_freq', 'Maximum Frequency', value = 0, min = 0, step = 1)
)

define_input(
    definition = checkboxInput('PHASE_HIST_SINGLE_SCALE', 'Common Scale for Histograms')
)

define_input(
    definition = checkboxInput('MERGE_PLOTS', 'Merge Plots')
)
# define_input(
#     definition = checkboxInput('sort_trials_by_type', 'Sort Trials')
# )
# define_input(
#     definition = checkboxInput('collapse_using_median', 'Collapse w/ Median (NI)')
# )

# Define layouts if exists
input_layout = list(
    '[#cccccc]Electrode' = list(
        c('ELECTRODE')
    ),
    '[#99ccff]Trial Selector' = list(
        'GROUPS'
    ),
    'Analysis Settings' = list(
        'FREQUENCY',
        'analysis_window'
    ),
    '[-]Plotting' = list(
        # c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
        'max_zlim', 'MERGE_PLOTS'
    )
)


# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
    definition = plotOutput(outputId = 'phase_plot'),
    title = 'Phase over time',
    width=12,
    order = 1
)

define_output(
    definition = plotOutput('sine_phase_over_time_plot'),
    title = 'Sine(Phase)',
    width = 12,
    order = 2
)

define_output(
    definition = plotOutput('phase_histogram'),
    title = 'Phase Histogram',
    width = 12,
    order = 4
)

define_output(
    definition = plotOutput(outputId = 'itpc_time_plot'),
    title = 'Inter-trial Phase Coherence',
    width = 12,
    order = 5
)


# Show ITPC across frequencies
define_output(
    definition = plotOutput(outputId = 'itpc_plot_heatmap'),
    title = 'Inter-trial Phase Coherence across Frequencies',
    width = 12,
    order = -3
)

define_output_3d_viewer(
    outputId = 'phase_3d',
    title = '3D Viewer for Phase',
    surfaces = 'pial',
    multiple_subject = F,
    height = '70vh',
    order = 5e10,
    width = 12,
    additional_ui = tagList(
        selectInput(ns('viewer_3d_type'), 'Which statistics', choices = c('b', 't', 'p')),
        p(ns('blah'))
    )
)

# output_layout = list(
#   'Tab1' = list(
#       'ITPC (All Freq)' = c('..itpc_plot_heatmap'),
#       '3D Viewer' = c('phase_3d_widget')
#   ),
#   'Phase Over Time' = list(
#       'Abs-Phase' = c('..phase_plot'),
#       'Sine-Phase' = c('..sine_phase_over_time_plot')
#   ),
#   width = c(12)
# )
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------






# -------------------------------- View layout ---------------------------------
module_id <- 'phase_explorer'
quos = parse_components(module_id)
view_layout(module_id, launch.browser = T, sidebar_width = 3L)

m = to_module('phase_explorer')
rave::init_app(m, test.mode = T)


