# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
# env$mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
# load_scripts(
#     'inst/modules/voltage_explorer/exports.R'
# )

define_initialization({
    ##
    ## Make sure power (referenced) exists
    ## with the following line, RAVE will pop up a dialogue if
    ## power is not loaded and ask users to load data
    ##
    rave_checks('voltage referenced')

    ##
    ## Get referenced power (Wavelet power)
    ##
    voltage = module_tools$get_voltage(referenced = TRUE)
    # unref_voltage = module_tools$get_voltage(referenced = FALSE, force=TRUE)

    ## Shared variables
    frequencies = preload_info$frequencies
    time_points = preload_info$time_points
    electrodes = preload_info$electrodes
    epoch_data = module_tools$get_meta('trials')
})


#  ---------------------------------  Inputs -----------------------------------
# Define inputs

# Select from multiple choices,


define_input(
    definition = customizedUI(inputId = 'input_customized')
)

define_input_condition_groups(inputId = 'GROUPS')
define_input_multiple_electrodes(inputId = 'ELECTRODE_TEXT')
define_input_frequency(inputId = 'FREQUENCY')
define_input_time(inputId = 'ANALYSIS_WINDOW', label='Analysis', initial_value = c(0,1))
define_input_time(inputId = 'BASELINE_WINDOW', label='Baseline', initial_value = c(-1,0))

define_srate_input_slider('ERP_SAMPLE_RATE', 'Sampling Rate (Hz)')

define_input(
    definition = numericInput('max_zlim', 'Max Heatmap Plot Value', value = 0, min = 0, step = 1)
)

define_input(
    definition = checkboxInput('do_baseline', 'Use Baseline')
)

define_input(
    definition = selectInput(inputId = 'BASELINE_TYPE', label = 'Baseline Method', choices=c('% Change', 'dB'), selected='% Change')
)

# Define layouts if exists
input_layout = list(
    '[#cccccc]Electrodes' = list(
        c('ELECTRODE_TEXT')
    ),
    #[#99ccff]
    'Trial Selector' = list(
        'GROUPS'
    ),
    'Analysis Settings' = list(
        'ERP_SAMPLE_RATE',
        'FREQUENCY',
        c('do_baseline',
          'BASELINE_TYPE'),
        'BASELINE_WINDOW',
        'ANALYSIS_WINDOW'
    ),
    #[#aaaaaa]
    '[-]Export Options' = list(),
    '[-]Plotting' = list()
)

# End of input
# ----------------------------------  Outputs ----------------------------------
# Define Outputs
define_output(
    definition = plotOutput(outputId = 'erp_over_time_plot'),
    title = 'ERP over time',
    width = 12,
    order = 1
)

define_output(
    definition = plotOutput('by_trial_erp_map'),
    title = 'ERP over time by trial',
    width = 12,
    order = 2
)

define_output(
    definition = plotOutput(outputId = 'by_condition_welch'),
    title = 'Compare Periodograms',
    width = 12,
    order = 3
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------


# -------------------------------- View layout ---------------------------------
quos = env$parse_components(module_id = 'voltage_explorer')

view_layout('voltage_explorer', launch.browser = T)

