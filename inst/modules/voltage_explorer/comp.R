# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(ravebuiltins)

env = dev_ravebuiltins(T)

## Load subject for debugging
# env$mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
# load_scripts(
#     'inst/modules/voltage_explorer/exports.R', 
# asis = TRUE
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
define_input_single_electrode(inputId = 'ELECTRODE')
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

# let people decide how much information to include in the plots. It's up to the individual plot to actually make
# use of this information, probably through shared decorators
define_input(
    definition = selectInput(inputId = 'PLOT_TITLE', label = 'Plot Decorations', multiple=TRUE,
                             choices =c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'),
                             selected=c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range', 'Sample Size', 'Baseline Window', 'Analysis Window'))
)

define_input(
    definition = selectInput(inputId = 'plots_to_export', label='Plots to Export', multiple=TRUE,
                             choices = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average'),
                             selected = c('Spectrogram', 'By Trial Power', 'Over Time Plot', 'Windowed Average'))
)

define_input(
    definition = selectInput(inputId = 'export_what', label='Which electrodes should be exported?', multiple=FALSE,
                             choices = c('All Loaded', 'Current Selection'))
)


define_input(
    definition = selectInput(inputId = 'color_palette', label='Color palette', multiple=FALSE, 
                             choice=get_palette(get_palette_names = TRUE),
                             selected = get_palette(get_palette_names = TRUE)[1]),
    
    # cache the color palette across data reloads. needs init_args and init_expr
    init_args = c('selected'),
    init_expr = {
        selected = cache_input('color_palette', val = get_palette(get_palette_names = TRUE)[1])
    }
)

define_input(
    definition = selectInput(inputId = 'background_plot_color_hint', label = 'Background color', multiple=FALSE,
                             choices = c('White', 'Black', 'Gray'))
)

define_input(
    definition = checkboxInput('invert_colors_in_palette', "Inverse Palette Colors", value=FALSE)
)

define_input(
    definition = checkboxInput('draw_decorator_labels', "Label Plot Decorations", value=TRUE)
)

define_input(
    definition = checkboxInput('reverse_colors_in_palette', "Reverse Palette Order", value=FALSE)
)

define_input(
    definition = customizedUI('graph_export')
)




# Define layouts if exists
input_layout = list(
    '[#cccccc]Electrodes' = list(
        c('ELECTRODE')
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
    '[-]Plot Options' = list(
        c('PLOT_TITLE'),
        'draw_decorator_labels',
        c('color_palette', 'background_plot_color_hint',
        'invert_colors_in_palette', 'reverse_colors_in_palette'),
        c('max_zlim'),
        c('log_scale', 'sort_trials_by_type', 'collapse_using_median')
    ),
    '[-]Export Options' = list()
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

