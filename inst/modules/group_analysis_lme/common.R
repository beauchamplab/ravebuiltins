input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

local_data = reactiveValues(
    potential_analysis = list(),
    analysis_name = NULL,
    sample_table = NULL,
    var_dependent = NULL,
    var_fixed_effects = NULL,
    lmer_results = NULL,
    full_table=NULL,
    filter_count = 0,
    filters = list()
)


matrix_to_table <- function(mat, row_label=' ') {
    cnms <- colnames(mat)
    rnms <- rownames(mat)

    str <- '<div style="width:100%;overflow-x:scroll;"><table style = "width:900px">'

    #header row
    str <- str %&%
        '<tr style="border-bottom:1px solid #333"><td style="font-weight:bold"> '%&%
        paste0(c(row_label, cnms), collapse='</td><td style="font-weight:bold">') %&%
        '</td><tr>'

    # all the rows
    for(ii in seq_len(nrow(mat))) {
        #one of the things we want to do is fix the row names so that instead of A:B they are A &times; B
        str <- str %&%
            '<tr><td>' %&%
            paste0(c(str_replace_all(rnms[ii], ':', '&times;'), formatC(mat[ii,], digits=3)), collapse='</td><td>') %&%
            '</td></tr>'
    }
    str <- str %&% '</table></div>'

    return(str)
}


output$lme_output <- renderUI({

    .lmer <- isolate(local_data$lmer_results)
    # flat_data <- isolate(local_data$full_table)
    #
    #   # ranef
    #   ngrps(.lmer)
    #
    #   model.matrix(.lmer, 'fixed') %>% str
    #   model.frame(.lmer) %>% str
    #
    #   plot(fitted(.lmer) + resid(.lmer), fitted(.lmer), asp=1)
    #

    # put a description row
    txt <- '<p>LME Call: ' %&% format(formula(.lmer)) %&% '<br/>' %&%
        (summary(.lmer)$methTitle %>% str_replace_all('\n', '<br/>')) %&% '</p>'

    # fancy up the variable printing a bit, put the str_rep in parentheses so we don't mess up the description lines above
    # we also want to give people a hint about the reference level
    # ref_tt <- levels(flat_data$)[1]
    # ref_tp <- levels(flat_data$time_period)[1]
    # intercept <- sprintf('Intercept=%s:%s', ref_tt, ref_tp)
    txt <- txt %&% (
        (summary(.lmer)$coefficients %>% matrix_to_table(row_label='Coef')) #%>%
        # str_replace_all('trial_type', 'Trial=') %>%
        # str_replace_all('time_period', 'Time=') %>%
        # str_replace_all('(Intercept)', intercept)
    )

    HTML(
        # '<iframe width="420" height="345" src="https://www.youtube.com/embed/gEDYR2N7wCM"></iframe>'
        txt)
})

var_sel = function(){
    if(local_data$filter_count > 0){
        minus_btn = actionButton(ns('filter_minus'), shiny::icon('minus'))

        vars = names(local_data$sample_table)
        vars %?<-% ''

        # restore filters if previous entered
        filters = isolate(local_data$filters)
        filter = list()  # default list

        filter_uis = lapply(
            seq_len(local_data$filter_count),
            function(ii){
                if(length(filters) >= ii){
                    # Make sure if you hit "+", the last loop won't crash
                    # in that case, filter will be the list() in parent environment
                    filter = filters[[ii]]
                }
                tagList(
                    tagList(
                        tags$label('Filter ' %&% ii, style = ifelse(ii == 1, '', 'margin-top: 15px;')),
                        div(
                            # To make a box to wrap group inputs
                            class = 'rave-grid-inputs',
                            div(
                                style = 'flex-basis: 50%;',
                                selectInput(ns('filter_var_' %&% ii), 'Variable', choices = vars, selected = get_val(filter, 'var', default = NULL))
                            ),
                            div(
                                style = 'flex-basis: 50%;',
                                selectInput(ns('filter_op_' %&% ii), 'Operator', choices = c('=', '!=', '>', '>=', '<', '<=', 'in', 'not in', 'between'), selected = get_val(filter, 'op', default = '='))
                            ),
                            div(
                                style = 'flex-basis: 100%;',
                                textInput(ns('filter_val_' %&% ii), 'Value', value = get_val(filter, 'val', default = NULL))
                            )
                        )

                    )
                )
            }
        )
    }else{
        filter_uis = NULL
        minus_btn = NULL
    }

    tagList(
        filter_uis,
        div(
            # Put a div to make buttons within a row
            actionButton(ns('filter_add'), shiny::icon('plus')),
            minus_btn
        )
    )
}

# A function to collect filters
# Non-reactive mode, please use isolate(get_filters()) to avoid unwanted updates!!!
get_filters = function(){
    fcounts = local_data$filter_count
    if(!length(fcounts)){
        removeNotification(id = ns('filter_noti'))
        # Always returns a list
        return(list(
            content = list(),
            error_counts = 0
        ))
    }

    lapply(seq_len(fcounts), function(ii){
        var = input[['filter_var_' %&% ii]]
        op = input[['filter_op_' %&% ii]]
        val = input[['filter_val_' %&% ii]]

        # TODO Check data
        failed = TRUE
        msg = 'Filter Blablabla'

        list(
            # Don't change
            var = var, op = op, val = val, failed = failed, msg = msg
            # Add filtered value, var, op here
            # filtered_val = ...
        )
    }) ->
        re

    err_msg = lapply(re, function(v){
        if(v$failed){
            return(tags$li(v$msg))
        }else{
            return()
        }
    })
    err_msg = dropNulls(err_msg)

    if(length(err_msg)){
        showNotification(p(strong('Filter error: '), tags$ul(tagList(err_msg))), id = ns('filter_noti'), duration = 60, type = 'error')
    }else{
        removeNotification(id = ns('filter_noti'))
    }
    return(list(
        content = re,
        error_counts = length(err_msg)
    ))
}

observeEvent(input$filter_add, {
    local_data$filter_count = local_data$filter_count + 1
})
observeEvent(input$filter_minus, {
    # Make sure
    local_data$filter_count = max(local_data$filter_count - 1, 0)
})

# test, remove this observer later
# Save filters to local_data
observe({
    filters = get_filters()
    local_data$filters = get_val(filters, 'content', default = list())
})


lme_out = function() {
    # put analysis information in here
    if(!is.null(local_data$lmer_results)) {
        return(htmlOutput(ns('lme_output')))
    }
    return('no calculations yet')

}

# rave_execute({
#     # local_data$participants = participants
#
#     # Compromise, I'll just look at the first subject
#     r = lapply(participants, get_analysis); names(r) = participants
#     local_data$potential_analysis = r
# })


##### Input responses


# function to check analysis names for subjects - right now it's slow
# this will be changed later once group analysis table comes out #?????
get_analysis = function(subject_code){
    rave:::module_analysis_names(module_id = 'power_explorer')
}

analysis_name_ui = function(){
    # It's depending on local_data$potential_analysis, only checks chosen subject
    ps = participants
    r = local_data$potential_analysis

    tbl = table(unlist(r[ps]))
    choice = names(tbl)[tbl == length(ps)]
    selected = local_data$analysis_name

    if(length(selected) == 1 && choice %in% selected){
        imported = TRUE
        btn = actionButtonStyled(ns('import_analysis'), 'Load Analysis', width = '100%', type = 'primary', disabled = TRUE)
    }else{
        imported = FALSE
        btn = actionButtonStyled(ns('import_analysis'), 'Load Analysis', width = '100%', type = 'primary')
    }

    selected = selected[selected %in% choice]

    tagList(
        selectInput(ns('analysis_name'), 'Analysis', choices = choice, selected = selected),
        btn
    )
}

observeEvent(input$import_analysis, {
    analysis_name = input$analysis_name
    if(!length(analysis_name) || is.blank(analysis_name)){
        showNotification(p('Analysis cannot be blank!'), type = 'error', id = ns('notif'))
        return()
    }
    module_table = rave::module_analysis_table(subject$project_name, module_id = 'power_explorer', analysis_name = analysis_name)

    file = module_table$file[1]

    # file = file.path(project_dir, participants[1], 'rave', 'module_data', 'condition_explorer', analysis_name)
    # file = "/Volumes/data/rave_data/ent_data/congruency/YAB/rave/module_data/condition_explorer/stat_out.RDS"
    if(!file.exists(file)){
        showNotification(p('File ', file, ' does not exist!'), type = 'error', id = ns('notif'))
        return()
    }

    tbl = readRDS(file)

    ### Please check your data here
    if(FALSE){
        showNotification(p('File ', file, ' does not exist!'), type = 'error', id = ns('notif'))
        return()
    }

    local_data$sample_table = tbl
    local_data$analysis_name = analysis_name
    showNotification(p('Analysis loaded!'), type = 'message', id = ns('notif'))
})

# Cache inputs to restore
observe({
    local_data$var_dependent = input$var_dependent
})
observe({
    local_data$var_fixed_effects = input$var_fixed_effects
})
observe({
    local_data$var_rand_effects = input$var_rand_effects
})


### filters
build_op <- function(nm) {return (
    function() {
        selectInput(ns(nm), 'Op',
                    choices = c('=', '!=', '>', '>=', '<', '<=', 'in', '! in'),
                    selected = '=')
    })
}
build_var <- function(nm) {
    return ( function() {
        tbl = local_data$sample_table
        vars = names(tbl)
        vars %?<-% ''
        selectInput(ns(nm), 'Var', choices = vars, selected = '')
    })
}
build_val <- function(nm) return ( function() {textInput(ns(nm), 'Val')})


# build the filters using assign just so it's more compact if we end up wanting to have >2 filters
f1var_ui <- build_var('f1var_ui')
f2var_ui <- build_var('f2var_ui')
f1op_ui <- build_op('f1op_ui')
f2op_ui <- build_op('f2op_ui')
f1val_ui <- build_val('f1val_ui')
f2val_ui <- build_val('f2val_ui')

# sapply(1:10, function(ii) {
#     for(v in c('var', 'op', 'val')) {
#       nm <- sprintf('f%s%s_ui', ii, v)
#       assign(nm, do.call(paste0('build_', v), list(nm)),
#              envir = globalenv())
#     }
# })


var_dependent_ui = function(){
    tbl = local_data$sample_table
    vars = names(tbl)
    vars %?<-% ''

    selectInput(ns('var_dependent'), 'Dependent', choices = vars, selected = 'power')
}


var_fixed_effects_ui = function(){
    tbl = local_data$sample_table
    vars = names(tbl)

    vars = vars[!vars %in% local_data$var_dependent]

    vars %?<-% ''
    selectInput(ns('var_fixed_effects'), 'Fixed Effects', choices = vars, selected = isolate(local_data$var_fixed_effects), multiple = T)
}

var_rand_effects_ui = function(){
    tbl = local_data$sample_table
    vars = names(tbl)

    vars = vars[!vars %in% c(local_data$var_dependent, local_data$var_fixed_effects)]
    vars %?<-% ''
    selectInput(ns('var_rand_effects'), 'Random Effects', choices = vars, selected = isolate(local_data$var_rand_effects), multiple = T)
}


var_formula_ui = function(){
    tbl = local_data$sample_table

    dv <- local_data$var_dependent
    fe <- paste0(local_data$var_fixed_effects, collapse=' + ')
    .f <- sprintf('%s ~ %s + (1|subject_id/elec)', dv, fe)

    textInput(ns('var_formula'), 'Formula (editable)', value = as.character(.f))
}




do_btn_ui = function(){
    actionButtonStyled(ns('do_btn'), 'Run Analysis', type = 'info')
}


observeEvent(input$do_btn, {
    main_function()
})




main_function = function(){
    var_fe = local_data$var_fixed_effects
    if(!length(var_fe)){
        local_data$results <- NULL
        showNotification(p('Something is wrong, go back and re-select'), type = 'error', id = ns('notif'))
    }else{
        progress = progress('Analysis', max = length(subjects)+1)
        on.exit({progress$close()})

        module_table = rave::module_analysis_table(subject$project_name, module_id = 'power_explorer', analysis_name = local_data$analysis_name)


        full_table <- lapply_async(subjects, function(sbj) {
            tryCatch({
                readRDS(file = module_table$file[module_table$subject_code == sbj])
            }, error = function(e){
                NULL
            })
        }, .call_back = function(ii) {
            progress$inc('Loading data for ' %>% paste0(subjects[ii]))
        }) %>% {do.call(rbind, (.))}

        var_fixed_effects <- c('condition')
        dv <- 'power'
        fe <- paste0(var_fixed_effects, collapse=' + ')
        .f <- sprintf('%s ~ %s + (1|subject_id/elec)', dv, fe)

        progress$inc('Running LME: ' %&% .f)

        local_data$full_table <- full_table
        local_data$lmer_results <- lmer(.f, data=full_table)

        # summary(lmer_results)
        #
        # lmer_full <- lmer(power ~ condition +
        #                     (1|subject_id) + (1|subject_id:elec) + (1|subject_id:trial), data=full_table)
        # summary(lmer_full)
        # ranef(lmer_full)
        #

        # .l <- length(ranef(lmer_full))
        #
        #     par(mfrow=c(1, .l))
        #     lapply(seq_along(ranef(lmer_full)), function(re_i) {
        #       plot((ranef(lmer_full)[[re_i]][[1]]), main = names(ranef(lmer_full))[re_i])
        #     })



        # lmer1 <- lmer(power ~ condition + (condition|subject_id/elec), data=full_table)
        # summary(lmer1)
        # ranef(lmer1)

        # lmer(power ~ condition + (1|subject_id/elec), data=full_table, REML = FALSE) -> lmer_red
        # summary(lmer_red)
        # ranef(lmer_red)

        # ranef(lmer_full)[['subject_id:trial']][[1]] %>% hist
        # ranef(lmer_full)[['elec:subject_id']][[1]] %>% which.max
        #
        # ranef(lmer_full)[['elec:subject_id']][617,,drop=F]
        #
        # colf <- factor(rownames(ranef(lmer_full)[['elec:subject_id']]) %>% str_remove_all('[0-9]|:'))
        # plot(ranef(lmer_full)[['elec:subject_id']][[1]], col=as.integer(colf), pch=19)

        # agg_table <- aggregate(power ~ elec + subject_id + condition, data=full_table, mean)

        # local_data$lmer_results@formula <- as.character(.f)
        #
        #     summary(lmer_full) %>% coef %>% round(3)
        #     summary(lmer_red) %>% coef %>% round(3)

    }
}


