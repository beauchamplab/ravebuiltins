input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()
session = getDefaultReactiveDomain()

local_data %?<-% reactiveValues(
    # Full data has two parts: local_data$analysis_data_raw, and local_data$additional_data
    # together makes analysis_data
    analysis_data_raw = NULL,
    additional_data = NULL,
    analysis_data = NULL,
    collapsed_data = NULL,
    analysis_window = 0:1,
    
    potential_analysis = list(),
    analysis_name = NULL,
    sample_table = NULL,
    var_dependent = NULL,
    var_fixed_effects = NULL,
    lmer_results = NULL,
    lmer_results_summary = NULL
)
local_filters = reactiveValues(
    filter_count = 0,
    filter_observers = 0
)

# cond_group_ui = function(){
#     dipsaus::compoundInput2(
#         inputId = ns('cond_group'), prefix= 'Condition Group', inital_ncomp = 1, components = {
#             textInput('group_name', 'Name', value = '', placeholder = 'Condition Name')
#             selectInput('group_conditions', ' ', choices = '', multiple = TRUE, selected = character(0))
#         }, max_ncomp = 20)
# }


# Sync all group_names
# lapply(1:0, function(ii){
#     name_id = paste0('cond_group_group_name_', ii)
#     .env = environment()
#     observeEvent(input[[name_id]], {
#         val = val_raw = input[[name_id]]
#         if(length(val)){
#             if( stringr::str_detect(val, '^CondGroup[0-9]*') || 
#                 val %in% names(local_data$analysis_data_raw$headers) ){
#                 # Invalid group name, reset to default
#                 val = sprintf('CondGroup%d', ii)
#             }
#             if( val != val_raw ){
#                 updateTextInput(session, name_id, value = val)
#             }
#         }
#     }, event.env = .env, handler.env = .env)
# })

observe({
    print('assigning raw')
    raw = local_data$analysis_data_raw
    
    if( !is.list(raw) ){
        local_data$analysis_data_filtered = NULL
        return()
    }
    
    # raw = list(data = local_data$analysis_data_raw)
    local_data$analysis_data_filtered = raw$data
    
    conditions = unique(raw$data$Condition); if(!length(conditions)){ conditions = '' }
    time_range = range(raw$data$Time, na.rm = TRUE)
    analysis_window = time_range
    confs = dipsaus::drop_nulls(raw$confs)
    groups = list()
    if(length(confs)){
        confs = confs[[1]]
        groups = confs$GROUPS
        analysis_window = sort(c(confs$ANALYSIS_WINDOW, time_range)[1:2])
    }
    
    # store this in local_data so that we have everything in one place
    local_data$analysis_window = analysis_window
    
    dipsaus::updateCompoundInput2(session, 'cond_group', ncomp = max(length(groups), 1), 
                                  initialization = list(
                                      group_conditions = list( choices = conditions )
                                  ), value = groups)
    
    updateSliderInput(session, 'analysis_window', min = time_range[[1]], 
                      max=time_range[[2]], value=analysis_window)
    
    dipsaus::updateCompoundInput2(session, 'multi_window_analysis', 
                                  initialization = list(
                                      analysis_window = list(min = time_range[1], max = time_range[2], value = time_range)
                                  ))
    
})

# observeEvent(input$cond_group, {
#     assign('aaa', session, envir = globalenv())
#     
#     print(input$cond_group)
# })

# Get additional data
observe({
    print('building condition groups')
    
    cond_groups = lapply(1:20, function(jj){input[[paste0('cond_group_group_conditions_', jj)]]})
    cond_groups = dipsaus::drop_nulls(cond_groups)
    conditions = NULL
    
    if( is.list(local_data$analysis_data_raw) ){
        conditions = local_data$analysis_data_raw$data$Condition
    }
    
    if(length(cond_groups) && length(conditions)){
        cols = lapply(cond_groups, function(conds){
            as.numeric(conditions %in% unlist(conds))
        })
        gnames = lapply(seq_along(cols), function(jj){input[[paste0('cond_group_group_name_', jj)]]})
        names(cols) = gnames
        local_data$additional_data = do.call('data.frame', cols)
    }else{
        local_data$additional_data = NULL
    }
})


# the idea here is to add terms while ensuring no duplicates
add_term <- function(x, value) {
    unique(c(x, value))
}

remove_term <- function(x, value) {
    x[x!=value]
}

observeEvent(input$multi_window_analysis, {
    if(sum(sapply(multi_window, '[[', 'window_is_active')) > 0) {
        local_data$var_fixed_effects %<>% add_term('TimeWindow')
    } else {
        local_data$var_fixed_effects %<>% remove_term('TimeWindow')
    }
})



# Combine raw and additional_data, update analysis window range
# observe({
#     raw = local_data$analysis_data_raw
#     # add = local_data$additional_data
#     if( is.list(raw) ){
#         # if(is.data.frame(add)){
#         #     raw = cbind(raw$data, add)
#         # }
#         local_data$analysis_data_filtered = raw$data
#         time_range = range(raw$data$Time, na.rm = TRUE)
#         updateSliderInput(session, 'analysis_window', min = time_range[[1]], max=time_range[[2]],
#                           value=cache_input('analysis_window', time_range))
#     }else{
#         local_data$analysis_data_filtered = NULL
#     }
#     
# })


# build Model 
observe({
    if(!is.data.frame(local_data$analysis_data_filtered)){
        local_data$table_headers = NULL
        local_data$sample_table = NULL
    }else{
        local_data$sample_table = head(local_data$analysis_data_filtered)
        local_data$table_headers = names(local_data$analysis_data_filtered)
    }
})


get_table_headers = function(){
    vars = local_data$table_headers
    if(!length(vars)){ vars = '' }
    vars = vars[!vars %in% c('Project')]
    if( isTRUE(input$model_splinetime) && 'Time' %in% vars ){
        vars[vars == 'Time'] = 'splines::bs(Time)'
    }
    vars
}

observe({
    vars = local_data$table_headers
    
    # we may not know the DVs, but we know some of the IVs
    vars = vars[! vars %in% c('Electrode', 'Time', 'Trial', 'Condition', 'uuid', 'Subject', 'Project', 'TrialIsOutlier')]
    
    updateSelectInput(session, 'model_dependent', choices = vars, selected = vars[1])
})

collect_model = function(..., exclude = NULL){
    re = list()
    re$dependent %?<-% {
        var = input$model_dependent
        var[!var %in% exclude]
    }
    re$fixed %?<-% {
        var = input$model_fixed_effects
        var[!var %in% exclude]
    }
    re$random %?<-% {
        var = input$model_random_effects
        var[!var %in% exclude]
    }
    re
}


# Formula
# observe({
#     # build formula
#     fo = get_formula(input$model_dependent, input$model_fixed_effects, input$model_random_effects, isTRUE(input$model_embedsubject))
#     local_data$fixed = input$model_fixed_effects
#     local_data$rand = input$model_random_effects
#     updateTextInput(session, 'model_formula', value = fo)
# })

# get_formula = function(dv, fe, fr, embed_subject = TRUE){
#     if(!is.data.frame(local_data$analysis_data)){
#         return('')
#     }
#     if(length(fe)){
#         fe = paste(fe, collapse = '+')
#     }else{
#         fe = '1'
#     }
#     fr_valid = sapply(fr, function(v){
#         v = unique(local_data$analysis_data[[v]])
#         v = v[!is.na(v)]
#         length(v) > 1
#     })
#     fr = fr[fr_valid]
#     
#     fr_template = '(1|%s)'
#     if(all(c('Subject', 'Electrode') %in% fr) && embed_subject){
#         fr_add = '(1|Subject/Electrode)'
#         fr = fr[!fr %in% c('Subject', 'Electrode')]
#         fr = paste(c(fr_add, sprintf(fr_template, fr)), collapse = '+')
#     }else{
#         fr = paste(sprintf(fr_template, fr), collapse = '+')
#     }
#     if(fr != ''){
#         fr = paste(' +', fr)
#     }
#     
#     fo = sprintf('%s ~ %s%s', dv,fe,fr)
#     fo    
# }






observeEvent(input$analysis_window, {
    local_data$analysis_window = input$analysis_window
})

observeEvent(input$run_analysis, {
    cond_group <- dipsaus::drop_nulls(lapply(input$cond_group, function(g){
        if(length(g$group_conditions) == 0) return( NULL )
        return(g)
    }))
    if(length(cond_group) < 2) {
        showNotification(p('Must specify at least 2 Groups to run analysis'),
                         duration=5, type='warning', id=ns('noti'))
        
        return()
    }
    
    if(exists('.__DEBUG__')) {
        assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
        assign('..rm_windows', input$multi_window_analysis, envir=globalenv())        
    }
    
    # multi_window = ..rm_windows
    multi_window = input$multi_window_analysis
    # if(length(multi_window))
    if(sum(sapply(multi_window, '[[', 'window_is_active')) == 1) {
        showNotification(p('Must mark at least 2 windows as active to run repeated-measures analysis. Uncheck "Active" to revert to single-window analysis'),
                         duration=5, type='warning', id=ns('noti'))
        
        return()
    }
    
    
    # first we need to collapse the data
    # print(str(local_data$analysis_data_filtered))
    
    # if(exists('.__DEBUG__')) assign('ldf', value = local_data$analysis_data_filtered, envir = globalenv())
    
    all_trial_types <- cond_group %>% lapply(`[[`, 'group_conditions') %>% unlist %>% unique
    
    # make sure all the groups have a name
    for (ii in seq_along(cond_group)) {
        if(!isTRUE(nchar(cond_group[[ii]]$group_name) > 0)) {
            # cond_group[[ii]]$group_name = 'Group_' %&% 
        }
    }
    
    # create a joint variable representing the Group as a factor
    showNotification(p('Fitting mixed effect model. Please wait...'), duration = NULL, type = 'default', id = ns('noti'))
    
    ldf <- local_data$analysis_data_filtered
    subset_data <- subset(ldf, subset = Condition %in% all_trial_types)
    subset_data$Group = cond_group[[1]]$group_name
    for(ii in seq_along(cond_group)[-1]) {
        subset_data$Group[subset_data$Condition %in% cond_group[[ii]]$group_conditions] = cond_group[[ii]]$group_name
    }
    subset_data$Group %<>% factor(levels = sapply(cond_group, `[[`, 'group_name'))
    
    
    print(paste('assigning ', input$model_dependent, ' to y'))
    subset_data$y = subset_data[[input$model_dependent]]
    
    local_data$over_time_data = subset_data
    analysis_window = local_data$analysis_window
    subset_data %<>% subset(Time %within% analysis_window)
    
    collapsed_data <- do_aggregate(y ~ Group + Electrode + Subject, data=subset_data, FUN=mean)
    
    local_data$collapsed_data = collapsed_data
    local_data$agg_over_trial = aggregate(y ~ Group + Time + Subject + Electrode,
                                          local_data$over_time_data, FUN=mean) %>% do_aggregate(y ~ Group + Time, .fast_mse)
    by_el_data = local_data$over_time_data %>%
        subset((.)$Time %within% analysis_window) %>% 
        do_aggregate(y ~ Condition + Trial + Subject + Electrode + Project + Group, mean)
    by_el_data$UUID = by_el_data %$% {Subject %&% Electrode}
    
    by_el_data %>% split((.)$UUID) %>% lapply(function(bed) {
        # bed <- by_el_data %>% split((.)$UUID) %>% extract2(1)
        summ = summary(lsmeans::lsmeans(
                            lm(y ~ Group, data=bed),
                            pairwise ~ Group),
                       infer=TRUE)
        
        a = summ$lsmeans[c('Group', 'lsmean', 't.ratio', 'p.value')] %$% {
            c(rbind(lsmean, t.ratio, p.value))
        } %>% set_names(
            c(outer(c('m(', 't(', 'p('), levels(bed$Group), paste0)) %&% ')'
        )
        
        b = summ$contrasts[c('contrast', 'estimate', 't.ratio', 'p.value')] %$% {
            c(rbind(estimate, t.ratio, p.value))
        } %>% set_names(
            c(outer(c('m(', 't(', 'p('), summ$contrasts$contrast, paste0)) %&% ')'
        )

        res <- data.frame(Project = bed$Project[1], Subject = bed$Subject[1], 
                          Electrode = bed$Electrode[1])
        res[names(a)] = a
        res[names(b)] = b
        res
    })  %>% rbind_list %>% magrittr::set_rownames(NULL) -> by_el_results
    
    
    round_pvals <- function(pval){
        lpval = pmax(-16, log10(pval))
        
        ifelse(lpval > -3.5,
               formatC(round(pval,4),width = 4, digits=4),
               paste0('1e', formatC(round(lpval), width=3,flag=0)))
    }
    
    by_el_results[names(by_el_results) %>% startsWith('p(')] %<>% lapply(round_pvals)
    by_el_results[names(by_el_results) %>% startsWith('t(')] %<>% lapply(round, 2)
    by_el_results[names(by_el_results) %>% startsWith('m(')] %<>% lapply(round, 2)
    by_el_results$Electrode <-  as.numeric(as.character(by_el_results$Electrode))
    local_data$by_electrode_results = by_el_results
    
    fo <-  str_replace_all(input$model_formula, input$model_dependent, 'y')
    
    fo %<>% as.formula #fo=as.formula('y ~ Group + (1|Subject/Electrode)')
    tryCatch({
        print('trying LME')
        lmer_results = lmerTest::lmer(fo, data=collapsed_data, na.action=na.omit)
        print('LME succeeded')
        local_data$lmer_results_summary <- summary(lmer_results)
        print('summary succeeded')
        
        local_data$lmer_results = lmer_results
        
        if(exists('.__DEBUG__'))
            assign('..local_data', value = shiny::isolate(shiny:::reactiveValuesToList(local_data)), envir = globalenv())
        
        showNotification(p('Model finished!'), duration = 3, type = 'default', id = ns('noti'))
    }, error = function(e){
        print(e)
        if(is.list(e)){
            msg = e$message; msg %?<-% ''
            cal = e$call; cal %?<-% ''
            e = sprintf('%s in %s', msg, cal)
        }
        # grouping factors must have > 1 sampled level
        showNotification(p(e), duration = 20, type = 'error', id = ns('noti'))
        local_data$lmer_results = NULL
    })
})


# update_model_formula <- function() {
#     updateTextInput(session, 'model_formula', value=new_formula)
# }

observeEvent(input$model_dependent, {
    if(is.null(local_data$analysis_data_filtered)) return(1)
    
    orig = input$model_formula
    # if(!str_detect())
    
    # just update the LHS
    tokens = unlist(stringr::str_split(orig, '~'))
    
    re = '(1|Electrode)'
    if(length(unique(local_data$analysis_data_filtered$Subject)) > 1) {
        re = '(1|Subject/Electrode)'
    }
    
    new_formula = sprintf("%s ~ Group + %s", input$model_dependent, re)
    
    updateTextInput(session, 'model_formula', value=new_formula)
})

#### Feature selection field handlers ####

# UI for filters
# var_sel = function(){
#     if(!is.data.frame(local_data$analysis_data)){
#         return(span(style = 'color: #a1a1a1', 'Analysis table not loaded'))
#     }
#     n_filters = local_filters$filter_count
#     vars = local_data$analysis_data; vars %?<-% ''
#     
#     filter_uis = NULL
#     minus_btn = NULL
#     
#     if(n_filters > 0){
#         minus_btn = actionButton(ns('filter_minus'), shiny::icon('minus'))
#         filter_uis = lapply( seq_len(n_filters), function(ii){ get_ui( ii , vars ) } )
#     }
#     
#     tagList(
#         filter_uis,
#         div(
#             # Put a div to make buttons within a row
#             actionButton(ns('filter_add'), shiny::icon('plus')),
#             minus_btn
#         ),
#         actionLink(ns('view_filtered'), 'Preview filtered data')
#     )
# }
# 
# observeEvent(input$view_filtered, {
#     # Collect data
#     shiny::showModal(shiny::modalDialog(
#         title = 'Preview input data', size = 'l', easyClose = TRUE, fade = FALSE,
#         tags$style('.modal-lg { min-width: 80vw; }'),
#         DT::dataTableOutput(ns('view_filtered_tbl'))
#     ))
# })
# output$view_filtered_tbl <- DT::renderDataTable({
#     shiny::validate(shiny::need(is.data.frame(local_data$analysis_data), message = 'No analysis loaded'))
#     sel = filter_summary()
#     local_data$analysis_data[sel,]
# })
# 
# get_ui = function(ii, vars = ''){
#     filter = shiny::isolate(local_filters[[paste0('filter', ii)]])
#     if(!is.list(filter)){ filter = list() }
#     tagList(
#         tagList(
#             tags$label(sprintf('Filter %d', ii), style = ifelse(ii == 1, '', 'margin-top: 15px;')),
#             div(
#                 # To make a box to wrap group inputs
#                 class = 'rave-grid-inputs',
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     selectInput(ns('filter_var_' %&% ii), 'Variable', choices = vars, selected = get_val(filter, 'var', default = NULL))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     selectInput(ns('filter_op_' %&% ii), 'Operator', choices = c('=', '!=', '>', '>=', '<', '<=', 'in', 'not in', 'between'), selected = get_val(filter, 'op', default = '='))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     textInput(ns('filter_val_' %&% ii), 'Value', value = get_val(filter, 'val', default = NULL))
#                 ),
#                 div(
#                     style = 'flex-basis: 25%; min-height: 80px;',
#                     uiOutput(ns('filter_msg_' %&% ii))
#                 )
#             )
#             
#         )
#     )
# }
# 
# get_operator = function(op){
#     switch (op,
#             '=' = '%s == %s',
#             'in' = '%s %%in%% %s',
#             'between' = '%s %%within%% %s',
#             'not in' = '!%s %%in%% %s',
#             {
#                 paste('%s', op, '%s')
#             }
#     )
# }
# 
# filter_data = function(dat, op, val){
#     if( is.numeric(dat) && is.character(val) ){
#         val = parse_svec(val, sort = FALSE, unique = FALSE)
#     }
#     expr = get_operator(op)
#     expr = sprintf(expr, 'dat', deparse(val))
#     sel = rlang::eval_tidy(rlang::parse_expr(expr), data = list(dat = dat))
#     sel
# }
# 
# get_filter_results = function(ii){
#     filter = local_filters[[paste0('filter', ii)]]
#     if(!is.data.frame(local_data$analysis_data) || !is.list(filter) || !isFALSE(filter$failed)){ return(NULL) }
#     var = filter$var; op = filter$op; val = filter$val
#     dat = local_data$analysis_data[[var]]
#     if( is.numeric(dat) ){
#         val = parse_svec(val)
#     }
#     sel = filter_data(dat, op, val)
#     sel[is.na(sel)] = FALSE
#     sel
# }
# add_filter_observer = function(ii){
#     
#     
#     local({
#         observe({
#             n_filters = local_filters$filter_count
#             if(!is.data.frame(local_data$analysis_data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
#             var = input[[sprintf('filter_var_%d', ii)]]; op = input[[sprintf('filter_op_%d', ii)]]; val = input[[sprintf('filter_val_%d', ii)]]
#             var %?<-% ''; op %?<-% '='; val %?<-% ''
#             val_txt = val
#             # Do checks
#             msg = ''
#             failed = FALSE
#             if( !var %in% shiny::isolate(local_data$all_vars) ){
#                 msg = 'Variable not found'
#                 failed = TRUE
#             }else{
#                 dat = shiny::isolate({ local_data$analysis_data[[var]] })
#                 if( is.numeric(dat) ){
#                     val = parse_svec(val)
#                     if( !length(val) || any(is.na(val)) ){
#                         msg = 'Value is blank or contains NA'
#                         failed = TRUE
#                     }
#                 }
#                 if( !failed ){
#                     sel = filter_data(dat, op, val)
#                     n_na = sum(is.na(dat[sel]))
#                     n_sel = sum(sel, na.rm = TRUE)
#                     msg = sprintf('%d of %d selected (%d NAs)', n_sel, length(sel), n_na)
#                     if(n_sel == 0){
#                         msg = 'No data selected'
#                         failed = TRUE
#                     }
#                 }
#             }
#             
#             re = list(
#                 var = var, op = op, val = val_txt, failed = failed, msg = msg
#             )
#             local_filters[[paste0('filter', ii)]] = re
#         })
#         
#         output[[sprintf('filter_msg_%d', ii)]] = shiny::renderUI({
#             n_filters = shiny::isolate(local_filters$filter_count)
#             if(!is.data.frame(local_data$analysis_data) || !length(n_filters) || n_filters < ii ){ return(NULL) }
#             
#             filter = local_filters[[paste0('filter', ii)]]
#             if(!is.list(filter)){ return() }
#             
#             col = ifelse( isTRUE(filter$failed) , 'red', 'grey' )
#             filter$msg %?<-% ''
#             htmltools::span(style = col2hex(col, prefix = 'color:#'), filter$msg)
#         })
#     })
#     
# }
# 
# # Add/remove filters
# observeEvent(input$filter_add, {
#     n_filters = shiny::isolate(local_filters$filter_count) + 1
#     n_observers = shiny::isolate(local_filters$filter_observers)
#     local_filters$filter_count = n_filters
#     # Check if observers are needed
#     if( n_filters > n_observers ){
#         add_filter_observer( n_filters )
#         local_filters$filter_observers = n_filters
#     }
# })
# observeEvent(input$filter_minus, {
#     n_filters = shiny::isolate(local_filters$filter_count) - 1
#     local_filters$filter_count = max(n_filters, 0)
# })
# 
# # summarise filters
# filter_summary = function(){
#     n_filters = shiny::isolate(local_filters$filter_count)
#     nrows = shiny::isolate({
#         re = 0
#         if(is.data.frame(local_data$analysis_data)){
#             re = nrow(local_data$analysis_data)
#         }
#         re
#     })
#     filters = shiny::isolate({
#         res = rep(TRUE, nrows)
#         for(ii in seq_len(n_filters)){
#             fil = get_filter_results( ii )
#             res = res & fil
#         }
#         res
#     })
#     filters
# }
# 
# observeEvent(local_filters$filter_count,{
#     print(sum(filter_summary()))
# })
