env = new.env(parent = emptyenv())
session = getDefaultReactiveDomain()
input = getDefaultReactiveInput()
output = getDefaultReactiveOutput()

local_data = shiny::reactiveValues(
    group_number = NULL,
    refresh = NULL,
    do_parallel_plot = NULL,
    load_mesh = TRUE
)


ref_group %?<-% list()
# Environment to store information
env = new.env(parent = baseenv())
env$ref_calc = 0
env$last_import = 'new..'



observeEvent(input[['bipolar_modal']], {
    # get group info
    group_info = current_group()
    if(!length(group_info)){
        return()
    }

    ref_tbl = get_ref_table()
    electrodes = group_info$electrodes
    bptbl = ref_tbl[ref_tbl$Electrode %in% electrodes,]
    bptbl$Type = 'Bipolar Reference'

    if(nrow(bptbl) > 0 && unique(bptbl$Reference) == 'noref'){
        e = bptbl$Electrode
        bptbl$Reference = c(paste0('ref_', e[-1]), '')
    }
    env$bipolar_tbl = bptbl

    showModal(
        shiny::modalDialog(
            title = 'Bipolar Reference',
            size = 'l',
            easyClose = F,
            footer = tagList(
                actionButton(ns('bp_confirm'), 'Confirm')
            ),
            DT::DTOutput(ns('bipolar_table'))
        )
    )
})

observeEvent(input[[('bp_confirm')]], {
    tbl = env$bipolar_tbl
    ref_tbl = get_ref_table()
    if(nrow(tbl)){
        for(ii in seq_len(nrow(tbl))){
            sel = ref_tbl$Electrode == tbl$Electrode[ii]
            ref_tbl$Reference[sel] = tbl$Reference[ii]
            ref_tbl$Type[sel] = 'Bipolar Reference'
        }
        # save ref_tbl
        save_ref_table(ref_tbl)
    }
    removeModal(session = session)
})

bipolar_proxy = DT::dataTableProxy('bipolar_table', session = session)

output[[('bipolar_table')]] = DT::renderDT({
    env$bipolar_tbl
}, env = ..runtime_env, editable = TRUE)

observeEvent(input[[('bipolar_table_cell_edit')]], {
    info = input[[('bipolar_table_cell_edit')]]
    i = info$row
    j = info$col
    v = info$value

    # string match electrode
    v = str_match(v, '(ref_|[\\ ]{0})([0-9]*)')[3]
    if(!length(v) || is.na(v) || isTRUE(v=='')){
        v = ''
    }else{
        v = subject$filter_all_electrodes(as.integer(v))
        if(!length(v)){
            return()
        }else{
            v = str_c('ref_', v)
        }
    }


    bipolar_tbl = env$bipolar_tbl

    if(names(bipolar_tbl)[j] == 'Reference'){
        env$bipolar_tbl[i, j] = v
        DT::replaceData(bipolar_proxy, env$bipolar_tbl, resetPaging = FALSE)  # important
    }
})


output[['elec_loc']] <- threeBrain::renderBrain({
    
    local_data$refresh
    brain = local_data$brain
    
    if(is.null(brain)){
        return()
    }
    try({
        group_info = current_group() 
        group_info %?<-% list(electrodes = NULL)
        name = group_info$rg_name
        ref_tbl = get_ref_table()
        if(!length(name) || is.blank(name)){ name = 'Current Group' }
        
        # join electrodes.csv with ref table
        tbl = merge(ref_tbl, subject$electrodes[,c('Electrode', 'Coord_x','Coord_y','Coord_z', 'Label')],
                    id = 'Electrode', suffixes = c('.x', ''))
        tbl$Label[is.na(tbl$Label)] = 'No Label'
        
        electrodes = group_info$electrodes
        sapply(electrodes, function(e){
            sel = tbl$Electrode == e
            Group = tbl$Group[sel]
            Type = tbl$Type[sel]
            Reference = tbl$Reference[sel]
            sprintf('Group - %s (%s)Reference to - %s', Group, Type, Reference)
        }) ->
            marker
        
        lev = factor(c('Curr Group', 'Bad El'), levels = c('Curr Group', 'Bad El'))
        values = rep(lev[1], length(electrodes))
        bad_electrodes = dipsaus:::parse_svec(input[[('ref_bad')]])
        values[electrodes %in% bad_electrodes] = lev[2]
        
        
        # brain = rave_brain2(surfaces = 'pial', multiple_subject = F)
        # brain$load_electrodes(subject)
        
        # make a table
        tbl = data.frame(
            Electrode = electrodes,
            ElectrodeType = values,
            Note = marker
        )
        if(!nrow(tbl)){ return() }
        brain$set_electrode_values( tbl )
        bg = ifelse('dark' %in% rave::get_rave_theme()$theme, '#1E1E1E', '#FFFFFF')
        
        if( isTRUE(local_data$load_mesh) ){
            brain$plot(
                volumes = FALSE, surfaces = TRUE, side_canvas = FALSE, 
                background = bg,
                control_panel = FALSE, palettes = list(
                    'ElectrodeType' = c('navy', 'red')
                ), side_display = FALSE, control_display = FALSE, cex = 0.5)
        }else{
            # Maybe load N27 brain if not exists
            brain$plot(
                volumes = FALSE, surfaces = FALSE, side_canvas = FALSE, 
                background = bg,
                control_panel = FALSE, palettes = list(
                    'ElectrodeType' = c('navy', 'red')
                ), side_display = FALSE, control_display = FALSE, cex = 0.5)
        }
        
        # brain$view(value_range = c(-1,1), control_panel = F)
    })
    
})

observeEvent(input$load_mesh, {
    load_mesh = isolate(!isTRUE(local_data$load_mesh))
    local_data$load_mesh = load_mesh
    updateActionButton(session, 'load_mesh', label = ifelse(load_mesh, 'Hide Mesh', 'Show Mesh'))
})


elec_loc_ui = function(){
    if(!length(local_data$brain)){
        div(tags$small('[Cannot find surface files. Hide viewer.]'), style='color:#E2E2E2')
    }else{
        div(
            actionLink(ns('load_mesh'), 'Hide Mesh'),
            threeBrain::threejsBrainOutput(ns('elec_loc'), height = '300px')
            # threejsr::threejsOutput(ns('elec_loc'), height = '300px')
        )
    }
    

}

observeEvent(input[[('cur_save')]], {
    ref_to = input[[('ref_to')]]
    group_info = current_group()
    if(is.null(group_info)){
        return()
    }
    electrodes = group_info$electrodes
    bad_electrodes = dipsaus:::parse_svec(input[[('ref_bad')]])

    ref_table = get_ref_table()
    sel = ref_table$Electrode %in% electrodes


    ref_table$Group[sel] = group_info$rg_name
    if(group_info$rg_type %in% c('Common Average Reference', 'White Matter Reference', 'No Reference')){
        ref_table$Reference[sel] = ref_to
        ref_table$Reference[sel & ref_table$Electrode %in% bad_electrodes] = '' # set bad electrodes
        ref_table$Type[sel] = group_info$rg_type
        save_ref_table(tbl = ref_table)
        showNotification(p(
            group_info$rg_name, ' (', group_info$rg_electrodes, ') is now set to be referenced to [', ref_to, ']'
        ), type = 'message')
    }
}, priority = -1L)


observeEvent(input$ref_group, {
    n_groups <- max(length(input$ref_group), 1)
    
    shiny::updateSelectInput(session, 'cur_group', choices = as.character(seq_len(n_groups)), 
                             selected = min(shiny::isolate(input$cur_group), n_groups))
})

# Customized UI
cur_group_ui = function(){
    refresh = local_data$refresh
    # dipsaus::cat2('cur_group_ui')
    new_ref = local_data$has_new_ref
    cur_group <- as.integer(cur_group)

    if(isTRUE(cur_group <= length(ref_group))){
        group_number = as.integer(cur_group)
        group_info = ref_group[[group_number]]
        group_type = group_info$rg_type
        group_name = group_info$rg_name
        electrodes = dipsaus:::parse_svec(group_info$rg_electrodes)
        if(length(electrodes) == 0){
            return(tagList(
                hr(),
                actionButton(ns('cur_group_save'), 'Preview & Export',width = '100%')
            ))
        }
    }else{
        return(tagList(
            hr(),
            actionButton(ns('cur_group_save'), 'Preview & Export',width = '100%')
        ))
    }

    refs = get_refs()
    ref_names = names(refs); ref_names = c('noref', ref_names)

    ref_tbl = get_ref_table()

    sel = ref_tbl$Electrode %in% electrodes


    switch (
        group_type,
        'No Reference' = {
            selectInput(ns('ref_to'), 'Reference to:', choices = 'noref', selected = 'noref')
        },
        'Bipolar Reference' = {
            tagList(
                tags$label('Reference to:'),
                actionButton(ns('bipolar_modal'), 'Open Table', width = '100%', style = 'margin-bottom: 15px')
            )
        },
        # By default, it's either 'Common Average Reference' or 'White Matter Reference'
        {
            # try to get reference name
            selected = unique(c(ref_tbl$Reference[sel]), 'noref')
            selected = selected[selected != ''][1]
            selectInput(ns('ref_to'), 'Reference to:', choices = ref_names, selected = selected)
        }

    ) ->
        inp




    tagList(
        fluidRow(
            column(
                width = 7,
                inp,
                p(
                    tags$label('Group Name: '), group_name, br(),
                    tags$label('Electrodes: '), group_info$rg_electrodes, br(),
                    tags$label('Bad Electrodes: '), textOutput(ns('bad_electrodes_out'), inline = T)
                )
            ),
            column(
                width = 5,
                textInput(ns('ref_bad'), 'Bad Electrodes:', value = dipsaus:::deparse_svec(ref_tbl$Electrode[sel & ref_tbl$Reference == ''])),
                div(
                    style = 'float: right',
                    actionButton(ns('cur_save'), 'Save Group')
                )
            )
        ),
        hr(),
        actionButton(ns('cur_group_save'), 'Preview & Export',width = '100%')
    )
}

# observeEvent(input[['elec_loc_callback']], {
#     dat = input[['elec_loc_callback']]
#     print(dat)
#     do.call(switch_to, dat)
# })

output[[('bad_electrodes_out')]] = renderText({
    bad_electrodes = dipsaus:::parse_svec(input[[('ref_bad')]])
    bad_electrodes = subject$filter_all_electrodes(bad_electrodes)
    if(length(bad_electrodes)){
        bad_electrodes = dipsaus:::deparse_svec(bad_electrodes)
        bad_electrodes
    }else{
        'No bad electrode'
    }
}, env = ..runtime_env)

# Utils
current_group = function(){
    group_number = as.integer(cur_group)
    if(!length(group_number) || group_number > length(ref_group)){
        return()
    }
    group_info = ref_group[[group_number]]
    electrodes = dipsaus:::parse_svec(group_info$rg_electrodes)
    electrodes = subject$filter_all_electrodes(electrodes)
    if(!length(electrodes)){
        return()
    }
    group_info$electrodes = electrodes




    return(group_info)
}
get_ref_table = function(){
    ref_info = cache(key = list(
        ref_name_alt = ref_name_alt,
        subject = subject$id
    ), import_external(), name = 'ref_info')
    ref_table = ref_info$table
    ref_table
}

save_ref_table = function(tbl, is_new = FALSE){
    print('Saving')
    val = list(
        table = tbl,
        new = is_new
    )
    old = cache(key = list(
        ref_name_alt = ref_name_alt,
        subject = subject$id
    ), val, name = 'ref_info', replace = TRUE)
    if(nrow(old$table) != nrow(tbl)){
        stop("Refernce table doesn't match")
    }
    local_data$ref_tbl = tbl
    invisible()
}

import_external = function(){
    dirs = module_tools$get_subject_dirs()
    ref_name_alt %?<-% sprintf('reference_%s.csv', preload_info$reference_name)
    f = file.path(dirs$meta_dir, ref_name_alt)
    if(file.exists(f)){
        tbl = read.csv(f, stringsAsFactors = F)
        if(!'Type' %in% names(tbl)){
            tbl$Type = 'No Reference'
        }else{
            tbl$Type[!tbl$Type %in% c('Common Average Reference', 'Bipolar Reference', 'White Matter Reference', 'No Reference')] = 'No Reference'
        }
        tbl = tbl[,c('Electrode', 'Group', 'Reference', 'Type')]
        is_new = T
    }else{
        tbl = data.frame(
            Electrode = subject$preprocess_info('channels'),
            Group = '',
            Reference = 'noref',
            Type = 'No Reference',
            stringsAsFactors = F
        )
        is_new = F
    }
    local_data$ref_tbl = tbl
    list(
        table = tbl,
        new = is_new
    )
}
load_reference = function(){
    dirs = module_tools$get_subject_dirs()
    ref_name_alt %?<-% sprintf('reference_%s.csv', preload_info$reference_name)
    # Get current settings
    key = list(
        ref_name_alt = ref_name_alt,
        subject = subject$id
    )

    ref_info = cache(key = key, import_external(), name = 'ref_info')
    ref_tbl = (ref_info$table)

    if(is.null(ref_tbl)){
        return()
    }

    if(env$last_import != ref_name_alt){
        env$last_import = ref_name_alt
        ref_info$new = TRUE
    }else{
        ref_info$new = FALSE
    }

    # If ref_info$new, update compound input ref_group s.t. it matches with current settings, else replace and cache ref_info
    if(ref_info$new){
        ref_info$new = FALSE
        unique_refs = ref_tbl[!duplicated(ref_tbl[,c('Group', 'Type')]), ]
        nn = nrow(unique_refs)
        if(nn > 0){
            lapply(seq_len(nn), function(i){
                # Group i
                row = unique_refs[i, ]
                # name
                updateTextInput(session, (sprintf('%s_%s_%d', 'ref_group', 'rg_name', i)), value = row$Group)
                # ref Method
                updateSelectInput(session, (sprintf('%s_%s_%d', 'ref_group', 'rg_type', i)), selected = row$Type)
                # Electrodes
                merged = merge(ref_tbl, row, by = c('Group', 'Type'), suffixes = c('', 'y'))
                updateTextInput(
                    session,
                    (sprintf('%s_%s_%d', 'ref_group', 'rg_electrodes', i)),
                    value = dipsaus:::deparse_svec(merged$Electrode)
                )

                dipsaus::updateCompoundInput2(
                    session, inputId = 'ref_group', ncomp = nn
                )
            })

        }

    }else{
        # Construct table
        all_es = NULL
        for(ii in seq_len(length(ref_group))){
            sub_group = ref_group[[ii]]
            sub_es = sub_group$rg_electrodes
            sub_es = dipsaus:::parse_svec(sub_es)
            if(any(sub_es %in% all_es)){
                dup_es = sub_es[sub_es %in% all_es]
                showNotification(
                    p('Group [', sub_group$rg_name, '(', ii, ')] has duplicated electrode(s): ', dipsaus:::deparse_svec(dup_es)),
                    type = 'warning'
                )
            }
            all_es = c(all_es, sub_es)
            sub_sel = ref_tbl$Electrode %in% sub_es
            if(any(sub_sel)){

                ref_tbl$Group[sub_sel] = sub_group$rg_name
                ref_tbl$Type[sub_sel] = sub_group$rg_type
            }
        }

        ref_info$table = ref_tbl
    }

    cache(key = key, val = ref_info, name = 'ref_info', replace = TRUE)
}

gen_reference_blockwise = function(blockwise_table){
    dirs = module_tools$get_subject_dirs()

    blocks = blockwise_table$Block
    refs = blockwise_table$Reference

    involved_es = dipsaus:::parse_svec(refs)
    if(length(involved_es) == 0){
        showNotification(p('No electrodes used. Why not use "noref"?'), type = 'error', session = session)
        return(FALSE)
    }
    fname = 'ref_0,' %&% dipsaus:::deparse_svec(involved_es) %&% '.h5'

    f = file.path(dirs$channel_dir, 'reference', fname)
    unlink(f)

    subprogress = rave::progress('Loading Data', max = length(involved_es))
    progress = rave::progress(sprintf('Generating reference [%s]', fname), max = length(blocks)+1)

    ref_data = new.env()

    for(ii in seq_along(blocks)){
        b = blocks[[ii]]
        subprogress$reset()
        progress$inc('Loading data from block ' %&% b)
        es = dipsaus:::parse_svec(refs[ii])

        ref_data[[b]] = new.env()
        ref_data[[b]][['volt']] = 0
        ref_data[[b]][['coef']] = 0

        lapply(es, function(e){
            subprogress$inc('Loading electrode ' %&% e)
            # load channel
            power = load_h5(file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)), name = '/raw/power/' %&% b)[]
            phase = load_h5(file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', e)), name = '/raw/phase/' %&% b)[]
            volt = load_h5(file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)), name = '/raw/voltage/' %&% b)[]

            ref_data[[b]][['volt']] = ref_data[[b]][['volt']] + volt
            ref_data[[b]][['coef']] = ref_data[[b]][['coef']] + sqrt(power) * exp(1i * phase)

        })
        if(length(es)){
            ref_data[[b]][['volt']] = ref_data[[b]][['volt']] / length(es)
            ref_data[[b]][['coef']] = ref_data[[b]][['coef']] / length(es)
        }else{
            e = involved_es[1]
            volt = load_h5(file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)), name = '/raw/voltage/' %&% b)
            power = load_h5(file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)), name = '/raw/power/' %&% b)

            ref_data[[b]][['volt']] = rep(0, length(volt))
            ref_data[[b]][['coef']] = matrix(0, nrow = dim(power)[1], ncol = dim(power)[2])
        }

    }


    progress$inc('Saving to disk...')
    # Average
    for(b in blocks){
        volt = ref_data[[b]][['volt']]
        coef = ref_data[[b]][['coef']]
        coef = array(c(Mod(coef), Arg(coef)), dim = c(dim(coef), 2)) # Freq x Time x 2
        save_h5(volt, file = f, name = sprintf('/voltage/%s', b), chunk = 1024, replace = T)
        save_h5(coef, file = f, name = sprintf('/wavelet/coef/%s', b), chunk = c(dim(coef)[1], 128, 2), replace = T)
    }

    progress$close()
    subprogress$close()
    removeModal()

    showNotification(p('Reference [', fname, '] exported.'), type = 'message')
    local_data$has_new_ref = Sys.time()

}

gen_reference = function(electrodes){
    electrodes = subject$filter_all_electrodes(electrodes)
    if(length(electrodes) == 0){
        return()
    }
    dirs = module_tools$get_subject_dirs()
    fname_h5 = sprintf('ref_%s.h5', dipsaus:::deparse_svec(electrodes))
    fname_fst = sprintf('ref_%s.fst', dipsaus:::deparse_svec(electrodes))
    f = file.path(dirs$channel_dir, 'reference', fname_h5)
    # generate reference
    # Step 0: chunk matrix
    ncores = rave_options('max_worker')
    ncols = ceiling(length(electrodes) / ncores)
    nes = length(electrodes)
    mat = matrix(NA, nrow = ncores, ncol = ncols)
    mat[seq_along(electrodes)] = electrodes

    # Summing up
    env$gen_volt = list()
    env$gen_coef = list()


    progress = rave::progress(sprintf('Generating reference [%s]', fname_h5), max = length(electrodes)+3)
    progress$inc('Initializing...')
    on.exit(progress$close())

    blocks = subject$preprocess_info('blocks')


    lapply(seq_len(ncols), function(ii){
        es = mat[, ii]
        es = es[!is.na(es)]

        lapply_async(es, function(e){
            root_dir = dirs$channel_dir
            fname_h5 = sprintf('%d.h5', e)
            fname_fst = sprintf('%d.fst', e)
            sapply(blocks, function(b){
                fst_file = file.path(root_dir, 'cache', 'power', 'raw', b, fname_fst)
                if(file.exists(fst_file)){
                    coef = fst::read_fst(fst_file)
                    coef = t(sqrt(as.matrix(coef)))
                }else{
                    coef = sqrt(load_h5(file.path(root_dir, 'power', fname_h5), name = sprintf('/raw/power/%s', b))[])
                }


                fst_file = file.path(root_dir, 'cache', 'phase', 'raw', b, fname_fst)
                if(file.exists(fst_file)){
                    phase = fst::read_fst(fst_file)
                    phase = exp(1i * t((as.matrix(phase))))
                }else{
                    phase = exp(1i * load_h5(file.path(root_dir, 'phase', fname_h5), name = sprintf('/raw/phase/%s', b))[])
                }

                fst_file = file.path(root_dir, 'cache', 'voltage', 'raw', b, fname_fst)
                if(file.exists(fst_file)){
                    volt = fst::read_fst(fst_file)[,1]
                }else{
                    volt = load_h5(file.path(root_dir, 'voltage', fname_h5), name = sprintf('/raw/voltage/%s', b))[]
                }


                list(
                    volt = volt,
                    coef = coef * phase
                )
            }, USE.NAMES = TRUE, simplify = FALSE) ->
                re
            gc()
            return(re)
        }, .call_back = function(i){
            progress$inc(sprintf('Loading electrode %d', es[[i]]))
        }) ->
            re


        gc()
        lapply(re, function(dat){
            for(b in blocks){
                if(length(env$gen_volt[[b]])){
                    env$gen_volt[[b]] = env$gen_volt[[b]] + dat[[b]][['volt']]
                    env$gen_coef[[b]] = env$gen_coef[[b]] + dat[[b]][['coef']]
                }else{
                    env$gen_volt[[b]] = dat[[b]][['volt']]
                    env$gen_coef[[b]] = dat[[b]][['coef']]
                }
            }
            NULL
        })
    })

    progress$inc('Saving to disk.')

    ref_dir = file.path(dirs$channel_dir, 'cache', 'reference')

    # Average
    for(b in blocks){
        volt = env$gen_volt[[b]] / nes
        coef = env$gen_coef[[b]] / nes
        coef = array(c(Mod(coef), Arg(coef)), dim = c(dim(coef), 2)) # Freq x Time x 2
        save_h5(volt, file = f, name = sprintf('/voltage/%s', b), chunk = 1024, replace = T)
        save_h5(coef, file = f, name = sprintf('/wavelet/coef/%s', b), chunk = c(dim(coef)[1], 128, 2), replace = T)

        # fast_cache
        fast_cache = rave_options('fast_cache'); fast_cache %?<-% TRUE
        fst_coef = file.path(ref_dir, 'coef', b)
        fst_phase = file.path(ref_dir, 'phase', b)
        fst_volt = file.path(ref_dir, 'voltage', b)
        dir.create(fst_coef, recursive = T, showWarnings = F)
        dir.create(fst_phase, recursive = T, showWarnings = F)
        dir.create(fst_volt, recursive = T, showWarnings = F)
        if(fast_cache){
            # fast cache referenced signals
            dat = as.data.frame(t(coef[,,1]))
            fst::write_fst(dat, file.path(fst_coef, fname_fst), compress = 100)

            dat = as.data.frame(t(coef[,,2]))
            fst::write_fst(dat, file.path(fst_phase, fname_fst), compress = 100)

            dat = data.frame(V1 = volt)
            fst::write_fst(dat, file.path(fst_volt, fname_fst), compress = 100)
        }
    }

    showNotification(p('Reference [', fname_h5, '] exported.'), type = 'message')
    local_data$has_new_ref = Sys.time()
}

get_refs = function(){
    dirs = module_tools$get_subject_dirs()
    refs = list.files(file.path(dirs$channel_dir, 'reference'), pattern = '^ref_.*\\.h5$')
    if(!length(refs)){
        return(list())
    }
    es = str_split_fixed(refs, '(ref_)|(\\.h5)', n = 3)[,2]
    re = lapply(es, dipsaus:::parse_svec)
    names(re) = 'ref_' %&% es
    re
}


output[[('export_table')]] <- DT::renderDT({
    if(is.data.frame(local_data$ref_tbl)){
        local_data$ref_tbl
    }
}, env = ..runtime_env)

observe({
    val = input[[('ref_export_name')]]
    val %?<-% 'default'
    val = str_replace_all(val, '\\W', '')
    val = str_to_lower(val)
    val = 'Reference Table Name: (reference_' %&% val %&% '.csv)'
    updateTextInput(session, 'ref_export_name', label = val)
})

write_ref_table = function(){
    # get ref_table
    ref_tbl = get_ref_table()
    dirs = subject$dirs
    fname = input[[('ref_export_name')]]
    fname %?<-% 'default'
    fname = str_replace_all(fname, '\\W', '')
    fname = str_to_lower(fname)
    fname = 'reference_' %&% fname %&% '.csv'
    fpath = file.path(dirs$meta_dir, fname)
    rave:::safe_write_csv(data = ref_tbl, file = fpath, row.names = F)
    # write to preprocess that subject is already refrenced
    utils = rave_preprocess_tools()
    utils$load_subject(subject_code = subject$subject_code, project_name = subject$project_name)
    utils$save_to_subject(checklevel = 4) # 4 means referenced
    # switch_to('condition_explorer')
    return(fname)
}


load_refchan = function(r, subject_channel_dir, blocks, ram = T){
    es = stringr::str_extract(r, '[0-9,\\-]+')
    es = dipsaus:::parse_svec(es)

    ref_file = file.path(subject_channel_dir, 'reference', sprintf('%s.h5', r))
    if(!file.exists(ref_file)){
        if(length(es) == 1){
            volt = sapply(blocks, function(b){
                load_h5(file.path(subject_channel_dir, 'voltage', sprintf('%d.h5', es)), '/raw/voltage/' %&% b, ram = ram)
            }, simplify = F, USE.NAMES = T)

            coef = sapply(blocks, function(b){
                power = load_h5(file.path(subject_channel_dir, 'power', sprintf('%d.h5', es)), name = '/raw/power/' %&% b, ram = ram)
                phase = load_h5(file.path(subject_channel_dir, 'phase', sprintf('%d.h5', es)), name = '/raw/phase/' %&% b, ram = ram)
                list(
                    power = power,
                    phase = phase
                )
            }, simplify = F, USE.NAMES = T)

        }else{
            stop('Reference ', r, ' does not exist.')
        }
    }else{
        volt = sapply(blocks, function(b){
            load_h5(ref_file, '/voltage/' %&% b, ram = ram)
        }, simplify = F, USE.NAMES = T)
        coef = sapply(blocks, function(b){
            load_h5(ref_file, '/wavelet/coef/' %&% b, ram = ram)
        }, simplify = F, USE.NAMES = T)
    }
    return(list(volt = volt, coef = coef))
}

observeEvent(input$do_export_cache, {
    fname = write_ref_table()
    showNotification(p('Reference table [', fname, '] exported. Creating cache referenced data.'), type = 'message', id = ns('ref_export_cache_notification'))
    # Start cache
    ref_tbl = get_ref_table()
    electrodes = ref_tbl$Electrode
    blocks = subject$preprocess_info('blocks')
    subject_channel_dir = subject$dirs$channel_dir

    # Step 1 get all the references
    ref = table(ref_tbl$Reference)
    ref = ref[!names(ref) %in% c('', 'noref')]

    progress = progress(title = 'Create cache for electrodes', max = length(electrodes) + length(ref))

    if(length(ref)){
        ram = as.list(ref > 1)
        lapply_async(names(ram), function(r){
            load_refchan(r = r, subject_channel_dir = subject_channel_dir, blocks = blocks, ram = ram[[r]])
        }, .call_back = function(ii){
            progress$inc(sprintf('Loading reference - [%s]', names(ram)[ii]))
        }) ->
            refs
        names(refs) = names(ram)
    }else{
        refs = list()
    }

    ref_names = names(ref)

    for(b in blocks){
        dir.create(file.path(subject_channel_dir, 'cache', 'power', 'ref', b), showWarnings = F, recursive = T)
        dir.create(file.path(subject_channel_dir, 'cache', 'phase', 'ref', b), showWarnings = F, recursive = T)
        dir.create(file.path(subject_channel_dir, 'cache', 'voltage', 'ref', b), showWarnings = F, recursive = T)
    }
    # write a 'noref' table to this file in case exporting scheme screw up
    cr_csv = file.path(subject_channel_dir, 'cache', 'cached_reference.csv')
    rave:::safe_write_csv(data.frame(
        Electrode = electrodes,
        Reference = 'noref'
    ), cr_csv, row.names = F)


    lapply_async(electrodes, function(e){
        fname = sprintf('%d.h5', e)
        r = ref_tbl$Reference[ref_tbl$Electrode == e]
        if(is.blank(r)) { r = 'noref' }

        volt_fname = file.path(subject_channel_dir, 'voltage', fname)
        power_fname = file.path(subject_channel_dir, 'power', fname)
        phase_fname = file.path(subject_channel_dir, 'phase', fname)

        lapply(blocks, function(b){
            # load electrode - raw
            volt = load_h5(volt_fname, '/raw/voltage/' %&% b, ram = T)
            # reference voltage
            volt_ref = refs[[r]][['volt']][[b]][]
            volt_ref %?<-% 0
            volt = volt - volt_ref
            save_h5(volt, volt_fname, name = '/ref/voltage/' %&% b, replace = T, chunk = 1024)
            fst::write_fst(data.frame(V1 = volt), path =
                               file.path(subject_channel_dir, 'cache', 'voltage', 'ref', b, sprintf('%d.fst', e)))

            # load electrode - coef
            power = load_h5(power_fname, '/raw/power/' %&% b, ram = T)
            phase = load_h5(phase_fname, '/raw/phase/' %&% b, ram = T)
            coef = sqrt(power) * exp(1i * phase)

            # reference coef
            coef_ref = refs[[r]][['coef']][[b]]
            if(!is.null(coef_ref)){
                if('power' %in% names(coef_ref)){
                    coef_ref = sqrt(coef_ref$power[]) * exp(1i * coef_ref$phase[])
                }else{
                    coef_ref = coef_ref[]
                    coef_ref = coef_ref[,,1] * exp(1i * coef_ref[,,2])
                }
            }else{
                coef_ref = 0
            }
            coef = coef - coef_ref

            # save power and phase
            power = Mod(coef)^2
            phase = Arg(coef)
            dim = dim(power); dim[2] = 128
            save_h5(power, power_fname, name = '/ref/power/' %&% b, replace = T, chunk = dim)
            fst::write_fst(as.data.frame(t(power)), path =
                               file.path(subject_channel_dir, 'cache', 'power', 'ref', b, sprintf('%d.fst', e)))


            save_h5(phase, phase_fname, name = '/ref/phase/' %&% b, replace = T, chunk = dim)
            fst::write_fst(as.data.frame(t(phase)), path =
                               file.path(subject_channel_dir, 'cache', 'phase', 'ref', b, sprintf('%d.fst', e)))

            rm(list = ls(envir = environment())); gc()
            invisible()
        })

        # save reference
        save_h5(r, file = volt_fname, name = '/reference', replace = T, chunk = 1, size = 1000)
        save_h5(r, file = power_fname, name = '/reference', replace = T, chunk = 1, size = 1000)
        save_h5(r, file = phase_fname, name = '/reference', replace = T, chunk = 1, size = 1000)
        return(r)
    }, .call_back = function(ii){
        progress$inc(sprintf('Referencing electrode - %d', electrodes[[ii]]))
    }) ->
        refs


    # overwrite cached_reference.csv
    write.csv(data.frame(
        Electrode = electrodes,
        Reference = unlist(refs)
    ), cr_csv, row.names = F)

    progress$close()
    showNotification(p('Now data are cached according to [', fname, ']. Reloading subject.'), type = 'message', id = ns('ref_export_cache_notification'))

    removeModal()

    shinyjs::alert(paste0('Reference table [', fname, 
                          '] exported. Please reload subject to take effect.'))
})

observeEvent(input[[('do_export')]], {
    fname = write_ref_table()
    showNotification(p('Reference table [', fname, 
                       '] exported. Please reload subject to take effect.'), 
                     type = 'message', 
                     id = ns('ref_export_cache_notification'))
    removeModal()
    
    shinyjs::alert(paste0('Reference table [', fname, 
                          '] exported. Please reload subject to take effect.'))

})

check_load_volt = function(){
    if(is.null(env$volt)){
        env$volt = module_tools$get_voltage2()
    }
}


observeEvent(input$cur_group_save, {
    # Save table
    local_data$ref_tbl = get_ref_table()
    showModal(
        shiny::modalDialog(
            title = 'Export Reference Table',
            size = 'l',
            easyClose = T,
            footer = fluidRow(
                div(
                    class = 'col-md-4 col-md-push-8 col-sm-12',
                    textInput(ns('ref_export_name'), 'Reference Name: ', value = 'default', placeholder = 'File name for reference table')
                ),
                column(
                    width = 12L,
                    modalButton('Cancel'),
                    actionButton(ns('do_export'), 'Export'),
                    actionButton(ns('do_export_cache'), 'Export & Cache')
                )
            ),
            DT::DTOutput(ns('export_table'))
        )
    )
})


# Output - visualizations
console = function(){
    print(reactiveValuesToList(input))
}


observeEvent(input$ref_blockwise, {

    env$blockwise_reference %?<-% data.frame(
        Block = subject$preprocess_info('blocks'),
        Reference = 'Zeros',
        stringsAsFactors = F
    )

    showModal(modalDialog(
        title = 'Reference Generator',
        easyClose = F,
        size = 'l',
        footer = tagList(
            actionButton(ns('ref_modal_cancel'), 'Discard'),
            actionButton(ns('ref_modal_ok'), 'Generate Reference')
        ),
        div(
            p('WARNING: Reference is not recommended on the block level. ',
              'This module only provides partial support and please do NOT cache the referenced electrodes. ',
              'Also, visualizations will be in accurate if reference by blocks, so use at your own risk.', style = 'color:red;'),
            DT::DTOutput(ns('ref_modal_tbl'))
        )
    ))
})

observeEvent(input$ref_modal_cancel, { removeModal() })

output$ref_modal_tbl <- DT::renderDT({
    env$blockwise_reference
}, env = ..runtime_env, editable = T)

ref_modal_tbl_proxy = DT::dataTableProxy('ref_modal_tbl', session = session)

observeEvent(input$ref_modal_tbl_cell_edit, {
    info = input$ref_modal_tbl_cell_edit
    i = info$row
    j = info$col
    v = info$value

    # string match electrode
    v = str_match(v, '(ref_|[\\ ]{0})([0-9,\\-]*)')[3]
    if(!length(v) || is.na(v) || isTRUE(v=='')){
        v = 'Zeros'
    }else{
        v = dipsaus:::parse_svec(v)
        v = subject$filter_all_electrodes(v)
        if(length(v)){
            v = dipsaus:::deparse_svec(v)
        }else{
            v = 'Zeros'
        }
    }
    blockwise_reference = env$blockwise_reference

    if(names(blockwise_reference)[j] == 'Reference'){
        env$blockwise_reference[i, j] = v
        DT::replaceData(ref_modal_tbl_proxy, env$blockwise_reference, resetPaging = FALSE)  # important
    }
})

observeEvent(input$ref_modal_ok, {
    blockwise_reference = env$blockwise_reference
    gen_reference_blockwise(blockwise_reference)
})



observe({
    ref_calc_label = 'Generate Reference'
    ref_es = dipsaus:::parse_svec(input$ref_electrodes)
    if(length(ref_es)){
        ref_es = subject$filter_all_electrodes(ref_es)

        if(length(ref_es)){
            ref_calc_label = 'Generate [ref_' %&% dipsaus:::deparse_svec(ref_es) %&% "]"
        }
    }

    updateActionButton(session, inputId = 'ref_calc', label = ref_calc_label)
})

observeEvent(input$ref_calc, {
    ref_electrodes = dipsaus::parse_svec(ref_electrodes)
    ref_es = subject$filter_all_electrodes(ref_electrodes)

    if(length(ref_es) == 0){
        showNotification(p('No electrode(s) selected'), type = 'error')
    }else{
        # check conditions if we need to create reference
        old_files = list.files(env$ref_dir, pattern = 'ref_.*\\.h5')
        old_files = str_split_fixed(old_files, '(ref_)|(\\.h5)', 3)[,2]
        new_file = dipsaus:::deparse_svec(ref_es)
        if(new_file %in% old_files){
            showNotification(p('Reference [ref_', new_file, '.h5] already exists.'), type = 'message')
        }else{
            gen_reference(ref_es)
        }
    }
})

