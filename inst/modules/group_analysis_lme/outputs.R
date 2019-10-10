src_data_snapshot <- function(){
    
    if(!is.data.frame(local_data$analysis_data)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center', 'Analysis data not loaded yet.'))
    }
    
    # show snapshot of analysis table
    tbl_raw = local_data$analysis_data
    vars = names(tbl_raw)
    tbl = local_data$analysis_data_filtered
    
    dv = input$model_dependent; dv = dv[dv %in% vars]
    fe = input$model_fixed_effects; fe = fe[fe %in% vars]
    fr = input$model_random_effects; fr = fr[fr %in% vars]
    rest = vars[!vars %in% c(dv,fe,fr)]
    
    str2 = function(val, v){
        suppressWarnings({
            if(is.character(val) || v %in% fr || is.factor(val)){
                val = as.factor(val)
                lv = levels(val); 
                nlv = length(lv)
                if(nlv){ lv = lv[1:min(nlv, 4)] }
                str = sprintf(' Factor w/ %d level%s [%s%s]', nlv, 
                              ifelse(nlv>1, 's', ''),
                              paste(lv, collapse = ', '), ifelse(nlv>4, ', ...', ''))
            }else if(is.numeric(val)){
                str = sprintf(' %s [range: %.4g ~ %.4g]', storage.mode(val), 
                              min(val, na.rm = TRUE), max(val, na.rm = TRUE))
            }else{
                str = utils::capture.output(str(val))
            }
            str
        })
    }
    dv_tag = rest_tag = fe_tag = fr_tag = NULL
    if(length(dv)){
        dv_tag = tagList(
            '- Dependent -',
            tags$ul(
                tags$li(strong(dv), ': ', str2(tbl[[dv]], dv))
            )
        )
    }
    
    if(length(fe)){
        fe_tag = tagList(
            '- Fixed effects -',
            tags$ul(
                lapply(fe, function(v){
                    tags$li(strong(v), ': ', str2(tbl[[v]], v))
                })
            )
        )
    }
    
    if(length(fr)){
        fr_tag = tagList(
            '- Random effects -',
            tags$ul(
                lapply(fr, function(v){
                    tags$li(strong(v), ': ', str2(tbl[[v]], v))
                })
            )
        )
    }
    
    if(length(rest)){
        rest_tag = tagList(
            '- Variables not in the model -',
            tags$ul(
                lapply(rest, function(v){
                    tags$li(strong(v), ': ', str2(tbl[[v]], v))
                })
            )
        )
    }
    
    if(length(c(dv, fe, fr))){
        n_complete = sum(complete.cases(tbl[,c(dv, fe, fr)]))
    }else{
        n_complete = nrow(tbl)
    }
    
    tags$p(
        # 1. dimensions
        'Original analysis table (raw): ', strong(sprintf('%d rows x %d columns', nrow(tbl_raw), ncol(tbl_raw))), br(),
        
        # 2. columns
        'Columns: ', strong(paste(vars, collapse = ', ')), br(),
        
        hr(),
        
        # 3. filtered table
        'Filtered analysis table (input data): ', strong(sprintf(
            '%d rows (%d complete entries)', nrow(tbl), n_complete
        )), br(),
        
        # 3. column types
        'Column types: ', br(),
        
        dv_tag, fe_tag, fr_tag, rest_tag
        
    )
    
}
