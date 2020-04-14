 
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

text_to_range = function(str) {
    r = suppressWarnings({
        range(as.numeric(unlist(stringr::str_split(str, ',| |:'))), na.rm=TRUE)
    })
    
    if(any(is.na(r), r %in% c(-Inf, Inf))) {
        r = NULL
    } else if(diff(r) == 0) {
        r = c(-1, 1) * max(abs(r))
    }
    
    
    return (r)
}



# concatenate ["+" s2] onto s1, only if s2 is non blank
`%?&%` <- function(s1,s2) {
    if(!isTRUE(nchar(s2)>0)) {
        return(s1)
    }
    return (paste0(s1, '+', s2))
}


pretty_string <- function(s) {
    stringr::str_replace_all(s,c(
        'Pct' = '%',
        '_' = ' '
    ))
}

# check if a given needle is in any one of the supplied haystacks
# returns TRUE/FALSE for each needle if contained in ANY of the hay stacks.
# alternatively, supply FUN=which to get the index of the haystack
`%within_any%` <- function(needles, haystacks, FUN=any) {
    if(!is.list(haystacks)) return (needles %within% haystacks)
    
    apply(sapply(haystacks, function(h) {
        needles %within% h
    }), 1, FUN)
}


multiple_comparisons <- function() {
    # return()
    # lmer_results = local_data$lmer_results
    # 
    # if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    # }
    # print('in MC')
    
    test_conditions <- htmltable_coefmat(ls_means(lmer_results))
    compare_conditions <- htmltable_coefmat(ls_means(lmer_results, pairwise = TRUE))
    
    htmltools::p(
        test_conditions$table,
        hr(),
        compare_conditions$table
    )
}

lme_out <- function() {
    # put analysis information in here
    if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    }
    
    dipsaus::cat2('in lme_out', level='info')
    if(exists('.__DEBUG__')) {
        assign('lmer_results', local_data$lmer_results, envir = globalenv())   
    }
    
    lmer_results = local_data$lmer_results
    lmer_summary = local_data$lmer_results_summary

    ss_type = 2    
    if(length(attr(terms(lmer_results), 'term.labels')) <1) {
        ss_type = 3
    }
    
    dipsaus::cat2('in lme_out:: deviance_summary', level='info')
    deviance_summary = car::Anova(lmer_results, type=ss_type)
        
    
    fix_rownames <- function(m, regression=FALSE) {
        look_for_vars = attr(terms(lmer_results), 'term.labels')
        
        # character vector with find=>replace structure
        remove_var_name = rep('', length(look_for_vars))
        names(remove_var_name) = look_for_vars
        
        # this is used later, but relies on rownames which are changed here... can we move this around, maybe change rownames after 
        # this if(regression ...) part?
        contains_lfv = sapply(rownames(m), function(rn) {
            any(str_detect(rn, look_for_vars))
        })
        
        if(length(remove_var_name) > 0) {
            rownames(m) %<>% str_replace_all(remove_var_name)
        }
        
        if(regression && nrow(m) > 1) {
            # ..local_data$lmer_results@frame
            # remove the interaction terms from look_for
            look_for_vars = look_for_vars[!str_detect(look_for_vars, ':')]
            
            bsl = paste0(sapply(lmer_results@frame[look_for_vars], 
                                function(x) levels(x)[1]), collapse=':')
            
            rownames(m) %<>% str_replace_all('\\(Intercept\\)', paste(bsl, '(INT)'))
            rownames(m)[contains_lfv] = paste(rownames(m)[contains_lfv], 'vs', bsl)
        }

        return(m)
    }
    
    lmer_summary$coefficients %<>% fix_rownames(regression=TRUE)
    local_data$lmer_summary_coefficients <- lmer_summary$coefficients
    tbl_html = htmltable_coefmat(lmer_summary$coefficients)
    
    dipsaus::cat2('in lme_out::anova_html', level='info')
    anova_html = htmltable_coefmat(deviance_summary)
    local_data$anova_summary <- deviance_summary
    
    lmer_cond = data.frame('No main effects are possible' = 0)
    lmer_compare = data.frame('No comparisons are possible' = 0)
    
    dipsaus::cat2('in lme_out::fixing rownames', level='info')
    if(inherits(lmer_results, "lmerModLmerTest")) {
        lmer_cond <- fix_rownames(
            lmerTest::ls_means(lmer_results)
        )
        lmer_compare <- fix_rownames(
            ls_means(lmer_results, pairwise = TRUE)
        )    
    } else {
        
    }
    
    local_data$test_conditions = lmer_cond
    test_conditions <- htmltable_coefmat(lmer_cond)

    local_data$compare_conditions= lmer_compare
    compare_conditions <- htmltable_coefmat(lmer_compare)
    
    dipsaus::cat2('in lme_out::building final output', level='info')
    # put a description row
    
    string_formula = as.character(formula(lmer_results))
    string_formula = paste(string_formula[2], string_formula[1], string_formula[3])
    htmltools::p(
        lmer_summary$methTitle, sprintf(' (%s)', lmer_summary$objClass), br(),
        'LME call: ', strong(string_formula), br(),
        'Number of obs: ', strong(lmer_summary$devcomp$dims[["n"]]), 'groups: ', 
        strong(paste(paste(names(lmer_summary$ngrps), lmer_summary$ngrps, sep = ', '), collapse = '; ')), br(),
        
        br(),
        # Convergence criteria
        local({
            res = NULL
            aictab = lmer_summary$AICtab
            if(!is.null(aictab)) {
                t.4 <- round(aictab, 1)
                if (length(aictab) == 1 && names(aictab) == "REML") 
                    res = tagList(paste("REML criterion at convergence:", t.4), br())
            }
            res
        }),
        
        # residual
        do.call('sprintf', c(
            list('Scaled residual: %.4g (min), %.4g (25%%), %.4g (median), %.4g (75%%), %.4g (max)'),
            structure(as.list(quantile(lmer_summary$residuals, na.rm = TRUE)), names = NULL)
        )),
        div(
            p('LME Message: ', ifelse(is.null(lmer_results@optinfo$conv$lme4$messages), 'No Message', lmer_results@optinfo$conv$lme4$messages))
        ),
        hr(),
        h3('Random Effects Table'),
        htmltable_mat(lme4::formatVC(lme4::VarCorr(lmer_results))),
        hr(),
        # coef table
        h3('LME Regression Table'),
        tbl_html$table,
        hr(),
        h3('Analysis of Deviance table from car::Anova'),
        anova_html$table,
        hr(),
        h3('Compare condition means against 0'),
        test_conditions$table,
        hr(),
        h3('All pairwise comparisons'),
        compare_conditions$table
    )
}
