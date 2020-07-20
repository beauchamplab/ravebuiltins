 
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
    # put analysis information in here
    if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    }
    # local_data = ..local_data
    
    lmer_results = local_data$lmer_results
    lmer_summary = local_data$lmer_results_summary
    
    lmer_cond = data.frame('No main effects are possible' = 0)
    lmer_compare = data.frame('No comparisons are possible' = 0)
    
    if(inherits(lmer_results, "lmerModLmerTest")) {
        look_for_vars = attr(terms(lmer_results), 'term.labels')
        main_effects_only = look_for_vars[!grepl(':', look_for_vars, fixed = TRUE)]
        
        if('ROI' %in% main_effects_only) {
            if(model_params$how_to_model_roi == 'Stratify (Random+Fixed)') {
                wo_roi <- main_effects_only[-which('ROI' == main_effects_only)]
                .fo <- as.formula('pairwise ~ ' %&% paste0(wo_roi, collapse='*') %&% "|ROI")
                
                res = emmeans::emmeans(lmer_results,
                                       specs = .fo, options=list(infer=c(F,T), adjust='fdr'),
                                       adjust='fdr', lmer.df='satterthwaite')
                .fo <- as.formula('pairwise ~ ROI | ' %&% paste0(wo_roi, collapse='*'))
                
                res2 = emmeans::emmeans(lmer_results,
                                        specs = .fo, options=list(infer=c(F,T), adjust='fdr'),
                                        adjust='fdr', lmer.df='satterthwaite')
                class(res) = 'list'
                
                res$emmeans = as.data.frame(res$emmeans)
                res$emmeans[which(sapply(res$emmeans, is.factor))] %<>% lapply(as.character)
                
                # this first RES has contrasts PER roi
                contrast_df = as.data.frame(res$contrasts)
                contrast_df[which(sapply(contrast_df, is.factor))] %<>% lapply(as.character)
                
                for(ii in 1:nrow(contrast_df)) {
                    contrast_df$contrast[ii] = paste(contrast_df$ROI[ii], contrast_df$contrast[ii], sep=': ')
                }
                contrast_df$ROI  = NULL
                
                # second RES has ROI contrast PER fixed-factor level
                df2 = as.data.frame(res2$contrasts)
                df2[which(sapply(df2, is.factor))] %<>% lapply(as.character)
                
                for(ii in 1:nrow(df2)) {
                    df2$contrast[ii] = 
                        paste(paste0(as.matrix(df2[c(wo_roi)])[ii,], collapse=','), df2$contrast[ii], sep=': ')
                }
                df2[[wo_roi]] = NULL
                res$contrasts = rbind(contrast_df, df2)
                
                ..roi = as.character(res$emmeans$ROI)
                res$emmeans$ROI = NULL
                res$emmeans = cbind('ROI'=..roi, res$emmeans,
                                    stringsAsFactors=FALSE)
                
                # p.adjust(
                # str(summary(res2$contrasts))[,'p.value']
                # , method = 'fdr')
            } else if(model_params$how_to_model_roi == 'All possible ITX (Random+Fixed)') {
                .fo <- as.formula('pairwise ~ ' %&% paste0(main_effects_only, collapse='*'))
                res = emmeans::emmeans(lmer_results,
                                       specs = .fo, options=list(infer=c(F,T), adjust='fdr'),
                                       adjust='fdr', lmer.df='satterthwaite')
            }
        } else {
            res = emmeans::emmeans(lmer_results,
                                   specs = as.formula(paste0('pairwise ~ ', paste0(main_effects_only, collapse='*'))),
                                   options=list(infer=c(F,T), adjust='fdr'), adjust='fdr', lmer.df='satterthwaite')
        }
        
        lmer_cond <- fix_rownames(
            do_if(inherits(res$emmeans, 'emmGrid'), summary(res$emmeans), res$emmeans)
        )
        lmer_compare <- fix_rownames(
            do_if(inherits(res$contrasts, 'emmGrid'), summary(res$contrasts), res$contrasts)
        )
        
        lmer_cond$emmean %<>% round(3)
        lmer_cond$SE %<>% round(2)
        lmer_cond$df %<>% round_df
        lmer_cond$t.ratio %<>% round_test_statistic
        lmer_cond$p.value %<>% round_pval
        
        lmer_compare$estimate %<>% round(3)
        lmer_compare$SE %<>% round(2)
        lmer_compare$df %<>% round_df
        lmer_compare$t.ratio %<>% round_test_statistic
        lmer_compare$p.value %<>% round_pval
        
    } else {
        
        ### do something for regular lm output?
    }
    
    local_data$test_conditions = lmer_cond
    test_conditions <- htmltable_mat(lmer_cond)
    
    local_data$compare_conditions= lmer_compare
    compare_conditions <- htmltable_mat(lmer_compare)
    
    htmltools::p(
        h4('Compare condition means against 0'),
        test_conditions$table,
        hr(),
        h4('All pairwise comparisons'),
        compare_conditions$table
    )
}

fix_rownames <- function(m, regression=FALSE, lmer_results) {
    lmer_results %?<-% local_data$lmer_results
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

lme_out <- function() {
    # put analysis information in here
    if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    }
    
    # local_data = ..local_data
    
    lmer_results = local_data$lmer_results
    lmer_summary = local_data$lmer_results_summary

    ss_type = 2    
    if(length(attr(terms(lmer_results), 'term.labels')) <1) {
        ss_type = 3
    }
    
    deviance_summary = car::Anova(lmer_results, type=ss_type)
        
    anova_html = htmltable_coefmat(deviance_summary)
    local_data$anova_summary <- deviance_summary
    
    
    # dipsaus::cat2('in lme_out::building final output', level='info')
    # put a description row
    
    htmltools::p(
        h4('Analysis of Deviance table from car::Anova'),
        anova_html$table
    )
}




regression_output <- function() {
    # put analysis information in here
    if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    }
    
    lmer_results = local_data$lmer_results
    lmer_summary = local_data$lmer_results_summary
    
    
    lmer_summary$coefficients %<>% fix_rownames(regression=TRUE)
    local_data$lmer_summary_coefficients <- lmer_summary$coefficients
    
    
    string_formula = as.character(formula(lmer_results))
    string_formula = paste(string_formula[2], string_formula[1], string_formula[3])

    lme_message = tryCatch({
        ifelse(is.null(lmer_results@optinfo$conv$lme4$messages),
               'No Message', lmer_results@optinfo$conv$lme4$messages)
    }, error=function(e) {
        'No Message'
    })
    
    tbl_html = htmltable_coefmat(lmer_summary$coefficients)
    
    if(inherits(lmer_results, "lmerModLmerTest")) {
        rand_eff_table = htmltable_mat(lme4::formatVC(lme4::VarCorr(lmer_results)))
    } else {
        rand_eff_table = htmltools::p('No random effects in model...')        
    }
    
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
        p('LME Message: ', lme_message)
    ),
    hr(),
    h4('Random Effects Table'),
    rand_eff_table,
    hr(),
    # coef table
    h4('LME Regression Table'),
    tbl_html$table
    )
}


round_pval <- function(pval) {
    lpval = pmax(-16, log10(pval))
    ifelse(lpval > -3.5,
           formatC(round(pval,4),width = 4, digits=4),
           paste0('1e', formatC(round(lpval), width=3,flag=0)))
}

round_test_statistic <- function(df) round(df, 2)
round_df <- function(df) round(df, 1)




build_stat_names <- function(lbls, stat.vars = c('m', 't', 'p')) {
    c(outer(c(stat.vars %&% '('),
            lbls, paste0)) %&% ')'
}

flatten_emmeans_pairwise <- function(summ) {
    # summ = bed.pairwise
    
    var.names = attr(summ[[1]], 'pri.vars')
    lbls = apply(summ[[1]][var.names], 1, paste0, collapse='&')
    
    a = summ[[1]] %$% {
        c(rbind(emmean, t.ratio, p.value))
    } %>% set_names (build_stat_names(lbls))
    
    b = summ[[2]] %$% {
        c(rbind(estimate, t.ratio, p.value))
    } %>% set_names(build_stat_names(summ[[2]]$contrast))
    
    res = c(a,b)
    names(res) = str_replace_all(names(res), ',', '&')
    
    res
}

# the idea here is to add terms while ensuring no duplicates
add_term <- function(x, value) {
    unique(c(x, value))
}

remove_term <- function(x, value) {
    x[x!=value]
}

analyze_single_electrode <- function(bed) {
    
    # note that for single electrode analyses, between-electrode variables (e.g., freesurferlabel) 
    # don't make sense
    fes <- local_data$var_fixed_effects
    lens = sapply(fes, function(vfe) {
        length(unique(bed[[vfe]]))
    })
    fes = fes[lens > 1]
    
    # create a string represent the requested fixed effects
    fe = paste(fes, collapse=' * ')
    
    # bed <- by_el_data %>% split((.)$UUID) %>% extract2(1)
    if(str_detect(fe, 'TimeWindow')) {
        .lm = lmer(as.formula('y ~ ' %&% fe %&% '+ (1|Trial)'), data=bed)
        # .lm = lmer(y ~ ConditionGroup*TimeWindow + (1|Trial), data=bed)
        omni.mat = as.matrix(car::Anova(.lm)[c('Chisq', 'Pr(>Chisq)')])
        
        bed.omni = c(t(omni.mat)) %>% set_names(
            build_stat_names(rownames(omni.mat), c('X2', 'p')))
        
        # this will help with the calls later
        emmeans::emm_options('lmer.df' = 'satterthwaite')
        
    } else {
        .lm = lm(as.formula('y ~ 1' %?&% fe), data=bed)
        omni.mat = as.matrix(car::Anova(.lm)[,c('F value', 'Pr(>F)')])
        omni.mat = omni.mat[str_detect(rownames(omni.mat),'Residuals', negate = 1),,drop=FALSE]
        
        bed.omni = c(t(omni.mat)) %>% set_names(
            build_stat_names(rownames(omni.mat), c('F', 'p'))
        )
    }
    
    # main effects
    bed.mains = lapply(fes, function(vfe) {
        m =summary(emmeans::emmeans(.lm,
                                    as.formula('~' %&% vfe)),
                   infer=c(F,T))[,c(1:2, 5:6)]
        nms = as.character(m[,1])
        c(t(m[,-1])) %>% set_names(build_stat_names(nms))
    }) %>% unlist
    
    # pairwise comparisons
    bed.pairwise = NULL
    if(length(fes) > 0) {
        bed.pairwise = summary(emmeans::emmeans(
            .lm, as.formula('pairwise ~ 1' %?&% fe)),
            infer=c(FALSE, TRUE)) %>% flatten_emmeans_pairwise
    }
    
    res <- data.frame(Project = bed$Project[1], Subject = bed$Subject[1])
    if(!is.null(bed$ROI)) { res$ROI = bed$ROI[1] }
    res$Electrode = bed$Electrode[1]
    
    res[names(bed.omni)] = bed.omni
    res[names(bed.mains)] = bed.mains
    if(!is.null(bed.pairwise)){
        res[names(bed.pairwise)] = bed.pairwise
    }
    
    res
}
