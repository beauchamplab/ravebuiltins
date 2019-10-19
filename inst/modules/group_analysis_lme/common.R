
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

multiple_comparisons <- function() {
    # return()
    # lmer_results = local_data$lmer_results
    # 
    # if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    # }
    print('in MC')
    
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
    
    lmer_results = local_data$lmer_results
    lmer_summary = local_data$lmer_results_summary
    
    deviance_summary = car::Anova(lmer_results)
    
    # clean row names
    gnames = levels(local_data$collapsed_data$Group)
    fix_rownames <- function(m, regression=FALSE) {
        .match = paste0("Group", gnames)
        if(regression) {
            .replace = c(gnames[1], paste(gnames, 'rel. to', gnames[1]))
            names(.replace) = c('\\(Intercept\\)', .match)
        } else {
            .replace = gnames
            names(.replace) = .match
        }
        rownames(m) = stringr::str_remove_all(rownames(m), .replace)
        m
    }
    
    lmer_summary$coefficients %<>% fix_rownames(regression=TRUE)
    tbl_html = htmltable_coefmat(lmer_summary$coefficients)
    
    anova_html = htmltable_coefmat(deviance_summary)
    
    test_conditions <- htmltable_coefmat(fix_rownames(ls_means(lmer_results)))
    compare_conditions <- htmltable_coefmat(fix_rownames(ls_means(lmer_results,
                                                     pairwise = TRUE)))
    
    # put a description row
    htmltools::p(
        lmer_summary$methTitle, sprintf(' (%s)', lmer_summary$objClass), br(),
        'LME call: ', strong(format(formula(lmer_results))), br(),
        'Number of obs: ', strong(lmer_summary$devcomp$dims[["n"]]), 'groups: ', 
        strong(paste(paste(names(lmer_summary$ngrps), lmer_summary$ngrps, sep = ', '), collapse = '; ')), br(),
        
        br(),
        # Convergence criteria
        local({
            aictab = lmer_summary$AICtab
            t.4 <- round(aictab, 1)
            if (length(aictab) == 1 && names(aictab) == "REML") 
                res = tagList(paste("REML criterion at convergence:", t.4), br())
            else {
                # t.4F <- format(t.4)
                # t.4F["df.resid"] <- format(t.4["df.resid"])
                # res = capture.output(print(t.4F, quote = FALSE))
                res = NULL
            }
            res
        }),
        
        # residual
        do.call('sprintf', c(
            list('Scaled residual: %.4g (min), %.4g (25%%), %.4g (median), %.4g (75%%), %.4g (max)'),
            structure(as.list(quantile(lmer_summary$residuals, na.rm = TRUE)), names = NULL)
        )),
        
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
