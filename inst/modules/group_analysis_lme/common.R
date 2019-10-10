


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


lme_out = function() {
    # put analysis information in here
    if(is.null(local_data$lmer_results)){
        return(htmltools::div(style='color:#a1a1a1; text-align:center; ', 'No model calculated yet'))
    }
    
    lmer_results = local_data$lmer_results
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
    
    smry = summary(lmer_results)
    
    tbl_html = htmltable_coefmat(smry$coefficients, caption = 'LME Summary Table')
    # put a description row
    htmltools::p(
        smry$methTitle, sprintf(' (%s)', smry$objClass), br(),
        'LME call: ', strong(format(formula(lmer_results))), br(),
        
        'Number of obs: ', strong(smry$devcomp$dims[["n"]]), 'groups: ', 
        strong(paste(paste(names(smry$ngrps), smry$ngrps, sep = ', '), collapse = '; ')), br(),
        
        br(),
        # Convergence criteria
        local({
            aictab = smry$AICtab
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
            structure(as.list(quantile(smry$residuals, na.rm = TRUE)), names = NULL)
        )),
        
        # coef table
        tbl_html$table
        
    )
}
