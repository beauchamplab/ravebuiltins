get_pluriform_power <- function(baselined_data, trial_indices, events, epoch_event_types, event_of_interest, trial_outliers_list,
    logger=function(...){invisible(NULL)}) {
    
    res <- list()
    
    res <- list(
        data = baselined_data$subset(Trial = Trial %in% trial_indices)
    )
    
    # now check if we need shifted data
    # R is copy on write, so no worries here about memory
    res$shifted_data = res$data
    shift_amount = NULL
    # event_of_interest = '1stWord'
    if(event_of_interest != epoch_event_types[1]) {
        logger('Shifting data to: ' %&% event_of_interest)
        new_range = determine_available_shift(event_of_interest,
            available_time = range(res$data$dimnames$Time),
            epoch_information = events
        )
        
        logger('available shift: ' %&% paste0(new_range, collapse=':'))
        
        shift_amount = determine_shift_amount(event_time = events[[event_of_interest]],
            available_shift=new_range)
        
        logger('dispaus::shift')
        
        if(length(shift_amount) != dim(res$data)[1L]) {
            # assign('shift_amt', shift_amount, envir = globalenv())
            # assign('event_mat', events, envir = globalenv())
            stop('shift amount != # trials... stopping')
        }
        
        res$shifted_data = get_shifted_tensor(raw_tensor = res$data$get_data(),
            shift_amount = shift_amount, new_range = new_range,
            dimnames = dimnames(res$data), varnames = names(res$data$dimnames))
        
        # alright, now that we've shifted the data we also need to shift the events dataset, so that future sorts on the event_of_interest don't do anything
        logger('updating events file')
        events[epoch_event_types[-1]] <- events[epoch_event_types[-1]] - events[[event_of_interest]]
        logger('done with shifting')
    }
    
    # handle outliers
    if(length(trial_outliers_list) == 0) {
        res$clean_data <- data
        res$shifted_clean_data <- res$shifted_data
    } else {
        logger('Handling outliers...')
        res$clean_data <- res$data$subset(Trial = !(Trial %in% trial_outliers_list))
        res$shifted_clean_data <- res$shifted_data$subset(Trial = !(Trial %in% trial_outliers_list))
    }
    
    # make sure to save out the update time stamps to be used later
    res$events = events

    return(res)
}

build_heatmap_data <- function(data, data_wrapper, do_censor=FALSE, censor_window=NULL, analysis_settings, ...) {
    
    hd <- data_wrapper(
        # hmd is using the clean data
        data$collapse(keep = c(3,2), method = analysis_settings$collapse_method),
        xlab='Time (s)', ylab='Frequency', zlab='auto',
        x = data$dimnames$Time,
        y = data$dimnames$Frequency,
        N = dim(data)[1L], ...
    )
    
    if(!missing(analysis_settings)) {
        hd[names(analysis_settings)] = analysis_settings
    }
    
    if(do_censor && !is.null(censor_window)) {
        hd$range <- .fast_range(hd$data[!(hd$x %within% censor_window), ] )
    }
    
    return(hd)
}

build_heatmap_correlation_data <- function(data, data_wrapper,
    analysis_settings, analysis_settings2, ...) {
    
    time_index1 <- data$dimnames$Time %within% analysis_settings$analysis_window
    time_index2 <- data$dimnames$Time %within% analysis_settings2$analysis_window
    
    if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
        t_clean <- !(data$dimnames$Time %within% analysis_settings$censor_window)
        
        time_index1 = time_index1 & t_clean
        time_index2 = time_index2 & t_clean
    }
    
    t1 <- data$subset(Time=time_index1)$collapse(keep = c(3,2), method = analysis_settings$collapse_method)
    t2 <- data$subset(Time=time_index2)$collapse(keep = c(3,2), method = analysis_settings$collapse_method)
    
    # image(cor(cbind(t1,t2)), col=colorRampPalette(c('navy', 'white', 'red'))(101), y=data$dimnames$Frequency, x=data$dimnames$Frequency)
    
    # fastest to just get the whole cmat, but then subset it so that it only includes the t1-t2 correlations
    ind <- 1:dim(t1)[2L]
    cmat <- cor(cbind(t1,t2))[-ind,-ind]
    # image(t(cmat)[,rev(ind)], axes=F,
    #     y=data$dimnames$Frequency, x=data$dimnames$Frequency,
    #     col=colorRampPalette(c('navy', 'white', 'red'))(101), zlim=c(-1,1), xlab='Frequency at Time 1', ylab='Frequency at Time 2')
    # axis(2, at=data$dimnames$Frequency, las=1, lwd=0)
    # 
    data_wrapper(
        data=cmat,
        xlab=sprintf('%s from %ss', analysis_settings$unit_of_analysis, str_collapse(analysis_settings$analysis_window, '-')),
        ylab=sprintf('%s from %ss', analysis_settings$unit_of_analysis, str_collapse(analysis_settings2$analysis_window, '-')),
        zlab='Frequency-wise Pearson correlation',
        x = data$dimnames$Frequency,
        y = data$dimnames$Frequency,
        N = 2*dim(data)[2L], 
        frequency_window1 = analysis_settings$frequency_window,
        frequency_window2 = analysis_settings2$frequency_window,   
        ...
    )
}

build_by_trial_heatmap_data <- function(data, data_wrapper, analysis_settings, ...) {
    bthd <- data_wrapper(
        data$collapse(keep = c(3,1), method = analysis_settings$collapse_method),
        x = data$dimnames$Time,
        y = seq_along(data$dimnames$Trial),
        xlab='Time (s)', ylab='Trial', zlab='auto'
    )
    
    bthd[names(analysis_settings)] = analysis_settings
    
    if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
        bthd$range <- .fast_range(bthd$data[ !(bthd$x %within% analysis_settings$censor_window), ])
    }
    
    return(bthd)
}

build_over_time_correlation_data <- function(f1, f2, 
    lag_length=50, ...) {
    
    # time_index1 <- f1$dimnames$Time %within% f1$analysis_window
    # time_index2 <- f1$dimnames$Time %within% f2$analysis_window
    
    # warning('over_time_correlation is ignoring censoring information')
    
    # if(do_censor && !is.null(censor_window)) {
    #     t_clean <- !(f1$dimnames$Time %within% censor_window)
    #     
    #     time_index1 = time_index1 & t_clean
    #     time_index2 = time_index2 & t_clean
    # }
    
    t1a <- f1$data[,1]
    t2a <- f2$data[,1]
    
    # nb: negative lag means t2 is "ahead" of t1
    lag_corr <- c(
        rev(lagged_cor(t1a,t2a,len=lag_length)),
        (lagged_cor(t2a,t1a,len=lag_length))
    )
    
    #remove the duplicate at 0
    lag_corr <- lag_corr[-(lag_length+1)]
    abs_ind <- which.max(abs(lag_corr))
    signed_ind <- which.max(lag_corr)
    lags <- -lag_length:lag_length
    
    return(list(
        correlations = lag_corr,
        ind_max_cor = signed_ind,
        ind_max_abs_cor = abs_ind,
        lags = lags,
        zf1 = scale(t1a),
        zf2 = scale(t2a),
        rf1 = rank(t1a),
        rf2 = rank(t2a)
    ))
}

# xy <- get_lagged_vectors(t1a, t2a, 50-abs_ind)
# plot(xy[,1], type='l', ylim=range(pretty(xy)))
# lines(get_lagged_vectors(t1a, t2a, 50-abs_ind)[,2])


build_electrode_heatmap_data <- function(data, data_wrapper, analysis_settings, ...) {
    ehd <- data_wrapper(
        data$collapse(keep = c(3,4), method = analysis_settings$collapse_method),
        x=data$dimnames$Time,
        y=data$dimnames$Electrode,
        xlab='Time (s)', ylab='Electrode', zlab='auto', ...
    )
    
    ehd[names(analysis_settings)] = analysis_settings
    
    if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
        ehd$range <- .fast_range(ehd$data[ !(ehd$x %within% analysis_settings$censor_window), ])
    }
    
    return(ehd)
}

build_over_time_data <- function(data, data_wrapper, analysis_settings, ...) {
    otd <- data_wrapper(t(
        apply(data$collapse(keep = 3:4, method = analysis_settings$collapse_method),
            1, .fast_mse)
        ),
        xlab='Time (s)', ylab='auto', N=dim(data)[4L], x=data$dimnames$Time, ...
    )
    
    
    otd[names(analysis_settings)] <- analysis_settings
    
    # set NA (divide by zero) error bars to 0  
    otd$data[is.na(otd$data[,2]),2] <- 0
    
    # we want to make a special range for the line plot data that takes into account mean +/- SE
    if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
        otd$range <- .fast_range(plus_minus(otd$data[!(otd$x %within% analysis_settings$censor_window),]))
    } else {
        otd$range <- .fast_range(plus_minus(otd$data))
    }
    
    if(!all(is.finite(otd$range))) {
        if(any(is.nan(otd$data[,1]))) {
            stop('Unable to plot data, data contain NaN')
        } else {
            stop(paste0('non-finite range... ', paste(otd$range, collapse='|')))
        }
    }
    
    return(otd)
}

build_scatter_bar_data <- function(data, data_wrapper,
    analysis_settings, group_info, jitter_seed, ...) {
    
    time_ind <- data$dimnames$Time %within% analysis_settings$analysis_window
    if(analysis_settings$do_censor && !is.null(analysis_settings$censor_window)) {
        time_ind = time_ind & (! data$dimnames$Time %within% analysis_settings$censor_window)
    }

    sbd <- data_wrapper(
        rowMeans(data$subset(Time = time_ind, data_only = TRUE)),
        xlab='Group', ylab='auto'
    )
    
    # add in the analysis settings so that we have them
    sbd[names(analysis_settings)] = analysis_settings
    
    # Although this seems to be the wrong place to do this, not sure where else we can do it
    # to enable point identification later, we need to know the x-location of each point. So the jittering
    # needs to be done here.
    .xp <- barplot(which(group_info$group_statuses),plot=FALSE)
    .r <- if(sum(group_info$group_statuses)>1) {
        mean(unique(diff(.xp)))*0.25  
    } else {
        0.75*(1/3)
    }
    
    xpi <- which(group_info$current_group == which(group_info$group_statuses))
    sbd$xp <- .xp[xpi]
    set.seed(jitter_seed)
    sbd$x <- .xp[xpi] + runif(length(sbd$data), -.r, .r)
    
    # for the scatter_bar_data we also need to get m_se within condition, this is ALWAYS with the clean data
    sbd$mse <- .fast_mse(sbd$data[sbd$is_clean])
    
    return(sbd)   
}

build_scatter_bar_correlation_data <- function(sb1, sb2, data_wrapper, ...) {
    dv <- attr(sb1$data, 'ylab')
    do_str <- function(d) {
        paste0(dv,' from ', str_collapse(d$my_analysis_window, '-'), 's, at ',
            str_collapse(d$my_frequency_window,':'), 'Hz')
    }
    
    data_wrapper(
        data = cor(sb1$data, sb2$data),
        x = sb1$data,
        y = sb2$data,
        xlab=do_str(sb1),
        ylab=do_str(sb2), ...
    )
}


get_p.adjust_method <- function(pval_filter=c('p', 'FDR(p)', 'Bonf(p)')) {
    pval_filter = match.arg(pval_filter)
    c('p'='none', 'FDR(p)'='fdr', 'Bonf(p)'='bonferroni')[pval_filter]
}


combine_emmeans_results <- function(r) {
    # as.data.frame(r$emmeans)
    # as.data.frame(r$contrasts)
    
    if(all(c('Electrode', 'Group') %in% names(as.data.frame(r$emmeans)))) {
        d = as.data.frame(r$emmeans)
        d$Electrode <- paste0('E', d$Electrode)
        gind <- which(names(d) == 'Group')
        tbl = d[,-gind]
        tbl$Electrode %<>% paste(as.character(d$Group), sep=':')
    } else {
        tbl = as.data.frame(r$emmeans)
    }
    names(tbl)[1:2] = c('label', 'estimate')
    
    contr = as.data.frame(r$contrasts)
    if('Electrode' %in% names(contr)) {
        contr$label = contr$Electrode %&% ':' %&% contr$contrast
        contr = subset(contr, select = -c(contrast, Electrode))
        rbind(tbl, contr)
    } else {
        rbind(tbl, contr %>% set_colnames(colnames(tbl)))
    }
}

summary_stat.random_intercept <- function(overall_stats) {
    if(nlevels(overall_stats$Group) > 1) {
        return (
            combine_emmeans_results(
                emmeans::emmeans(
                    lmerTest::lmer(y ~ Group + (1|Electrode), data = overall_stats),
                    options = list(infer=c(F,T)),
                    specs = pairwise ~ Group, infer=c(F,T))
            )
        )
    }
    
    res <- as.data.frame(emmeans::emmeans(lmerTest::lmer(
        y ~ 1 + (1|Electrode), data=overall_stats
    ), specs=~1, options=list(infer=c(F,T))))
    names(res)[1:2] = c('label', 'estimate')
    
    return(res)
}

summary_stat.contrasts_per_electrode <- function(overall_stats) {
    if(nlevels(overall_stats$Group) > 1) {
        return(
            combine_emmeans_results(
                emmeans::emmeans(
                    lmerTest::lmer(y ~ Group*Electrode + (1|Electrode), data = overall_stats),
                    options = list(infer=c(F,T)),
                    specs = pairwise ~ Group|Electrode, infer=c(F,T))
            )
        )
    } 
    
    res <- as.data.frame(emmeans::emmeans(
        lmerTest::lmer(y ~ Electrode + (1|TrialNumber), data=overall_stats)
        , specs = pairwise ~ Electrode, options=list(infer=c(F,T)))$emmeans
    )
    names(res)[1:2] = c('label', 'estimate')
    res$label = 'E' %&% res$label
    return(res)
}

summary_stat.collapse_electrode <- function(overall_stats) {
    .d <- aggregate(y ~ TrialNumber+Group, mean, data=overall_stats)
    
    if(nlevels(.d$Group) > 1) {
        return (combine_emmeans_results(emmeans::emmeans(lm(y ~ Group, data=.d), 
            options = list(infer=c(F,T)),
            pairwise ~ Group, infer=c(F,T)))
        )
    } 
    res = as.data.frame(
        emmeans::emmeans(lm(y ~ 1, data=.d), ~ 1, options=list(infer=c(F,T)))
    )
    names(res)[1:2] = c('label', 'estimate')
    return(res)
}

summary_stat.fixed_effect <- function(overall_stats) {
    if(nlevels(overall_stats$Group) > 1) {
        return(
            combine_emmeans_results(emmeans::emmeans(
                lmerTest::lmer(y ~ Group*Electrode + (1|TrialNumber), data=overall_stats),
                options = list(infer=c(F,T)),
                specs=pairwise~Electrode*Group, infer=c(F,T)
            ))
        )
    } 
    
    combine_emmeans_results(emmeans::emmeans(lmerTest::lmer(y~factor(Electrode)+(1|TrialNumber), data=overall_stats),
        options = list(infer=c(T,T)),
        specs=pairwise~Electrode, infer=c(F,T))
    )
}

get_summary_statistics <- function(overall_stats, analysis_type) {
    switch(analysis_type,
        'Random intercept' = summary_stat.random_intercept(overall_stats),
        'Contrasts per electrode' = summary_stat.contrasts_per_electrode(overall_stats),
        'Collapse electrode' = summary_stat.collapse_electrode(overall_stats),
        'Fixed effect' = summary_stat.fixed_effect(overall_stats)
    )
}
