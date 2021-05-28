# Main algorithm - rave_executes

# Initialize inputs
# rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
ravebuiltins:::dev_ravebuiltins(T)

mount_demo_subject()
view_layout('power_explorer')
# mount_demo_subject(subject_code = 'YCZ', 'Sentences', epoch='YCZ_gingko', electrodes=50:56, time_range=c(1.5, 4), force_reload_subject=TRUE)

if(FALSE) {
  attachDefaultDataRepository()
  init_module('power_explorer', TRUE)
  frequency_window = c(76,130)
  # frequencies
  # GROUPS = list(list('group_name'='d', group_conditions=c('Dynamic')),
  #               list(group_name = 's', group_conditions=c('Static'))
  # )
  GROUPS = list(list(group_name='A-only', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                list(group_name='V-only', group_conditions=c()),
                list(group_name='AV', group_conditions=c('known_av', 'last_av', 'drive_av', 'meant_av')))
  frequency_window = c(75,150)
  electrode_text = '10-15'
  electrode_text = '1,5-6,8-9'
}

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
# event_of_interest = '2ndWord'
cat2_timestamp <- function() {
  t0 <- proc.time()[3]
  last_time = t0
  return(function(lbl, level='DEBUG') {
    .t = proc.time()[3]
    tick = .t - last_time
    last_time <<- .t
    elapsed = .t - t0
    dipsaus::cat2(lbl, '\t tick ', round(tick,1), ' sec\tTotal: ', round(elapsed,1), ' sec', level=level)
  })
}

if(sum(frequencies %within% frequency_window) < 1) {
  stop('No frequencies available within specified range')
}


cat2t <- cat2_timestamp()

# attributes(GROUPS) <- NULL

requested_electrodes = dipsaus::parse_svec(electrode_text, sep=',|;', connect  = ':-')
requested_electrodes %<>% get_by(`%in%`, electrodes)
# electrodes = preload_info$electrodes
# this will be NA if the only requested electrodes are not available
# electrode <- requested_electrodes[1]
assertthat::assert_that(length(requested_electrodes) >= 1 &&
                          all(not_NA(requested_electrodes)), msg = 'No electrode selected')

# grab the subject code so it can be used later
subject_code = subject$subject_code

assertthat::assert_that(length(GROUPS) > 0,
                        msg = 'Must have at least one group with data')

# Clean group input data
group_data = lapply(seq_along(GROUPS), function(idx) {
  g = GROUPS[[idx]]
  
  Trial_num = epoch_data$Trial[epoch_data$Condition %in% unlist(g$group_conditions)]
  list(
    name = g$group_name,
    Trial_num = Trial_num,
    group_index = idx,
    has_trials = length(Trial_num) > 0,
    conditions = unlist(g$group_conditions)
  )
})
has_trials <- vapply(group_data, function(g){g$has_trials}, FALSE)
any_trials <- any(has_trials)

# Idea for performance -- check if the previous baseline exists in the cache. If so,
# grab it, if not, then recaclulate, but remove the previous one to save on space

calculate_baseline <- function(elecs) {
  if(missing(elecs)) {
    elecs = requested_electrodes
  }
  unit_dims = c(1,2,4)
  
  if(isTRUE(global_baseline)) {
    unit_dims = c(2,4)
  }
  
  els <- power$subset(Electrode = Electrode %in% elecs)
  bl = dipsaus:::baseline_array(
    x = els$get_data(),
    baseline_indexpoints = which(els$dimnames$Time %within% baseline_window),
    along_dim = 3L,
    method = get_unit_of_analysis(unit_of_analysis),
    unit_dims = unit_dims
  )
  
  ECoGTensor$new(bl, dim = dim(els), dimnames = dimnames(els), 
                 varnames = els$varnames, hybrid = F)
}

# Subset data
cat2t('Starting baseline')
bl_power <- cache(
  key = list(subject$id, requested_electrodes, baseline_window,
             preload_info$time_points, unit_of_analysis, global_baseline,
             any_trials, preload_info$epoch_name, preload_info$reference_name),
  val = calculate_baseline(),
  name = 'bl_power'
)
cat2t('Finished baseline')

jitter_seed <- cache(key = 'jitter_seed', val = sample(1:100, 1), name = 'jitter_seed')

# Prepare plot datasets
scatter_bar_data <- over_time_data <- by_electrode_heat_map_data <- 
  by_trial_heat_map_data <- heat_map_data <- group_data

flat_data <- data.frame()

# set transform method
collapse_method = 'mean'

contrast_conditions <- list()

for(ii in which(has_trials)) {
  #ii=1
  cat2t(sprintf('main calc loop %s of %s', ii, sum(has_trials)))
  
  epoch_data_subset <- subset(epoch_data, 
                              epoch_data$Trial %in% group_data[[ii]]$Trial_num)
  
  power_all = bl_power$subset(Trial = Trial %in% group_data[[ii]]$Trial_num)
  
  events = get_events_data(epoch_event_types) %>%
    subset((.)$Trial %in% group_data[[ii]]$Trial_num)
  
  # now check if we need shifted data
  # R is copy on write, so no worries here about memory
  power_all_shifted = power_all
  shift_amount = NULL
  # event_of_interest = '1stWord'
  if(event_of_interest != epoch_event_types[1]) {
    cat2t('Shifting data to: ' %&% event_of_interest)
    new_range = determine_available_shift(event_of_interest, available_time = range(power$dimnames$Time), epoch_information = events)
    cat2t('available shift: ' %&% paste0(new_range, collapse=':'))
    
    shift_amount = determine_shift_amount(event_time = events[[event_of_interest]],
                                          available_shift=new_range)
    
    cat2t('dispaus::shift')
    
    if(length(shift_amount) != dim(power_all)[1L]) {
      assign('shift_amt', shift_amount, envir = globalenv())
      assign('event_mat', events, envir = globalenv())
      stop('shift amount != # trials... stopping')
    }
    
    power_all_shifted = get_shifted_tensor(raw_tensor = power_all$get_data(), shift_amount = shift_amount, new_range = new_range,
                                           dimnames = dimnames(power_all), varnames = names(power_all$dimnames))
    
    # alright, now that we've shifted the data we also need to shift the events dataset, so that future sorts on the event_of_interest don't do anything
    cat2t('updating events file')
    events[epoch_event_types[-1]] <- events[epoch_event_types[-1]] - events[[event_of_interest]]
    cat2t('done with shifting')
  }
  
  # handle outliers
  if(length(trial_outliers_list) == 0) {
    power_all_clean <- power_all
    power_all_shifted_clean <- power_all_shifted
  } else {
    cat2t('Handling outliers...')
    power_all_clean <- power_all$subset(Trial = !(Trial %in% trial_outliers_list))
    power_all_shifted_clean <- power_all_shifted$subset(Trial = !(Trial %in% trial_outliers_list))
  }
  
  N = dim(power_all_shifted)[1L]
  Nclean <- dim(power_all_shifted_clean)[1L]
  
  # This copies over some information from group_data that is needed by particular plots
  # as well as populating data/range 
  wrap_data = function(value, ...){
    ll = list(
      data = value,
      range = .fast_range(value),
      N = N,
      trials = epoch_data_subset$Condition,
      Trial_num = group_data[[ii]]$Trial_num,
      is_clean = !(epoch_data_subset$Trial %in% trial_outliers_list),
      name = group_data[[ii]]$name,
      has_trials = group_data[[ii]]$has_trials,
      conditions = group_data[[ii]]$conditions,
      baseline_window = baseline_window,
      analysis_window = analysis_window,
      stimulation_window = stimulation_window,
      censor_stimulation_window = censor_stimulation_window,
      frequency_window = frequency_window,
      electrodes = requested_electrodes,
      events = events,
      trial_alignment = event_of_interest,
      subject_code=subject_code
    )
    
    vals = list(...)
    
    for (k in c('ylab', 'zlab')) {
      if (isTRUE(vals[[k]] == 'auto')) {
        vals[[k]] = 'Mean ' %&% unit_of_analysis
      }
    }
    
    for(k in names(vals)) {
      # check for attribute labels
      if (k %in% c('xlab', 'ylab', 'zlab')) {
        attr(ll$data, k) = vals[[k]]
      }
      # all other values just add into the data list
      else {
        ll[[k]] = vals[[k]]
      }
    }
    
    return (ll)
  }
  
  # 1. power @ frequency over time
  # this should use the clean and shifted data
  cat2t('Building spectrogam data')
  heat_map_data[[ii]] <- wrap_data(
    power_all_shifted_clean$collapse(keep = c(3,2), method = collapse_method),
    xlab='Time (s)', ylab='Frequency', zlab='auto',
    x = power_all_shifted_clean$dimnames$Time,
    y = power_all_shifted_clean$dimnames$Frequency,
    # hmd is using the clean data
    N = Nclean
  )
  
  if(censor_stimulation_window) {
    heat_map_data[[ii]]$range <- .fast_range(heat_map_data[[ii]]$data[
      !(power_all_shifted_clean$dimnames$Time %within% stimulation_window),]
    )
  }
  
  power_all_shifted_clean_freq_subset = power_all_shifted_clean$subset(Frequency = Frequency %within% frequency_window)
  power_all_shifted_freq_subset = power_all_shifted$subset(Frequency = Frequency %within% frequency_window)
  
  # 2. power @ trial over time
  cat2t('Building by_trial heatmap data')
  by_trial_heat_map_data[[ii]] <- wrap_data(
    power_all_shifted_freq_subset$collapse(keep = c(3,1), method = collapse_method),
    x = power_all_shifted_freq_subset$dimnames$Time,
    y = seq_along(power_all_shifted_freq_subset$dimnames$Trial),
    xlab='Time (s)', ylab='Trial', zlab='auto'
  )
  
  if(censor_stimulation_window) {
    by_trial_heat_map_data[[ii]]$range <- .fast_range(by_trial_heat_map_data[[ii]]$data[
      !(power_all_shifted_freq_subset$dimnames$Time %within% stimulation_window),]
    )
  }
  
  # 2.5 by electrode over time
  cat2t('Building by_electrode_heat_map_data')
  by_electrode_heat_map_data[[ii]] <- wrap_data(
    power_all_shifted_clean_freq_subset$collapse(keep = c(3,4), method = collapse_method),
    x=power_all_shifted_clean_freq_subset$dimnames$Time,
    y=power_all_shifted_clean_freq_subset$dimnames$Electrode,
    xlab='Time (s)', ylab='Electrode', zlab='auto'
  )
  
  if(censor_stimulation_window) {
    by_electrode_heat_map_data[[ii]]$range <- .fast_range(by_electrode_heat_map_data[[ii]]$data[
      !(power_all_shifted_clean_freq_subset$dimnames$Time %within% stimulation_window),]
    )
  }
  
  
  # 3. Time only
  # coll freq and trial for line plot w/ ebar.
  cat2t('Building over_time_data')
  over_time_data[[ii]] = wrap_data(t(
    apply(power_all_shifted_clean_freq_subset$collapse(keep = 3:4, method = collapse_method), 1, .fast_mse)),
    xlab='Time (s)', ylab='auto', N=dim(power_all_shifted_clean_freq_subset)[4L], x=power_all_shifted_clean_freq_subset$dimnames$Time
  )
  
  # set NA (divide by zero) error bars to 0  
  over_time_data[[ii]]$data[is.na(over_time_data[[ii]]$data[,2]),2] <- 0
  
  # we want to make a special range for the line plot data that takes into account mean +/- SE
  
  if(censor_stimulation_window) {
    over_time_data[[ii]]$range <- .fast_range(plus_minus(over_time_data[[ii]]$data[
      !(power_all_shifted_clean_freq_subset$dimnames$Time %within% stimulation_window),]
    ))
  } else {
    over_time_data[[ii]]$range <- .fast_range(plus_minus(over_time_data[[ii]]$data))
  }
  
  if(!all(is.finite(over_time_data[[ii]]$range))) {
    assign('otd_ii',over_time_data[[ii]], envir=globalenv())
    if(any(is.nan(otd_ii$data[,1]))) {
      stop('Unable to plot data, data are NaN')
    } else {
      stop(paste0('non-finite range... ', paste(over_time_data[[ii]]$range, collapse='|')))
    }
  }
  
  # scatter bar data -- here we want all of the data because we are going to highlight (or not) the outliers -- same for by-trial heatmap
  # if(show_outliers_on_plots) {
  cat2t('Building scatter_bar_data')
  
  if(censor_stimulation_window) {
    scatter_bar_data[[ii]] <- wrap_data(
      rowMeans(power_all_shifted_freq_subset$subset(Time = (Time %within% analysis_window) & !(Time %within% stimulation_window), data_only = TRUE)),
      xlab='Group', ylab='auto'
    )
  } else {
    scatter_bar_data[[ii]] <- wrap_data(
      rowMeans(power_all_shifted_freq_subset$subset(Time = (Time %within% analysis_window), data_only = TRUE)),
      xlab='Group', ylab='auto'
    )
  }
  
  # Although this seems to be the wrong place to do this, not sure where else we can do it
  # to enable point identification later, we need to know the x-location of each point. So the jittering
  # needs to be done here.
  .xp <- barplot(which(has_trials),plot=FALSE)
  .r <- if(sum(has_trials)>1) {
    mean(unique(diff(.xp)))*0.25  
  } else {
    0.75*(1/3)
  }
  
  xpi <- which(ii == which(has_trials))
  scatter_bar_data[[ii]]$xp <- .xp[xpi]
  set.seed(jitter_seed)
  scatter_bar_data[[ii]]$x <- .xp[xpi] + runif(length(scatter_bar_data[[ii]]$data), -.r, .r)
  
  # for the scatter_bar_data we also need to get m_se within condition, this is ALWAYS with the clean data
  scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data[scatter_bar_data[[ii]]$is_clean])
  
  # for the analysis, we use the clean data
  nm <- group_data[[ii]]$name
  if(nm == '') {
    nm = 'RAVE_GROUP_' %&% LETTERS[ii]
  }
  flat_data %<>% rbind(
    data.frame('group'= ii,
               'group_name' = nm,
               orig_trial_number = group_data[[ii]]$Trial_num[scatter_bar_data[[ii]]$is_clean],
               'y' = with(scatter_bar_data[[ii]], data[is_clean])
    )
  )
  
  ### add in the stats data
  if(T) {
    nm <- GROUPS[[ii]]$group_name
    if(!isTRUE(nm != "" & !is.null(nm))) {
      nm = 'GROUP_' %&% ii
    }
    cat2t('Calc subset analyses')
    if(censor_stimulation_window) {
      m = power_all_shifted_clean_freq_subset$subset(Time = (Time %within% analysis_window) & !(Time %within% stimulation_window))$collapse(keep=c(1,4))
    } else {
      m = power_all_shifted_clean_freq_subset$subset(Time = Time %within% analysis_window)$collapse(keep=c(1,4))
    }
    
    els = power_all_shifted_clean_freq_subset$dimnames$Electrode
    
    if(exists('.__DEBUG__')) {
      assign('vals', list(
        contrast_conditions,
        m, els, nm, power_all_shifted_clean_freq_subset$dimnames$Trial
      ), envir = global_env())
    }
    
    contrast_conditions[[nm]] = data.frame(
      y = c(m),
      Group = nm,
      Electrode = rep(els, each = nrow(m)),
      TrialNumber = rep(
        power_all_shifted_clean_freq_subset$dimnames$Trial,
        times = length(els)
      )
    )
    
  }
}

flat_data$group_i = flat_data$group
flat_data$group %<>% factor


overall_stats <- do.call(rbind, contrast_conditions)

overall_stats$Group %<>% factor(levels = names(contrast_conditions))
overall_stats$Electrode %<>% factor


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
eat <- electrode_analysis_type
if(nlevels(overall_stats$Electrode) == 1) {
  dipsaus::cat2('Forcing etype to collapse electrode because n=1')
  eat = 'Collapse electrode'
}
cat2t('running elec stats as: ' %&% eat)

summary_statistics <- switch(
  eat,
  'Random intercept' = {
    
    if(nlevels(overall_stats$Group) > 1) {
      combine_emmeans_results(
        emmeans::emmeans(
          lmerTest::lmer(y ~ Group + (1|Electrode), data = overall_stats),
          options = list(infer=c(F,T)),
          specs = pairwise ~ Group, infer=c(F,T))
      )
    } else {
      r <- as.data.frame(emmeans::emmeans(lmerTest::lmer(
        y ~ 1 + (1|Electrode), data=overall_stats
      ), specs=~1, options=list(infer=c(F,T))))
      names(r)[1:2] = c('label', 'estimate')
      r
    }
  },
  'Contrasts per electrode' = {
    if(nlevels(overall_stats$Group) > 1) {
      combine_emmeans_results(
        emmeans::emmeans(
          lmerTest::lmer(y ~ Group*Electrode + (1|Electrode), data = overall_stats),
          options = list(infer=c(F,T)),
          specs = pairwise ~ Group|Electrode, infer=c(F,T))
      )
    } else {
      r <- as.data.frame(emmeans::emmeans(
        lmerTest::lmer(y ~ Electrode + (1|TrialNumber), data=overall_stats)
        , specs = pairwise ~ Electrode, options=list(infer=c(F,T)))$emmeans
      )
      names(r)[1:2] = c('label', 'estimate')
      r$label = 'E' %&% r$label
      r
    }
  },
  'Collapse electrode' = {
    .d <- aggregate(y ~ TrialNumber+Group, mean, data=overall_stats)
    if(nlevels(.d$Group) > 1) {
      combine_emmeans_results(emmeans::emmeans(lm(y ~ Group, data=.d), 
                                               options = list(infer=c(F,T)),
                                               pairwise ~ Group, infer=c(F,T)))
    } else {
      r = as.data.frame(
        emmeans::emmeans(lm(y ~ 1, data=.d), ~ 1, options=list(infer=c(F,T)))
      )
      names(r)[1:2] = c('label', 'estimate')
      r
    }
  },
  'Fixed effect' = {
    if(nlevels(overall_stats$Group) > 1) {
      combine_emmeans_results(emmeans::emmeans(
        lmerTest::lmer(y ~ Group*Electrode + (1|TrialNumber), data=overall_stats),
        options = list(infer=c(F,T)),
        specs=pairwise~Electrode*Group, infer=c(F,T)
      ))
    } else {
      combine_emmeans_results(emmeans::emmeans(lmerTest::lmer(y~factor(Electrode)+(1|TrialNumber), data=overall_stats),
                                               options = list(infer=c(T,T)),
                                               specs=pairwise~Electrode, infer=c(F,T))
      )
    }
  }
)
cat2t('done')

local_data$summary_statistics = summary_statistics

# this can be used elsewhere to quickly check the number of groups that have data
has_data = sum(has_trials)

all_trial_types <- GROUPS %>% lapply(`[[`, 'group_conditions') %>% unlist %>% unique

get_p.adjust_method <- function(pval_filter=c('p', 'FDR(p)', 'Bonf(p)')) {
  pval_filter = match.arg(pval_filter)
  c('p'='none', 'FDR(p)'='fdr', 'Bonf(p)'='bonferroni')[pval_filter]
}


# calculate some statistics across electrodes
# we need the omnibus result per-electrode, do for all electrodes, not just selected
get_stats_per_electrode <- function(ttypes){
  # ttypes = all_trial_types
  trial_numbers = epoch_data$Trial[epoch_data$Condition %in% ttypes]
  
  if(length(trial_outliers_list) > 0) {
    trial_numbers <- trial_numbers[! trial_numbers %in% trial_outliers_list]
  }
  
  unit_dims = c(1,2,4)
  if(isTRUE(global_baseline)) {
    unit_dims = c(2,4)
  }
  baseline_method = get_unit_of_analysis(unit_of_analysis)
  
  # see if there are contrasts to run
  group_f = NULL
  if(has_data > 1) {
    # assign('flat_data', flat_data, envir = global_env())
    # assign('GROUPS', GROUPS, envir = global_env())
    # assign('trial_numbers', trial_numbers, envir = global_env())
    
    group = flat_data$group_i
    gnames = flat_data$group_name #GROUPS[unique(flat_data$group_i)] %>% sapply('[[', 'group_name')
    # if(any(gnames == '')) {
    #   gnames[gnames == ''] = paste0('rave_group_', LETTERS[which(gnames=='')])
    # }
    # group_f = gnames[group]
    df_shell = data.frame(group=factor(gnames), flat_data$orig_trial_number)
    
    # we are doing this here instead of when lmmeans is called because that is called inside the lapply_async
    # the critical thing is the call to factory above that ensure the levels are ordered based on how they are entered
    # into the condtion groups.
    lbls = build_group_contrast_labels(levels(df_shell$group))
  }
  
  # Do not baseline all elecs simultaneously, otherwise memory will explode
  cat2t('starting elec calc')
  # e = electrodes[1]
  
  shift_amount = new_range = NULL
  if(event_of_interest != epoch_event_types[1]) {
    ev = get_events_data(epoch_event_types = epoch_event_types) %>% subset((.)$Trial %in% trial_numbers)
    new_range = determine_available_shift(event_of_interest,
                                          available_time = range(power$dimnames$Time),
                                          epoch_information = ev)
    
    shift_amount = determine_shift_amount(event_time = ev[[event_of_interest]], available_shift=new_range)
  }
  
  res = #lapply(electrodes, function(e, ...){
    rave::lapply_async(electrodes, function(e){
      # Subset on electrode is memory optimized, and is fast
      el = power$subset(Electrode = Electrode == e,
                        Frequency = Frequency %within% frequency_window,
                        Trial=Trial %in% trial_numbers
      )
      # because of possible time re-alignment, we can't just take the analysis window :(, slow but true!
      #,                      Time = (Time %within% analysis_window) | (Time %within% baseline_window)
      bl = dipsaus::baseline_array(
        x = el$get_data(),
        baseline_indexpoints = which(el$dimnames$Time %within% baseline_window),
        along_dim = 3L,
        method = baseline_method,
        unit_dims = unit_dims
      )
      
      # do we need to shift the array?
      if(!is.null(shift_amount)) {
        bl = get_shifted_tensor(bl, shift_amount, new_range = new_range,
                                dimnames = dimnames(el), varnames = el$varnames)
      } else {
        bl = ECoGTensor$new(bl, dim = dim(el), dimnames = dimnames(el),
                            varnames = el$varnames, hybrid = FALSE)
      }
      
      if(censor_stimulation_window) {
        bl.analysis <- bl$subset(Time=(Time %within% analysis_window) & !(Time %within% stimulation_window))
      } else {
        bl.analysis <- bl$subset(Time=Time %within% analysis_window)
      }
      
      trial_means = rowMeans(bl.analysis$get_data())
      names(trial_means) = as.character(bl.analysis$dimnames$Trial)
      
      mse = .fast_mse(trial_means)
      t = mse[1]/mse[2]
      p = 2*pt(abs(t), df = length(trial_means)-1, lower=F)
      res = rbind(mse[1], t, p)
      
      # now we also need to run the contrasts
      if(has_data > 1) {
        df2 = df_shell
        df2$y = trial_means[as.character(df_shell$flat_data.orig_trial_number)]
        # df2$group_f %<>% factor(levels = gnames)
        
        .lsm <- emmeans::emmeans(lm(y ~ group, data=df2), pairwise ~ group)
        lmat = matrix(c(t(summary(.lsm$emmeans, infer = T)[c('emmean', 't.ratio', 'p.value')])))
        cntr = summary(.lsm, adjust='none')$contrasts
        cmat = as.matrix(c(t(as.matrix(cntr[,c('estimate','t.ratio', 'p.value')]))))
        
        res = rbind(res, lmat, cmat)
      }
      return(res)
    } ,
    .globals = c('baseline_array', 'baseline_method', 'unit_dims', 'electrodes', 'e', 'gnames', 'has_data', 'shift_amount', 'new_range',
                 'trial_numbers', 'frequency_window', 'analysis_window', 'baseline_window', '.fast_mse', 'df_shell'),
    .gc = FALSE)
  
  cat2t('Finished elec calc')
  
  combined_res = do.call('cbind', res)
  
  rownames(combined_res)[1] <- format_unit_of_analysis_name(unit_of_analysis)
  colnames(combined_res) <- electrodes
  
  # do we need to adjust the p-values? yes, just do this always
  adjusted_p = p.adjust(combined_res[3,], method=get_p.adjust_method(p_filter))
  # print("pval adjustment:")
  # print(m_sd(adjusted_p-combined_res[3,]))
  
  if(nrow(combined_res) > 3) {
    stat_lbls = c('m_', 't_', 'p_')
    tmp = combined_res[4:nrow(combined_res),]
    # add the names for these folks
    rownames(tmp) = c(outer(stat_lbls, levels(df_shell$group), paste0), outer(stat_lbls, lbls, paste0))
    
    combined_res = rbind(combined_res[1:3,], adjusted_p, tmp)
  } else {
    combined_res %<>% rbind(adjusted_p)
  }
  rownames(combined_res)[4] = p_filter
  
  cat2t('Stats are labelled')
  
  return(combined_res)
}

cat2t('start calc result')

if(any(duplicated(sapply(GROUPS, `[[`, 'group_name')))) {
  if(shiny_is_running()) {
    shiny::showNotification('Duplicate group names are not allowed', type = 'error')
    shiny::req(FALSE)
  } else {
    stop('Duplicate group names are not allowed')
  }
}

# assign("GROUPS", GROUPS, envir = globalenv())
omnibus_results <- cache(
  key = list(subject$id, baseline_window, frequency_window, all_trial_types, GROUPS, global_baseline,
             analysis_window, unit_of_analysis, preload_info$epoch_name, event_of_interest,
             preload_info$reference_name, trial_outliers_list),
  val = get_stats_per_electrode(ttypes = all_trial_types),
  name = 'omnibus_results'
)

# grab all the details needed for plotting and put them in a list that
# can be passed around (and I guess modified?)

# plot_options <- build_plot_options()
## is this safe???
# to_copy = names(plot_options) %in% ls(envir=globalenv())
# if(any(to_copy)) {
#   for(ii in which(to_copy)) {
#     plot_options[[names(plot_options)[ii]]] = get(names(plot_options)[ii], envir = global_env())
#   }
# }

# plot time range isn't being set correctly...
# plot_options$plot_time_range = plot_time_range
# plot_options$sort_trials_by_type = sort_trials_by_type
# plot_options$draw_decorator_labels = draw_decorator_labels
# local_data$plot_options = plot_options


# load up local data the plot data and the plot options
local_data$over_time_data = over_time_data
local_data$heat_map_data = heat_map_data
local_data$scatter_bar_data = scatter_bar_data
local_data$by_electrode_heat_map_data = by_electrode_heat_map_data
local_data$by_trial_heat_map_data = by_trial_heat_map_data
local_data$electrodes_csv = electrodes_csv

if(!is.null(local_data$current_active_set)) {
  pass = electrodes %in% local_data$current_active_set
  cas = matrix(as.integer(pass), nrow=1) %>%
    set_colnames(electrodes) %>% set_rownames('Passes_Filters')
  omnibus_results %<>% rbind(cas)
}

omnibus_results %<>% rbind(
  matrix(as.integer(electrodes %in% requested_electrodes), nrow=1, dimnames = list('Selected_Electrodes', electrodes))
)

local_data$omnibus_results = omnibus_results

### we need to store the plot options 

# flat_data
# local_data$condition_stats

cat2t('Finished calc')

if(rave::rave_context()$context %in% c('rave_running', 'default')) {
  dipsaus::cat2("Update 3D viewer")
  btn_val = isolate(input[['power_3d_btn']]) - 0.001
  dipsaus::set_shiny_input(session = session, inputId = 'power_3d_btn',
                           value = btn_val, method = 'proxy', priority = 'event')
}

cat2t('Done in main')

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

###tricky
if(FALSE) {
  init_module('power_explorer', TRUE)
  results <- list(
    get_value = function(key, ifNotFound=0) {
      get0(key, envir = globalenv(), ifnotfound = ifNotFound)  
    }
  )
}

# Debug
rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
.__DEBUG__ = 1
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject(force_reload_subject = T)
view_layout('power_explorer')

reload_module_package()

module = rave::get_module(module='power_explorer', package = 'ravebuiltins', local=TRUE)

# eval_when_ready %?<-% function(FUN, ...) {FUN(...)}
# attachDefaultDataRepository() 
result = module(electrode_text = '14', percentile_range = TRUE, 
                GROUPS = list(
                  list(group_name='AUD', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
                  list(group_name='AV', group_conditions=c('known_av', 'last_av', 'drive_av', 'meant_av')),
                  list(group_name='VIS', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))
                ),
                background_plot_color_hint='white',
                baseline_window = c(-1,-.4),
                analysis_window = c(0.5,1),
                heatmap_color_palette = 'BlueWhiteRed',
                max_column_heatmap = 2,
                # plot_time_range = c(-1.5,3),
                unit_of_analysis = 'decibel',
                show_stimulation_window = TRUE,
                censor_stimulation_window = TRUE,
                stimulation_window = c(0., 0.3),
                frequency_window = c(70,150), show_outliers_on_plots = TRUE,
                max_zlim=99
                # ,                event_of_interest = '1stWord'
)
results = result$results

by_trial_heat_map_plot(results)

by_electrode_heat_map_plot(results)
assess_normality_plot(results)

# results$get_variables(level = 3)

across_electrode_statistics_plot(results)

assess_stability_over_time_plot(results)
over_time_plot(results)
by_electrode_heat_map_plot(results)

results$get_value('flat_data')

mount_demo_subject()

env = reload_this_package(expose = FALSE, clear_env = TRUE)

# Step 2: make sure rave data is attached
attachDefaultDataRepository()

# Step 3: try to run from local session
module = rave::get_module(package = 'ravebuiltins',
                          module_id = 'power_explorer', local = T)

res = module()

# Step 4: launch modules in RAVE (production)
# Cmd+Shift+B
m = rave::detect_modules('ravebuiltins')
rave::start_rave()


