# Main algorithm - rave_executes

# Initialize inputs
# rm(list = ls(all.names=T)); rstudioapi::restartSession()
require(ravebuiltins)
ravebuiltins:::dev_ravebuiltins(T)

mount_demo_subject(electrodes = 10:25)

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
  frequency_window2 = c(1,40)
  enable_frequency_window2 <- FALSE
  enable_event_of_interest2 <- FALSE
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
  val = calculate_baseline(requested_electrodes),
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


## handling separate frequencies
# we need to keep track of the dependent variables.
# What are the relevant windows of time and frequency,
# what is time-0 for the analysis etc
f1_analysis_settings <- list(
  frequency_window = frequency_window,
  analysis_window = analysis_window,
  unit_of_analysis = unit_of_analysis,
  event_of_interest = event_of_interest,
  baseline_window = baseline_window,
  collapse_method = collapse_method,
  do_censor = censor_stimulation_window,
  censor_window = stimulation_window
)

# first set equality to get shared parameters
f2_analysis_settings <- f1_analysis_settings

if(f2_analysis_settings$enabled <- enable_frequency_window2) {
  
  f2_analysis_settings$frequency_window = frequency_window2    
  
  if(enable_analysis_window2) {
    f2_analysis_settings$analysis_window = analysis_window2    
  }
  # this is just a drop-down, so it feels simpler to just assign rather than 
  # having the check box
    f2_analysis_settings$event_of_interest = event_of_interest2
}

for(ii in which(has_trials)) {
# ii <- 1  
  group_info <- list(current_group=ii, group_statuses=has_trials)
  
  #ii=1
  cat2t(sprintf('main calc loop %s of %s', ii, sum(has_trials)))
  
  # subset the event data to be specific to the current trial group
  events = get_events_data(epoch_event_types) %>% subset((.)$Trial %in% group_data[[ii]]$Trial_num)
  
  epoch_data_subset <- subset(epoch_data, epoch_data$Trial %in% group_data[[ii]]$Trial_num)
  
  power_data <- get_pluriform_power(
    baselined_data=bl_power,
    trial_indices = group_data[[ii]]$Trial_num,
    events = events,
    epoch_event_types = epoch_event_types,
    trial_outliers_list=trial_outliers_list,
    event_of_interest = event_of_interest,
    logger = cat2t
  )
  
  # re-assign the events list, in case it changed
  events = power_data$events
  
  N = dim(power_data$shifted_data)[1L]
  Nclean <- dim(power_data$shifted_clean_data)[1L]
  
  # create versions for each frequency
  power_data$shifted_data_F1 = power_data$shifted_data$subset(Frequency = Frequency %within% frequency_window)
  power_data$shifted_clean_data_F1 = power_data$shifted_clean_data$subset(Frequency = Frequency %within% frequency_window)
  
  # are we using multiple frequencies? create versions of each data type for the second frequency
  if(enable_frequency_window2) {
    power_data$shifted_data_F2 = power_data$shifted_data$subset(Frequency = Frequency %within% frequency_window2)
    power_data$shifted_clean_data_F2 = power_data$shifted_clean_data$subset(Frequency = Frequency %within% frequency_window2)
  }
  
  # This copies over some information from group_data that is needed by particular plots
  # as well as populating data/range 
  
  # params <- ?fastmap::fastmap()
  
    # self.params = list(),
    
    # wrap_data = function(data, ...) {
    #   params$self.params
    # }
  # )
  
  wrap_data = function(data, ...){
    ll <- list(
      data = data,
      range = .fast_range(data),
      N = N,
      trials = epoch_data_subset$Condition,
      Trial_num = group_data[[ii]]$Trial_num,
      is_clean = !(epoch_data_subset$Trial %in% trial_outliers_list),
      name = group_data[[ii]]$name,
      has_trials = group_data[[ii]]$has_trials,
      conditions = group_data[[ii]]$conditions,
      stimulation_window = stimulation_window,
      censor_stimulation_window = censor_stimulation_window,
      electrodes = requested_electrodes,
      events = events,
      trial_alignment = event_of_interest,
      subject_code=subject_code,
      group_info = group_info
    )
    
    ## by default we use the f1 setttings
    ll[names(f1_analysis_settings)] = f1_analysis_settings
    
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

    # common arguments for all data types 
  ._F1_BUILDER <- {
    list(
      heat_map_data = function() {
        list('F1' = build_heatmap_data(data=power_data$shifted_clean_data,
          data_wrapper = wrap_data, analysis_settings = f1_analysis_settings),
          'F1_F2' = build_heatmap_correlation_data(data=power_data$shifted_clean_data,
            data_wrapper = wrap_data, analysis_settings = f1_analysis_settings,
            analysis_settings2 = f1_analysis_settings)
        )
      },
      by_trial_heat_map_data = function() {
        list('F1' = build_by_trial_heatmap_data(data=power_data$shifted_data_F1,
          data_wrapper = wrap_data, analysis_settings = f1_analysis_settings)
        )
      },
      by_electrode_heat_map_data = function() {
        list('F1' = build_electrode_heatmap_data(data=power_data$shifted_clean_data_F1,
          data_wrapper = wrap_data, analysis_settings = f1_analysis_settings)
        )
      },
      over_time_data = function() {
        list('F1' = build_over_time_data(data=power_data$shifted_clean_data_F1,
          data_wrapper = wrap_data, analysis_settings = f1_analysis_settings)
        )
      },
      scatter_bar_data = function() {
        list('F1' = build_scatter_bar_data(data=power_data$shifted_data_F1, 
          data_wrapper = wrap_data, analysis_settings = f1_analysis_settings, group_info = group_info, jitter_seed = jitter_seed)
          )
      }
    )
  }
  
  ._F2_BUILDER <- {
    list(
      heat_map_data = function() {
        res = list(
          'F1' = ._F1_BUILDER$heat_map_data()$F1,
          'F1_F2' = build_heatmap_correlation_data(data=power_data$shifted_clean_data,
            data_wrapper = wrap_data, analysis_settings = f1_analysis_settings,
            analysis_settings2 = f2_analysis_settings)
        )
        
        res$F1$enable_frequency2 = TRUE
        res$F1$frequency_window2 = f2_analysis_settings$frequency_window
        res$F1$analysis_window2 = f2_analysis_settings$analysis_window
        
        return(res)
      },
      by_trial_heat_map_data = function() {
        list(
          'F1' = ._F1_BUILDER$by_trial_heat_map_data()$F1,
          'F2' = build_by_trial_heatmap_data(data=power_data$shifted_data_F2, analysis_settings = f2_analysis_settings,
            data_wrapper = wrap_data)
        )
      },
      by_electrode_heat_map_data = function() {
        list(
          'F1' = ._F1_BUILDER$by_electrode_heat_map_data()$F1,
          'F2' = build_electrode_heatmap_data(data=power_data$shifted_clean_data_F2,
            analysis_settings=f2_analysis_settings, data_wrapper = wrap_data)
        )
      },
      over_time_data = function() {
        res <- list(
          'F1' = ._F1_BUILDER$over_time_data()$F1,
          'F2' = build_over_time_data(data=power_data$shifted_clean_data_F2, 
            analysis_settings=f2_analysis_settings, data_wrapper = wrap_data)
        )
        
        res$F1_F2 = build_over_time_correlation_data(res$F1, res$F2)
        return(res)
      },
      scatter_bar_data = function() {
        res <- list(
          'F1' = ._F1_BUILDER$scatter_bar_data()$F1,
          'F2' = build_scatter_bar_data(data=power_data$shifted_data_F2, data_wrapper = wrap_data,
              analysis_settings = f2_analysis_settings, group_info = group_info, jitter_seed = jitter_seed)
        )
        
        res$F1_F2 = build_scatter_bar_correlation_data(res$F1, res$F2, data_wrapper = wrap_data)
        
        return(res)
      }
    )
  }
  
  data_builder_helper <- if(enable_frequency_window2) {
    ._F2_BUILDER
  } else {
    ._F1_BUILDER
  }
  
  # 1. power @ frequency over time
  cat2t('Building spectrogam data')
  # this should use the clean and shifted data
  heat_map_data[[ii]] <- data_builder_helper$heat_map_data()
  
  # 2. power @ trial over time
  cat2t('Building by_trial heatmap data')
  # here we're using all (clean and unclean) data, time-shifted, Freq-subset'd
  by_trial_heat_map_data[[ii]] <- data_builder_helper$by_trial_heat_map_data()
  
  # 2.5 by electrode over time
  cat2t('Building by_electrode_heat_map_data')
  by_electrode_heat_map_data[[ii]] <- data_builder_helper$by_electrode_heat_map_data()
  
  # 3. Time only
  # coll freq and trial for line plot w/ ebar.
  cat2t('Building over_time_data')
  over_time_data[[ii]] = data_builder_helper$over_time_data()
  
  # scatter bar data -- here we want all of the data because we are going to highlight (or not) the outliers -- same for by-trial heatmap
  cat2t('Building scatter_bar_data')
  scatter_bar_data[[ii]] <- data_builder_helper$scatter_bar_data()
  
  # for the analysis, we use the clean data
  nm <- group_data[[ii]]$name
  if(nm == '') {
    nm = 'RAVE_GROUP_' %&% LETTERS[ii]
  }
  
  build_df <- function(sbd) {
      # changing this to _not_ just use clean data
      data.frame('group'= ii,
                 'group_name' = nm,
                 'orig_trial_number' = group_data[[ii]]$Trial_num,
                 'trial_type' = sbd$trials,
                 'y' = sbd$data,
                 'is_clean' = sbd$is_clean,
                 'freq' = paste0(sbd$frequency_window, collapse='-') %&% ' Hz'
      )
  }

  # add on the F1 data
  flat_data %<>% rbind(
    build_df(scatter_bar_data[[ii]]$F1)
  )
  
  # now the F2 data
  if(enable_frequency_window2) {
    flat_data %<>% rbind(
      build_df(scatter_bar_data[[ii]]$F2)
    )
  }
  
  ### add in the stats data
  if(T) {
    nm <- GROUPS[[ii]]$group_name
    if(!isTRUE(nm != "" & !is.null(nm))) {
      nm = 'GROUP_' %&% ii
    }
    cat2t('Calc subset analyses')
    if(censor_stimulation_window) {
      m = power_data$shifted_clean_data_F1$subset(Time = (Time %within% analysis_window) & !(Time %within% stimulation_window))$collapse(keep=c(1,4))
    } else {
      m = power_data$shifted_clean_data_F1$subset(Time = Time %within% analysis_window)$collapse(keep=c(1,4))
    }
    
    els = power_data$shifted_clean_data_F1$dimnames$Electrode#  _shifted_clean_freq_subset$dimnames$Electrode
    
    if(exists('.__DEBUG__')) {
      assign('vals', list(
        contrast_conditions,
        m, els, nm, power_data$shifted_clean_data_F1$dimnames$Trial
      ), envir = globalenv())
    }
    
    contrast_conditions[[nm]] = data.frame(
      y = c(m),
      Group = nm,
      Electrode = rep(els, each = nrow(m)),
      TrialNumber = rep(
        power_data$shifted_clean_data_F1$dimnames$Trial,
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

eat <- electrode_analysis_type
if(nlevels(overall_stats$Electrode) == 1) {
  dipsaus::cat2('Forcing etype to collapse electrode because n=1')
  eat = 'Collapse electrode'
}
cat2t('running elec stats as: ' %&% eat)

summary_statistics <- get_summary_statistics(overall_stats, eat)
cat2t('done')

local_data$summary_statistics = summary_statistics

# this can be used elsewhere to quickly check the number of groups that have data
has_data = sum(has_trials)

all_trial_types <- GROUPS %>% lapply(`[[`, 'group_conditions') %>% unlist %>% unique

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
    
    # group = flat_data$group_i
    no_outliers <- flat_data %>% subset((.)$is_clean)
    gnames = no_outliers$group_name #GROUPS[unique(flat_data$group_i)] %>% sapply('[[', 'group_name')
    # if(any(gnames == '')) {
    #   gnames[gnames == ''] = paste0('rave_group_', LETTERS[which(gnames=='')])
    # }
    # group_f = gnames[group]
    df_shell = data.frame(group=factor(gnames), no_outliers$orig_trial_number)
    
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
    rave::lapply_async3(electrodes, function(e){
      # Subset on electrode is memory optimized, and is fast
      # e = electrodes[1]
      # we need to figure out if there are two frequencies being measured
      requested_frequencies <- f1_analysis_settings$frequency_window
      if(f2_analysis_settings$enabled) {
        requested_frequencies <- c(requested_frequencies, f2_analysis_settings$frequency_window)
      }
      
      el = power$subset(Electrode = Electrode == e,
                        Frequency = Frequency %within% requested_frequencies,
                        Trial=Trial %in% trial_numbers
      )
      # because of possible time re-alignment, we can't just take the analysis window :(, slow but true!
      #,                      Time = (Time %within% analysis_window) | (Time %within% baseline_window)
      bl = dipsaus::baseline_array(
        x = el$get_data(),
        baseline_indexpoints = which(el$dimnames$Time %within% f1_analysis_settings$baseline_window),
        along_dim = 3L,
        method = baseline_method,
        unit_dims = unit_dims
      )
      
      # do we need to shift the array? ##TODO update for F2 when ready
      if(!is.null(shift_amount)) {
        bl = get_shifted_tensor(bl, shift_amount, new_range = new_range,
                                dimnames = dimnames(el), varnames = el$varnames)
      } else {
        bl = ECoGTensor$new(bl, dim = dim(el), dimnames = dimnames(el),
                            varnames = el$varnames, hybrid = FALSE)
      }
      
      if(f1_analysis_settings$do_censor) {
        bl.analysis <- bl$subset(Time=(Time %within% analysis_window) & !(Time %within% stimulation_window))
      } else {
        bl.analysis <- bl$subset(Time=Time %within% analysis_window)
      }
      
      do_analysis <- function(dd) {
        trial_means = rowMeans(dd)
        names(trial_means) = as.character(bl.analysis$dimnames$Trial)
        
        mse = .fast_mse(trial_means)
        t = mse[1]/mse[2]
        p = 2*pt(abs(t), df = length(trial_means)-1, lower.tail = F)
        
        res = rbind(mse[1], t, p)
        
        # now we also need to run the contrasts
        if(has_data > 1) {
          df2 = df_shell
          df2$y = trial_means[as.character(df_shell$no_outliers.orig_trial_number)]
          # df2$group_f %<>% factor(levels = gnames)
          
          .lsm <- emmeans::emmeans(lm(y ~ group, data=df2), pairwise ~ group)
          lmat = matrix(c(t(summary(.lsm$emmeans, infer = TRUE)[c('emmean', 't.ratio', 'p.value')])))
          cntr = summary(.lsm, adjust='none')$contrasts
          cmat = as.matrix(c(t(as.matrix(cntr[,c('estimate','t.ratio', 'p.value')]))))
          
          res = rbind(res, lmat, cmat)
        }
        
        return(res)
      }
      
      if(f2_analysis_settings$enabled) {
        res <- list('F1' = do_analysis(bl.analysis$subset(Frequency = Frequency %within% f1_analysis_settings$frequency_window)$get_data()),
          'F2' = do_analysis(bl.analysis$subset(Frequency = Frequency %within% f2_analysis_settings$frequency_window)$get_data())
        )
      } else {
        res <- list("F1" = do_analysis(dd=bl.analysis$get_data()))
      }
      
      return(res)
    } ,
    .globals = c('baseline_array', 'baseline_method', 'unit_dims', 'electrodes', 'e',
      'gnames', 'has_data', 'shift_amount', 'new_range', 'trial_outliers_list',
                 'trial_numbers', 'f1_analysis_settings', 'f2_analysis_settings', '.fast_mse', 'df_shell'))
  
  cat2t('Finished elec calc')

  apply_labels <- function(mat) {
    rownames(mat)[1] <- format_unit_of_analysis_name(unit_of_analysis)
    colnames(mat) <- electrodes
    
    # do we need to adjust the p-values? yes, just do this always
    adjusted_p = p.adjust(mat[3,], method=get_p.adjust_method(p_filter))
    
    if(nrow(mat) > 3) {
      stat_lbls = c('m_', 't_', 'p_')
      tmp = mat[4:nrow(mat),,drop = FALSE]
      # add the names for these folks
      rownames(tmp) = c(outer(stat_lbls, levels(df_shell$group), paste0), outer(stat_lbls, lbls, paste0))
      
      mat = rbind(mat[1:3,,drop=FALSE], adjusted_p, tmp)
    } else {
      mat %<>% rbind(adjusted_p)
    }
    rownames(mat)[4] = p_filter
    
    mat  
  }
  
  combined_res = apply_labels(do.call('cbind', lapply(res, `[[`, 'F1')))
  
  # get the F2 names figured out
  if(f2_analysis_settings$enabled) {
    f2_res <- apply_labels(do.call('cbind', lapply(res, `[[`, 'F2')))
    rownames(f2_res) = 'F2_' %&% rownames(f2_res)
    
    rownames(combined_res) = 'F1_' %&% rownames(combined_res)
    
    combined_res %<>% rbind(f2_res)
  }
  
  # print("pval adjustment:")
  # print(m_sd(adjusted_p-combined_res[3,]))
  
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
  key = list(subject$id, f1_analysis_settings, f2_analysis_settings, all_trial_types, GROUPS, global_baseline,
             unit_of_analysis, preload_info$epoch_name,
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
rm(list = ls(all.names=T))
rstudioapi::restartSession()
require(ravebuiltins)
.__DEBUG__ = 1
# options(warn=0)
ravebuiltins:::dev_ravebuiltins(T)
mount_demo_subject(force_reload_subject = T)

view_layout('power_explorer', theme='black')

reload_module_package()
module = rave::get_module(module='power_explorer', package = 'ravebuiltins', local=TRUE)

# eval_when_ready %?<-% function(FUN, ...) {FUN(...)}
# attachDefaultDataRepository() 
 result = module(electrode_text = '14:100', percentile_range = TRUE, 
  # GROUPS = list(
    # list(group_name='AUD', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a'))
    # list(group_name='AV', group_conditions=c('known_av', 'last_av', 'drive_av', 'meant_av')),
    # list(group_name='VIS', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))
  # ),
  background_plot_color_hint='white',
  draw_decorator_labels = TRUE,
  baseline_window = c(-1,-.4),
  analysis_window = c(0,0.55),
  heatmap_color_palette = 'BlueWhiteRed',
  max_column_heatmap = 3,
  # plot_time_range = c(-1.5,3),
  unit_of_analysis = 'z-score Amplitude',
  show_stimulation_window = F,
  censor_stimulation_window = F,
  stimulation_window = c(0., 0.3),
  frequency_window = c(70,150),
  # trial_outliers_list = sample(1:280, 50),
  show_outliers_on_plots = TRUE,
  max_zlim=99,
   sort_trials_by_type = '2ndWord',
   event_of_interest = '2ndWord',
  enable_frequency_window2=TRUE
)


 results = result$results

# result$results$get_variables()
# dev.off()
heat_map_plot(results)

windowed_comparison_plot(results)

frequency_correlation_plot(results)

by_trial_heat_map_plot(results)

# by_electrode_heat_map_plot(results)

assess_normality_plot(results)
over_time_plot(results)
# results$get_variables(level = 3)

heat_map_plot(results)

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


