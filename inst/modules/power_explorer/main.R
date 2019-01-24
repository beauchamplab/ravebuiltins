# Main algorithm - rave_executes

# Initialize inputs
mount_demo_subject()
list2env(as.list(init_module(module_id = 'power_explorer')), globalenv())


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------

requested_electrodes = parse_selections(electrode_text %>% str_replace_all(':', '-'))
requested_electrodes %<>% get_by(`%in%`, electrodes)

# this will be NA if the only requested electrodes are not available
# electrode <- requested_electrodes[1]
assertthat::assert_that(length(requested_electrodes) >= 1 &&
                          all(not_NA(requested_electrodes)), msg = 'No electrode selected')

# ce_main(requested_electrodes)
#baseline all available trials
GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% unlist(g$GROUP)]; g })
has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
any_trials <- any(has_trials)

bl_power <- cache(
  key = list(subject$id, requested_electrodes, BASELINE, any_trials, preload_info),
  val = baseline(power$subset(Electrode = Electrode %in% requested_electrodes),
                 BASELINE[1],  BASELINE[2], hybrid = FALSE, mem_optimize = FALSE)
)

# we were repeating a lot of calculations and looping over GROUPS too many times
# let's clean that up

#helper file to build lists with common elements pre-populated
build_list <- function() {
  ##NB: this is faster than using replicate(length(has_trials))
  lapply(seq_len(length(has_trials)), function(ii)
    list('has_trials' = has_trials[ii],
         'name' = GROUPS[[ii]]$GROUP_NAME))
}

# declare all our variables with pre-populated 'has_trials' and 'name' variables
build_list() ->
  scatter_bar_data -> line_plot_data ->
  by_trial_heat_map_data -> heat_map_data

flat_data <- data.frame()

# load up our collapsers into a list, this allows us to swap them out as needed
collapse <- rave_collapsers.mean()

# swap out the collapsers for medians
if (exists('collapse_using_median')) {
  if(isTRUE(collapse_using_median)) {
    collapse <- rave_collapsers.median()
  }
}

# how should we collapse across electrodes?
.transform <- electrode_transform(combine_method)
#relies on transform as defined above
do_row_transform <- function(.tens) {
  vapply(seq_len(dim(.tens)[3]), function(ei) {
    t(apply(.tens[,,ei], 1, .transform))
  }, FUN.VALUE = .tens[,,1])
}

# now we loop through only those groups with data
# voltage.time_ind <- which(volt$dimnames$Time %within% TIME_RANGE)
# system.time( {
for(ii in which(has_trials)) {
  
  .power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num)
  
  #helper function to clean up the syntax below, value here should be a function
  # we're relying on power being defined above, so don't move this function out of this scope
  `add_data<-` <- function(x, value) {
    x[c('data', 'range', 'N', 'trials')] <-
      list(value, .fast_range(value), dim(.power)[1L], epoch_data$Condition)
    return(x)
  }
  
  if(identical(.transform, IDENTITY_TRANSFORM)) {
    # first build the heat map data
    add_data(heat_map_data[[ii]]) <- .power$collapse(keep = c(3,2), method = 'mean')
    
    # by trial data. Set drop to FALSE b/c we want to keep the electrode dim even if #e ==1
    bt_dat <- .power$subset(Frequency=Frequency %within% FREQUENCY, data_only = FALSE, drop=FALSE)
    add_data(by_trial_heat_map_data[[ii]]) <- bt_dat$collapse(keep = c(3,1), method = 'mean')
    
    # coll freq and trial for line plot w/ ebar. Because we're doing error bars, we have to know whether we have 1 vs. >1 electrodes
    if(dim(.power)[4] == 1) {
      add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(.power)
    } else {
      # here we want the error bars to be across electrodes, rather than across trials
      #NB: take advantage of bt_dat having already subset'd FREQ
      oft <- bt_dat$collapse(keep = c(3,4), method = 'mean')
      add_data(line_plot_data[[ii]]) <- apply(oft, 1, .fast_mse) %>% t
    }
    
    # we want to make a special range for the line plot data that takes into account mean +/- SE
    line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                         line_plot_data[[ii]]$data[,2]))
    
    # scatter bar data
    add_data(scatter_bar_data[[ii]]) <- collapse$over_frequency_and_time(.power, TIME_RANGE, FREQUENCY)
    
    # for the scatter_bar_data we also need to get m_se within condition
    scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)
    
  } else {
    # we need to transform the data before combining
    
    # collapse over trial then row transform
    hmd <- do_row_transform(
      .power$collapse(keep = 2:4, method = 'mean')
    )
    add_data(heat_map_data[[ii]]) <- rutabaga::collapse(hmd, keep=2:1) / dim(hmd)[3]
    
    #collapse over frequency then row transform
    bt_dat <- .power$subset(Frequency=Frequency %within% FREQUENCY, data_only = FALSE, drop=FALSE)
    bthmd <- do_row_transform(
      bt_dat$collapse(keep = c(1,3,4), method = 'mean')
    )
    add_data(by_trial_heat_map_data[[ii]]) <-  rutabaga::collapse(bthmd, keep=2:1) / dim(bthmd)[3]
    
    # collapse over frequency and trial. here we have to consider #elec b/c of the error bars
    if(dim(.power)[4] == 1) {
      add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(.power)
    } else {
      # here we want the error bars to be across electrodes, rather than across trials
      oft <- apply(
        bt_dat$collapse(keep = c(3,4), method = 'mean'),
        2, .transform)
      add_data(line_plot_data[[ii]]) <- apply(oft, 1, .fast_mse) %>% t
    }
    
    # we want to make a special range for the line plot data that takes into account mean +/- SE
    line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                         line_plot_data[[ii]]$data[,2]))
    
    # collapse over freq and time so we get response per trial for scatter bar data.
    # use the bthmd that is already frequency selected and transformed per trial (across time)
    
    # now we want the summary across time \in TIME_RANGE and electrode. one mean per trial
    ind.t <- .power$dimnames$Time %within% TIME_RANGE
    add_data(scatter_bar_data[[ii]]) <- rowMeans(bthmd[,ind.t,])
    
    # for the scatter_bar_data we also need to get m_se within condition
    scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)
  }
  
  # for the scatter_bar_data we also need to get m_se within condition
  scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)
  
  flat_data %<>% rbind(data.frame('group'=ii, 'y' = scatter_bar_data[[ii]]$data))
}

# for baseline you want to have only the baseline times
flat_data$group %<>% factor

# this can be used elsewhere
has_data = sum(has_trials)

# calculate some statistics

# calculate the statistics here so that we can add them to the niml_out
# if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
if(length(unique(flat_data$group)) > 1) {
  # we need to check if they have supplied all identical data sets
  # easy way is to check that the trials are the same?
  g1 <- GROUPS[[which(has_trials)[1]]]$GROUP
  if(all(sapply(which(has_trials)[-1],
                function(ii) identical(GROUPS[ii]$GROUP, g1)))) {
    result_for_suma <-
      get_t(flat_data$y[flat_data$group==flat_data$group[1]])
  } else {
    result_for_suma <- get_f(y ~ group, flat_data)
  }
} else {
  result_for_suma <- flat_data$y %>% get_t
}

attr(scatter_bar_data, 'stats') <- result_for_suma


# Optional, return variables
# rave_return(as.list(environment()))
# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

