input <- getDefaultReactiveInput()
output = getDefaultReactiveOutput()

power_3d_fun = function(need_calc, side_width, daemon_env, viewer_proxy, ...){
  showNotification(p('Rebuild 3d viewer...'), id='power_3d_fun')
  brain = rave::rave_brain2(subject = subject)
  
  if(is.null(brain)){
    rave::close_tab('power_explorer', 'Results on surface')
    showNotification('No surface file is available...', duration=2)
  }
  
  shiny::validate(shiny::need(!is.null(brain), message = 'No surface/volume file found!'))
  re = NULL
  
  display_side = isTRUE(isolate(input$power_3d_widget_side_display))
  
  zoom_level = shiny::isolate(viewer_proxy$main_camera$zoom)
  controllers = viewer_proxy$get_controllers()
  
  if(isolate(input$synch_3d_viewer_bg)) {
    bgcolor = isolate(input$background_plot_color_hint)
    if(bgcolor == 'gray') {
      bgcolor = '#1E1E1E'
    } else {
      bgcolor %<>% col2hex
    }
    controllers[['Background Color']] = bgcolor
  }
  
  if( need_calc ){
    or = local_data$omnibus_results#cache(name='omnibus_results')
    
    values = data.frame(t(or))
    values$Subject = as.factor(subject$subject_code)
    values$Electrode = as.numeric(colnames(or))
    values$Time = 0
    
    if(!is.null(values$Passes_Filters)) {
      values$Passes_Filters[values$Passes_Filters==1] = 'Pass'
      values$Passes_Filters %<>% factor(levels=c('Pass'))
    }
    if(!is.null(values$Selected_Electrodes)) {
      values$Selected_Electrodes[values$Selected_Electrodes==1] = 'Selected'
      values$Selected_Electrodes %<>% factor(levels='Selected')
    }
    
    # let's also set the electrode freesurfer label into the dset if we have it
    if('freesurferlabel' %in% names(electrodes_csv)) {
      # I'm randomizing the factor order here so the colors will not be nearby
      values$Anatomy = factor(electrodes_csv[['freesurferlabel']], levels=sample(unique(electrodes_csv[['freesurferlabel']])))
    }
    
    brain$set_electrode_values(values)
    # assign('omnibus_results', omnibus_results, globalenv())
    
    # check to see if we've udpated the dependent variable. We do this by comparing this list of Possible DVs with the 
    # actual current DV
    curr_dv =  controllers[['Display Data']]
    new_dv = rownames(or)[1]
    is_old_dv = curr_dv %in% str_subset(format_unit_of_analysis_name(get_unit_of_analysis(names=T)),
                                        new_dv, negate = TRUE)
    
    if(!length(controllers[['Display Data']]) || controllers[['Display Data']] == '[None]' || is_old_dv) {
      controllers[['Display Data']] = new_dv
      v = ceiling(max(abs(or[1,])) )
      controllers[['Display Range']] = sprintf('-%s,%s', v, v )
      # tr = controllers[['Threshold Range']]
    }
    
    ### maybe we don't always want to show legend...
    controllers[['Show Legend']] = TRUE
    
    ##FIXME -- change this to use the 3dViewer heatmap selector. not yet built
    cp = input$viewer_color_palette
    if(is.null(cp) || cp == 'Synch with heatmaps') {
      .colors = get_heatmap_palette(input$heatmap_color_palette)
    } else {
      .colors = get_heatmap_palette(cp)
    }
    pal = expand_heatmap(.colors, ncolors=128)
    pval_pal = expand_heatmap(
      rev(tail(.colors, ceiling(length(.colors)/2))),
      ncolors=128, bias=10)
    pals = list(pal)
    pals[2:nrow(omnibus_results )] = pals
    names(pals) = fix_name_for_js(rownames(omnibus_results))
    
    pals[str_detect(names(pals), 'p\\.')] = list(pval_pal)
    pals[names(pals) %in% c('p')] = list(pval_pal)
    
    pals[['[Subject]']] = 'black'
    pals[['Passes_Filters']] = 'black'
    
    re = brain$plot(symmetric = 0, palettes = pals,
                    side_width = side_width / 2, side_canvas = TRUE, 
                    side_display = display_side, start_zoom = zoom_level, controllers = controllers,
                    control_presets = 'syncviewer', timestamp = FALSE)
    
  }else{
    # optional, if you want to change the way 3D viewer looks in additional tab
    daemon_env$widget = brain$plot(side_width = side_width, side_canvas = FALSE, side_display = display_side)
    
    # Just initialization, no need to show sidebar
    re = brain$plot(side_width = side_width / 2, side_canvas = TRUE, side_display = display_side,
                    control_presets = 'syncviewer', timestamp = FALSE, control_display=FALSE)
  }
  
  
  
  shiny::removeNotification('power_3d_fun')
  
  re
}

## modified from downloadButton
fix_font_color_button <- function (outputId, label = "Download", class = NULL, ...)  {
  aTag <- tags$a(id = outputId,
                 class = paste("btn shiny-download-link", 
                                              class), href = "", target = "_blank", download = NA, 
                 icon("download"), label, ...)
}

download_all_graphs = function(){
  tagList(
    fix_font_color_button(ns('btn_graph_download'),
                          'Download graphs and their data', icon=shiny::icon('download'),
                          class = 'btn-primary text-white')
  )
}

### hi-res plot download
custom_plot_download <- custom_plot_download_impl(module_id = 'power_explorer',
  c('Activity over time by frequency',
    'Activity over time by trial',
    'Activty over time by condition',
    'Per trial, averaged across electrodes', 
    'Activity over time by electrode',
    'Per electrode statistical tests')
)

get_validated_plot_sizes <- function(pname, w, h) {
  valid_sizes <- list(
    'Per electrode statistical tests' = c(5, 2),
    'Activity over time by electrode' = c(5, 2),
    'Activity over time by frequency' = c(5,2),
    'Activity over time by trial' = c(5,2),
    'Activty over time by condition' = c(5,2),
    'Per trial, averaged across electrodes' = c(2,3)
  )
  w = max(w, valid_sizes[[pname]][1])
  h = max(h, valid_sizes[[pname]][2])
  
  return(c(w,h))
}  


output$btn_custom_plot_download <- downloadHandler(
  filename=function(...){
    paste0(stringr::str_replace_all(input$custom_plot_select,' ', '_'),
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.', input$custom_plot_file_type)
  },
  content = function(conn) {
    pt = input$custom_plot_file_type
    DEV = match.fun(pt)
    args = build_file_output_args(pt, input$custom_plot_width, input$custom_plot_height, conn)
    
    
    wh <- get_validated_plot_sizes(input$custom_plot_select,
                                   input$custom_plot_width, input$custom_plot_height)
    args[c('width', 'height')] = wh
    
    rgd <- dipsaus::dev_create('RAVE_PLOTTER' = do.call(DEV, args = args))
    on.exit({
      rgd$dev_off()
    }, add = TRUE)
    
    #### set the margins of the plot
    par(mar = c(3, 3.5, 2, 1))
    if(isTRUE(input$save_hires_plot_to_server)) {
      fname <- paste0(stringr::str_replace_all(input$custom_plot_select,' ', '_'),
                      format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.', pt)
      .dir <- paste0(subject$dirs$rave_dir, '/figures/')
      if(!dir.exists(.dir)) dir.create(.dir, showWarnings = TRUE, recursive = TRUE)
      on.exit({
        dipsaus::cat2('Trying to copy from: ', conn, ', to: ', paste0(.dir, fname))
        file.copy(conn, paste0(.dir, fname))
      }, add=TRUE, after = TRUE)
    }
    
    custom_plot_download_renderers(input$custom_plot_select)
  })


custom_plot_download_renderers <- function(nm) {
  plot_options = ravebuiltins_power_explorer_plot_options$as_list()
  plot_options %<>% set_font_scaling('Rutabaga')
  
  # dipsaus::cat2('setting rave context')
  
  pest  = function() {
    vars = c("has_data","which_result_to_show_on_electrodes","unit_of_analysis","omnibus_results","mean_filter",
              "mean_operator","mean_operand","p_filter","p_operator","p_operand","t_filter","t_operator","t_operand",
              "electrodes_csv","analysis_filter_variable","analysis_filter_variable_2", "analysis_filter_elec","analysis_filter_elec_2",
              "electrodes","show_result_densities","background_plot_color_hint","color_palette")
    
    ll = list()
    in_local_data = vars %in% names(local_data)
    ll[vars[in_local_data]] = lapply(vars[in_local_data], function(v) local_data[[v]])
    
    .input <- isolate(shiny::reactiveValuesToList(input))
    for(v in vars[!in_local_data]) {
        ll[[v]] = .input[[v]]
    }
    
    ll$has_data = length(ll$omnibus_results) > 1
    
    assign('ll', ll, envir = globalenv())
    across_electrode_statistics_plot(build_results_object(ll))
  }
  aotbe = function() {
    set_heatmap_palette_helper(plot_options=plot_options)
    draw_many_heat_maps(local_data$by_electrode_heat_map_data,
                        log_scale = plot_options$log_scale,
                        max_zlim = plot_options$max_zlim,
                        percentile_range=plot_options$percentile_range,
                        plot_time_range = plot_options$plot_time_range,
                        PANEL.LAST = by_electrode_heat_map_decorator(plot_options=plot_options),
                        PANEL.COLOR_BAR = ifelse(plot_options$show_heatmap_range, color_bar_title_decorator, 0)
    )
  }
  aotbf = function() {
    set_heatmap_palette_helper(plot_options=plot_options)
    draw_many_heat_maps(hmaps = local_data$heat_map_data,
                        log_scale = plot_options$log_scale,
                        max_zlim = plot_options$max_zlim,
                        percentile_range=plot_options$percentile_range,
                        plot_time_range = plot_options$plot_time_range,
                        PANEL.LAST = spectrogram_heatmap_decorator(plot_options=plot_options),
                        PANEL.COLOR_BAR = ifelse(plot_options$show_heatmap_range, color_bar_title_decorator, 0)
    )
  }
  aotbt = function() {
    set_heatmap_palette_helper(plot_options=plot_options)
    by_trial_heat_map_data = local_data$by_trial_heat_map_data
    
    decorator <- by_trial_heat_map_decorator(plot_options = plot_options)
    
    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # need to sort the data
    sort_trials_by_type <- plot_options$sort_trials_by_type
    if(sort_trials_by_type != 'Trial Number') {
      for(ii in which(sapply(by_trial_heat_map_data, '[[', 'has_trials'))) {
        by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_event(event_name = sort_trials_by_type)
      }
      
      # add a decorator that can draw the trial labels
      if(sort_trials_by_type == 'Condition') {
        decorator %<>% add_decorator(trial_type_boundaries_hm_decorator)
      } else  {
        decorator %<>% add_decorator(by_trial_analysis_window_decorator(event_name= sort_trials_by_type,
                                                                        show_label = plot_options$draw_decorator_labels))
      }
    }
    
    if(plot_options$show_outliers_on_plots) {
      # print('showing outliers')
      decorator %<>% add_decorator(heatmap_outlier_highlighter_decorator)
    } else {
      # print('not showing outliers, removing them, start with: ' %&% nrow(by_trial_heat_map_data[[1]]$data))
      by_trial_heat_map_data %<>% remove_outliers_from_by_trial_data
    }
    
    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    need_wide = ('Condition' == sort_trials_by_type)
    
    draw_many_heat_maps(hmaps = by_trial_heat_map_data,
                        log_scale = plot_options$log_scale,
                        max_zlim = plot_options$max_zlim,
                        percentile_range=plot_options$percentile_range,
                        plot_time_range = plot_options$plot_time_range,
                        PANEL.LAST=decorator,
                        PANEL.COLOR_BAR = ifelse(plot_options$show_heatmap_range, color_bar_title_decorator,0),
                        wide = need_wide,
                        # we always want the x axis, but we only want the y axis if we are NOT sorting by type
                        axes=c(TRUE, !need_wide))
  }
  aotbc = function() {
    time_series_plot(plot_data = local_data$over_time_data,
                     plot_time_range = plot_options$plot_time_range,
                     PANEL.FIRST = time_series_decorator(plot_options=ravebuiltins_power_explorer_plot_options$as_list())
    )
    
  }
  ptaac = function() {
    trial_scatter_plot(
      group_data = local_data$scatter_bar_data,
      show_outliers = plot_options$show_outliers_on_plots,
      PANEL.LAST = trial_scatter_plot_decortator(plot_title_options = plot_options$plot_title_options)
    )
  }
  
  FUNS = list(
    'Per electrode statistical tests' = pest,
    'Activity over time by electrode' = aotbe,
    'Activity over time by frequency' = aotbf,
    'Activity over time by trial' = aotbt,
    'Activty over time by condition' = aotbc,
    'Per trial, averaged across electrodes' = ptaac
  )
  nm = match.arg(nm, names(FUNS))
  set_palette_helper(plot_options=ravebuiltins_power_explorer_plot_options$as_list())
  FUNS[[nm]]()
  
}

#### Sheth special
sheth_special <- function() {
  tagList(div(class='rave-grid-inputs',
              div(style='flex-basis: 100%', customDownloadButton(ns('btn_sheth_special'),
                                                                 label = "Sheth's special stat heatmap", icon_lbl = 'user-md')),
              div(style='flex-basis: 25%', numericInput(ns('sheth_special_width'), label='width (inches)', value=15, min=5, step = 1)),
              div(style='flex-basis: 25%', numericInput(ns('sheth_special_height'), label='height (inches)', value=10, min=3, step = 1))
  ))
}

observeEvent(input$sheth_special_height, {
  local_data$sheth_special_height = input$sheth_special_height
  
  if(local_data$sheth_special_height < 3) {
    local_data$sheth_special_height = 3
    showNotification('Height cannot be less than 3')
    updateNumericInput(session, 'sheth_special_height', value = 3)
  }
  
})
observeEvent(input$sheth_special_width, {
  local_data$sheth_special_width = input$sheth_special_width
  
  if(local_data$sheth_special_width < 5) {
    local_data$sheth_special_width = 5
    showNotification('Width cannot be less than 5')
    updateNumericInput(session, 'sheth_special_width', value=5)
  }
})


build_sheth_matrices <- function() {
  unit_dims = c(1,2,4)
  if(isTRUE(global_baseline)) {
    unit_dims = c(2,4)
  }
  baseline_method = get_unit_of_analysis(unit_of_analysis)
  
  # see if there are contrasts to run
  gnames = sort(unique(c(unlist(sapply(GROUPS, '[[', 'group_conditions')))))
  
  # we are doing this here instead of when lmmeans is called because that is called inside the lapply_async
  # the critical thing is the call to factory above that ensure the levels are ordered based on how they are entered
  # into the condtion groups.
  lbls = build_group_contrast_labels(gnames)
  
  berger_bands <- list(
    delta = 0:3,
    theta = 4:7,
    alpha = 8:12,
    beta = 13:31,
    'low gamma' = 32:70,
    'high gamma' = 75:150
  )
  
  show_contrasts = local_data$sheth_special_include_contrasts
  epoch_data_Condition = epoch_data$Condition
  
  shift_amount = new_range = NULL
  if(event_of_interest != epoch_event_types[1]) {
    ev = get_events_data(epoch_event_types = epoch_event_types)
    new_range = determine_available_shift(event_of_interest,
                                          available_time = range(power$dimnames$Time),
                                          epoch_information = ev)
    
    shift_amount = determine_shift_amount(event_time = ev[[event_of_interest]], available_shift=new_range)
  }
  
  res = #lapply(electrodes, function(e, ...){
    rave::lapply_async(electrodes, function(e){
      # e = electrodes[1]
      
      if(is.null(shift_amount)) {
        el = power$subset(Electrode = Electrode == e,
                          Frequency = Frequency %within% 0:151,
                          Time = (Time %within% ANALYSIS_WINDOW) | (Time %within% BASELINE_WINDOW)
        )
        if(prod(dim(el)) < 1) return (NULL)
        
        bl = dipsaus::baseline_array(
          x = el$get_data(),
          baseline_indexpoints = which(el$dimnames$Time %within% BASELINE_WINDOW),
          along_dim = 3L, method = baseline_method, unit_dims = unit_dims)
        bl = ECoGTensor$new(bl, dim = dim(el), dimnames = dimnames(el),
                            varnames = el$varnames, hybrid = FALSE)
      } else {
        el = power$subset(Electrode = Electrode == e,
                          Frequency = Frequency %within% 0:151)
        # can't do this because of the shift
        # ,Time = (Time %within% ANALYSIS_WINDOW) | (Time %within% BASELINE_WINDOW))
        if(prod(dim(el)) < 1) return (NULL)
        
        bl = dipsaus::baseline_array(
          x = el$get_data(),
          baseline_indexpoints = which(el$dimnames$Time %within% BASELINE_WINDOW),
          along_dim = 3L, method = baseline_method, unit_dims = unit_dims)
        bl = get_shifted_tensor(bl, shift_amount, new_range = new_range,
                                dimnames = dimnames(el), varnames = el$varnames)
      }
      
      bl.analysis <- bl$subset(Time=Time %within% ANALYSIS_WINDOW)$get_data()
      
      t2p <- function(t,df) {
        2*pt(abs(t), df=df, lower.tail = F)
      }
      
      result_by_band = lapply(berger_bands, function(band) {
        # band = berger_bands[[1]]
        trial_means = rowMeans(bl.analysis[,el$dimnames$Frequency %within% band,,1,drop=FALSE])
        # trial_means = rowMeans(bl.analysis$subset(get_data())
        
        mse = .fast_mse(trial_means)
        t = mse[1]/mse[2]
        mean_res = matrix(c(mse[1], t, t2p(t, length(trial_means)-1)), nrow=1,
                          dimnames = list(
                            'Omnibus', c('m', 't', 'p'))
        )
        median_res = matrix(
          c(median(trial_means), wilcox.test(trial_means)$p.value),
          nrow=1, dimnames = list('Omnibus', c('med', 'p'))
        )
        
        # now we also need to run the contrasts
        mtp = tapply(trial_means, epoch_data_Condition, function(x) {
          m = .fast_mse(x)
          .t <- m[1]/m[2]
          c(m[1], .t, t2p(.t, length(x)-1))
        }) %>% rbind_list
        mean_res %<>% rbind(mtp)
        
        medp = tapply(trial_means, epoch_data_Condition, function(x) {
          m = median(x)
          c(m,(wilcox.test(x)$p.value))
        }) %>% rbind_list
        median_res %<>% rbind(medp)
        
        if(show_contrasts) {
          showNotification(p('Ignoring request to generate contrasts... not yet implemented'),
                           type='warning', duration = 4)
        }
        
        return(list(
          'median' = median_res,
          'mean' = mean_res
        ))
      })
      
      return(result_by_band)
    } ,
    .globals = c('baseline_array', 'baseline_method', 'unit_dims', 'electrodes', 'e', 'gnames', 'berger_bands', 'epoch_data_Condition',
                 'FREQUENCY', 'ANALYSIS_WINDOW', 'BASELINE_WINDOW', 'shift_amount',
                 '.fast_mse', 'df_shell', 'show_contrasts'),
    .gc = FALSE)
  
  by_band <- sapply(names(berger_bands), function(band) {
    .band = lapply(res, '[[', band)
    
    if(is.null(.band)) {
      return (NULL)
    }
    mean = list(
      m = sapply(.band, function(x) x$mean[,1]),
      t = sapply(.band, function(x) x$mean[,2]),
      p = sapply(.band, function(x) x$mean[,3])
    )
    mean %<>% lapply(function(x) {
      colnames(x) = electrodes
      x
    })
    
    median = list(
      m = sapply(.band, function(x) x$median[,1]),
      p = sapply(.band, function(x) x$median[,2])
    )
    median %<>% lapply(function(x) {
      colnames(x) = electrodes
      x
    })
    
    list('mean'=mean, 'median'=median)    
  }, simplify = FALSE)
  
  attr(by_band, 'band_structure') = sapply(berger_bands, dipsaus::deparse_svec)
  
  return(by_band)
}

output$btn_sheth_special <- downloadHandler(
  filename=function(...) {
    paste0('sheth_special_',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.pdf')
  }, 
  content = function(conn) {
    
    progress = rave::progress("Making Sheth's special", max = 4)
    
    on.exit({progress$close()}, add=TRUE)
    
    progress$inc('Building matrices')
    mats = build_sheth_matrices()
    # assign('.mats', mats, envir=globalenv())
    # mats = .mats
    
    progress$inc('Building PDF')
    
    bpch = input$background_plot_color_hint
    if(bpch == 'Gray') {
      bpch = rave_colors$DARK_GRAY
    }
    
    pdf(file = conn, width = local_data$sheth_special_width, bg = bpch,
        height = local_data$sheth_special_height, useDingbats = FALSE)
    
    on.exit(dev.off(), add = TRUE)
    
    max_char_count = max(unlist(lapply(mats, function(x) {
      nchar(rownames(x$mean$m))
    })))
    mar = c(1, 4 + max(0, (max_char_count - 5) * 0.95), 4, 1)
    
    layout(matrix(1:12, ncol=4, byrow=TRUE),
           widths = rep(c(1,lcm(3)), 6))
    
    # mean and t-score maps
    progress$inc('writing mean and t(mean)')
    for(current_stat in c('m', 't')) {
      mapply(make_stat_heatmap, mats, names(mats),
             attr(mats, 'band_structure'), MoreArgs = list(type='mean', 'stat'=current_stat, 'mar'=mar)
      )
    }
    #median maps
    progress$inc('writing median results')
    mapply(make_stat_heatmap, mats, names(mats), attr(mats, 'band_structure'),
           MoreArgs = list(type='median', stat='m', 'mar'=mar))
    
  }
)

make_stat_heatmap <- function(mat, nm, band, stat, type='mean', mar) {
  # print(nm)
  .m = t(mat[[type]][[stat]])
  
  x = 1:nrow(.m)
  y = 1:ncol(.m)
  zlim = max(pretty(abs(quantile(.m, c(0.01, 0.99)))))
  
  if(!missing(mar)) {
    par(mar=mar)
  }
  # 
  rutabaga::plot_clean(range(x)+c(-0.5,0.5),
                       range(y)+c(-1, 1))
  make_image(.m[,ncol(.m):1,drop=FALSE], y=y, x=x, add=TRUE, useRaster = FALSE, zlim = c(-zlim, zlim))
  rave_axis(3, 1:nrow(.m), labels=row.names(.m), tcl=0, lwd=0, cex.axis=0.75, mgpx = c(0, -1, 0))
  rave_axis(2, 1:ncol(.m), labels=rev(colnames(.m)), tcl=0, lwd=0, cex.axis=0.75)
  rave_title(sprintf("%s (%s)", nm, band), cex=1)
  
  if(local_data$highlight_significant_results) {
    pvals = t(mat[[type]][['p']])
    for(ii in 1:nrow(pvals)) {
      for(jj in 1:ncol(pvals)) {
        if(pvals[ii,jj] < 0.05) {
          # which(pvals < 0.05, arr.ind = TRUE)
          draw.box(x0 = ii-0.539, y0=jj-0.5,
                   x1 = ii+0.539, y1=jj+0.5,
                   lty = if(pvals[ii,jj] < 0.01) {1} else {3}, lwd=0.5)
        }
      }
    }
  }
  
  .ylab = paste(type, unit_of_analysis)
  if(stat == 't') {
    .ylab = paste0('t(mean ', unit_of_analysis, ')')
  }
  
  rave_color_bar(zlim, ylab=.ylab, mar = c(2,4.1,4,2),
                 cex.lab = 0.75, cex.axis = 0.75)
  color_bar_title_decorator(list(list(range=range(.m))), cex=0.75)
}

download_electrodes_csv <- function() {
  tagList(tags$p('   ',style='margin-top:20px; margin-bottom:5px'),
          downloadLink(ns('btn_electrodes_meta_download'), 'Download copy of meta data for all electrodes'))
}

# do_calculate_btn_float = function() {
#   dipsaus::actionButtonStyled(
#     ns('do_calculate_btn_float_button'), label = 'RAVE!',
#     width = '200px', type = 'info', 
#     icon = icon('magic'), style = 'z-index: 999; position: fixed; left: 50px; top:10px; display: block !important')
# }


# observeEvent(input$do_calculate_btn_float_button, {
#   if(shiny_is_running() & !auto_recalculate()) {
#     trigger_recalculate()
#   }
# })

output$btn_electrodes_meta_download <- downloadHandler(
  filename=function(...) {
    paste0('electrodes_meta_data_copy_',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.csv')
  },
  content = function(conn) {
    write.csv(module_tools$get_meta('electrodes'), file=conn)
  }
)

output$btn_graph_download <- downloadHandler(
  filename = function(...) {
    paste0('power_explorer_export',
           format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.zip')
  },
  content = function(conn) {
    tmp_dir = tempdir()
    
    # map the human names to the function names
    function_map <- list('Spectrogram' = 'heat_map_plot',
                         'By Trial Power' = 'by_trial_heat_map_plot',
                         'Over Time Plot' = 'over_time_plot', 
                         'Windowed Average' = 'windowed_comparison_plot',
                         'Over Time by Electrode' = 'by_electrode_heat_map_plot',
                         'Results by Electrode' = 'across_electrode_statistics_plot')
    
    to_export <- function_map[plots_to_export]
    prefix <- sprintf('%s_%s_%s_', subject$subject_code, subject$project_name, format(Sys.time(), "%b_%d_%Y_%H_%M_%S"))
    
    fnames <- function_map[plots_to_export]
    
    tmp_files <- prefix %&% str_replace_all(names(fnames), ' ', '_') %&% '.pdf'
    
    print('writing out graphs')
    write_out_graphs(conns=file.path(tmp_dir, tmp_files), plot_functions=fnames,
                     dir=tmp_dir, prefix=prefix)    
    wd = getwd()
    on.exit({setwd(wd)}, add = TRUE)
    
    setwd(tmp_dir)
    print('making zip file')
    zip(conn, files = list.files(pattern = prefix %&% '+.+(pdf|json)'), flags='-r2X')
  }
)

write_out_graphs <- function(conns=NA, plot_functions, dir, prefix, ...) {
  args = isolate(reactiveValuesToList(input))
  
  assign('..args', args, envir = globalenv())
  # args = ..args
  # attachDefaultDataRepository()
  electrodes_loaded = preload_info$electrodes
  # check to see if we should loop over all electrodes or just the currently selected electrode(s)
  if(export_what == 'Current Selection') {
    electrodes_loaded <- requested_electrodes
  }
  
  progress = rave::progress('Rendering graphs...',
                            max = length(electrodes_loaded) + 2)
  
  on.exit({progress$close()}, add=TRUE)
  progress$inc('Initializing')
  
  rave::rave_context('rave_running_local')
  module = rave::get_module('ravebuiltins', 'power_explorer', local = TRUE)
  formal_names = names(formals(module))
  if(F) {
    args = sapply(formal_names, function(fn) {
      results$get_value(fn)
    })
  }
  args = args[formal_names]
  names(args) = formal_names
  
  # so we want to open all the PDFs initially
  # based on the number of groups we should scale the plots
  ngroups = 0
  for(ii in seq_along(args$GROUPS)) {
    if(length(args$GROUPS[[ii]]$group_conditions)>1) {
      ngroups = ngroups+1
    }
  }
  # having issues here with the size of the plots being too large for the font sizes
  # we can't (easily) change the cex being used by the plots. So maybe we can 
  # just change the size of the output PDF. people can then resize but keep the relative sizes correct
  # FIXME the sizes of these outputs aren't very pretty. we should probably just hard code a list for common sizes, say
  # ngroup <= 4... then apply a scale factor after that? We also need to take care of the font cex... could maybe set some hint
  # such that the rave_cex is set to pdf mode... We should be able to query the current graphics device to see if it is a window or 
  # a pdf?
  
  # yes!
  # > dev.cur()
  # pdf 
  # 4 
  fin = mapply(function(conn, pf) {
    w_scale = h_scale = 1
    if(pf == 'windowed_comparison_plot') {
      w_scale = ngroups / 2.25
    }
    
    if(pf %in% c('by_trial_heat_map_plot', 'heat_map_plot')) {
      w_scale = ngroups*1.25
      h_scale = ngroups*1.05
    }
    
    .w <- round(9.75*w_scale,1)
    .h <- round(6.03*h_scale,1)
    
    pdf(conn, width = .w, height = .h, useDingbats = FALSE)
  }, conns, plot_functions)
  
  #there are more graphics devices existing than those that we just created, so
  #we need to be a little more careful about how we cycle through them, see 
  # names(dev.list()) == 'pdf' below
  
  find_open_pdfs <- function() {
    dev.list()[names(dev.list()) == 'pdf']
  }
  
  on.exit({
    sapply(find_open_pdfs(), dev.off)
  }, add = TRUE)
  
  plot_for_el <- function(etext, write_out_data=FALSE) {
    if(length(etext) > 1) {
      etext %<>% deparse_svec
    }
    # if(shiny_is_running()) {
    progress$inc(sprintf('Rendering graphs for %s', etext))
    # }
    
    args[['ELECTRODE_TEXT']] = etext
    result = do.call(module, args)
    .results = result$results
    
    fff = function(x) {
      paste(names(x), x, sep=':')
    }
    
    mapply(function(graf, dev_num) {
      cat2(paste('plotting:', graf), level = 'INFO')
      cat2(paste('plotting:', fff(dev.set(dev_num))), level = 'INFO')
      #get the function named by graf
      get(graf, envir = asNamespace('ravebuiltins'))(.results)
      
      # match.fun(graf)(.results)
      if(write_out_data) {
        fname <- file.path(dir, paste0(prefix, graf, '.json'))
        data_var = stringr::str_replace(graf, '_plot', '_data')
        cat(jsonlite::serializeJSON(result$results$get_value(data_var, 'NODATA')),
            file=fname)
      }
    }, plot_functions, find_open_pdfs())
  }
  
  # first write into the graphs the aggregate functions
  plot_for_el(electrodes_loaded, write_out_data = TRUE)
  
  # now for the individual electrodes
  lapply(electrodes_loaded, plot_for_el)
  
  if(shiny_is_running()) {
    showNotification(p('Exports finished!'))
  }
}

# Export data options
write_out_data_ui <- function(){
  download = isTRUE(input$export_also_download)
  if( download ){
    tags$a(id = ns('export_data_and_download'), class = 'btn btn-primary shiny-download-link',
           href = '', target='_blank', download='',
           shiny::icon('download'), 'Export data for group analysis')
  }else{
    actionButtonStyled(ns('export_data_only'), 
                       label = 'Export data for group analysis', 
                       icon=shiny::icon('save'),
                       type = 'primary')
  }
}

output$export_data_and_download <- downloadHandler(
  filename = function(){
    analysis_prefix = stringr::str_replace_all(analysis_prefix, '[^\\w]+', '_')
    paste0(analysis_prefix, '.fst')
  },
  content = function(con){
    res_path = write_out_data_function()
    file.copy(res_path, to=con)
  }
)

observeEvent(input$export_data_only, {
  write_out_data_function()
  showNotification(p('Done saving'), duration = 3, type = 'message')
})


save_inputs <- function(yaml_path, variables_to_export){
  if( !shiny_is_running() || !exists('getDefaultReactiveInput') ){ return(FALSE) }
  
  input <- getDefaultReactiveInput()
  cache_list = shiny::isolate(shiny::reactiveValuesToList(input))
  if(!missing(variables_to_export)) {
    cache_list =cache_list[variables_to_export]
  }
  # if( exists('local_data') && shiny::is.reactivevalues(local_data) ){
  #   local_dat = shiny::isolate(shiny::reactiveValuesToList(local_data))
  #   cl = names(cache_list); cl = cl[cl %in% names(local_dat)]
  #   cache_list[cl] = local_dat[cl]
  # }
  yaml::write_yaml(x = cache_list, fileEncoding = 'utf-8', file = yaml_path)
  return(TRUE)
}

# export data for group analysis
write_out_data_function <- function(write_out_movie_csv=TRUE){
  dipsaus::cat2('start WOD', level='INFO')
  project_name = subject$project_name
  subject_code = subject$subject_code
  
  # get electrodes to be exported
  electrodes = parse_svec(input$current_active_set)
  electrodes = electrodes[electrodes %in% preload_info$electrodes]
  
  progress = progress('Exporting baselined data...', max = 3 + length(electrodes))
  on.exit({ progress$close() }, add = TRUE)
  progress$inc('Collecting data')
  
  # Get trial conditions
  conditions = input$trial_type_filter
  conditions = conditions[conditions %in% preload_info$condition]
  trials = module_tools$get_meta('trials')
  trial_number = trials$Trial[trials$Condition %in% conditions]
  
  dipsaus::cat2('Line 856', level='INFO')
  
  # check if they want to include outliers
  .trial_outlier_list = input$trial_outliers_list
  if(length(.trial_outlier_list) > 0 && (!input$include_outliers_in_export)) {
    trial_number <- trial_number[!(trial_number %in% .trial_outlier_list)]
  }
  
  # Get timepoints,frequency range
  time_points = preload_info$time_points
  # time_points = time_points[time_points %within% input$export_time_window]
  freq_range = preload_info$frequencies
  freq_range = freq_range[freq_range %within% input$FREQUENCY]
  
  # get baseline
  baseline_range = input$BASELINE_WINDOW
  dipsaus::cat2('Line 872', level='INFO')
  # Do some checks
  
  # Check 1: if no electrode is chosen
  # Check 2: if no condition is chosen
  # Check 3: if no time is chosen
  # Check 4: if no frequency is found
  check_fails = !c(length(electrodes), length(trial_number), length(time_points), length(time_points))
  err_msg = c('None of the electrodes to be exported is loaded', 'No trial found matching selected condition', 
              'Time range is too narrow for any data points to be found', 'Frequency range is too narrow for any data points to be found')
  if(any(check_fails)){
    err_msg = err_msg[check_fails]
    showNotification(p('The following error(s) found:',br(),tags$ul(tagList(
      lapply(err_msg, tags$li)
    ))), type = 'error', id = ns('export_csv'))
    return()
  }
  dipsaus::cat2('Line 889', level='INFO')
  # Baseline
  progress$inc('Generating results... (might take a few minutes)')
  
  # Memory-friendly baseline but might be more time consuming
  power = module_tools$get_power(referenced = TRUE)
  
  # condition list
  cond_list = list(); cond_list[trials$Trial] = trials$Condition
  
  unit_dims = c(1,2,4)
  if(isTRUE(input$global_baseline)) {
    unit_dims = c(2,4)
  }
  unit_of_analysis <- input$unit_of_analysis
  baseline_method = get_unit_of_analysis(unit_of_analysis)
  unit_name = format_unit_of_analysis_name(unit_of_analysis)
  dipsaus::cat2('Line 906', level='INFO')
  # here we want to take into the event of interest as well I think we just shift the entire data set here
  res = rave::lapply_async(electrodes, function(e){
    # e = electrodes[1]
    progress$inc(sprintf('Electrode %d', e))
    # Important p_sub is assigned, otherwise, it will get gc before baselined
    p_sub = power$subset(Trial = Trial %in% trial_number,
                         Frequency = Frequency %within% freq_range,
                         Electrode = Electrode %in% e)
    
    # bl = baseline(p_sub, from = baseline_range[1], to = baseline_range[2], hybrid = FALSE, mem_optimize = FALSE)
    bl = dipsaus::baseline_array(x = p_sub$get_data(), baseline_indexpoints = which(p_sub$dimnames$Time %within% baseline_range),
      along_dim = 3L, unit_dims = unit_dims, method = baseline_method)
    
    ### here I think we just want to export all the events...
    by_event_type <- sapply(epoch_event_types, function(eot) {
      # eot = epoch_event_types[2]
      
      if(eot != epoch_event_types[1]) {
        epoch_info = get_events_data(epoch_event_types)
        
        # BUGFIX April 08, 2021
        # we need to also edit the epoch information to handle the removal outliers
        # the trial number variable was used to subset the tensor, so we'll use it here
        # other approach would be to use the Trial dimname in the tensor?
        epoch_info = subset(epoch_info, Trial %in% trial_number)
        
        new_range = determine_available_shift(eot, range(power$dimnames$Time), epoch_information = epoch_info)
        
        shift_amount = determine_shift_amount(event_time = epoch_info[[eot]],
                                              available_shift=new_range)
        
        analysis = get_shifted_tensor(bl, shift_amount, new_range = new_range,
                                dimnames = dimnames(p_sub), varnames = p_sub$varnames)
      } else {
        analysis = ECoGTensor$new(bl, dim = dim(p_sub), dimnames = dimnames(p_sub),
                            varnames = p_sub$varnames, hybrid = FALSE)
      }
      # analysis = analysis$subset(Time = Time %within% time_points)
      flat = analysis$collapse(keep = c(1,3))
      
      dimnames(flat) = dimnames(analysis)[c(1,3)]
      flat = reshape2::melt(flat, value.name = sprintf('%s_%s', unit_name, 
                                                       str_replace_all(eot, ' ', '_'))
                            ) # trial time, value
      flat$Condition = unlist(cond_list[flat$Trial])
      flat$Electrode = e
      
      # flat$uuid = paste0(flat$Trial, '_', formatC(flat$Time, width = 12, digits=10, flag='0'))
      
      return(flat)
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    # if there is only one event, return it, else we need to merge the results
    if(length(by_event_type) == 1) return (by_event_type[[1]])
    
    full_matrix = by_event_type[[1]]
    
    # sapply(by_event_type, function(bet) bet$Time %>% range)
    # sapply(by_event_type, names)
    
    for(ii in 2:length(by_event_type)) {
      full_matrix = merge(full_matrix, by_event_type[[ii]], all=TRUE)
    }
    
    return (full_matrix)
  }, 
  .globals = c('unit_name', 'unit_dims', 'baseline_method', 'power', 'event_of_interest',
                  'trial_number', 'time_points', 'freq_range', 'e', 'baseline_range', 'cond_list'),
  .gc = FALSE)
  
  dipsaus::cat2('Line 977', level='INFO')
  res = do.call('rbind', res)
  
  res$Project = project_name
  res$Subject = subject_code
  dipsaus::cat2('Line 981', level='INFO')
  # flag outliers as needed
  res$TrialIsOutlier = FALSE
  if(!is.null(.trial_outlier_list) && length(.trial_outlier_list) > 0) {
    res$TrialIsOutlier[res$Trial %in% .trial_outlier_list] = TRUE
  }
  
  # tack on the electrode info, checking for ROI candidates along the way
  cn <- tolower(colnames(subject$meta$electrode))
  extra_rois <- cn[startsWith(cn, 'group')]
  roi_vars <- c('hemisphere', 'freesurferlabel', extra_rois)
  vars_to_add = c('electrode', 'label', roi_vars)
  v_index <- sapply(vars_to_add, function(vn) {
    which(vn == cn)
  }) %>% unlist
  from_el = subject$meta$electrode[,v_index, drop=FALSE]
  
  # label the ROI vars
  for(vi in which(names(v_index) %in% roi_vars)) {
      names(from_el)[vi] = paste0(RAVE_ROI_KEY, names(from_el)[vi])
  }
  
  dipsaus::cat2('Line 996', level='INFO')
  res = merge(res, from_el)
  
  dipsaus::cat2('Line 998', level='INFO')
  # also add in the block numbers per DABI request
  if(exists('epoch_data')) {
    res <- merge(res, epoch_data[,c('Block', 'Trial')], by='Trial')
    
    # let's tag Block as an ROI variable too. Don't mess up DABI folks, so keep Block as a separate var,
    # but this expands the size of the data set, right. 1 million rows = 8MB, but should compress well
    res[[paste0(RAVE_ROI_KEY, 'Block')]] = res$Block
  }
  dipsaus::cat2('Line 1003', level='INFO')
  
  # Write out results
  progress$inc('Writing out on server, preparing...')
  # write to server _project_data/power_explorer/file
  analysis_prefix = stringr::str_replace_all(analysis_prefix, '[^\\w]+', '_')
  now = strftime(Sys.time(), '-%Y%m%d-%H%M%S')
  
  fname = paste0(analysis_prefix, now, '.fst')
  dirname = file.path(subject$dirs$subject_dir, '..', '_project_data', 'power_explorer', 'exports')
  dir.create(dirname, showWarnings = FALSE, recursive = TRUE)
  
  fst::write_fst(
    res, file.path(dirname, fname), compress = 99
  )
  save_inputs(file.path(dirname, paste0(fname, '.yaml')))
  
  # Collapse Trial and save to 3D viewer
  if(write_out_movie_csv) {
    collapsed_trial = reshape2::dcast(res, Project+Subject+Electrode+Time~Condition, mean, value.var = paste0(unit_name, '_', 'Trial_Onset'))
    dirname_viewer = file.path(subject$dirs$subject_dir, '..', '_project_data', '3dviewer')
    dir.create(dirname_viewer, showWarnings = FALSE, recursive = TRUE)
    
    # create average columns based on the condition groups
    for(ii in seq_along(input$GROUPS)) {
      g <- input$GROUPS[[ii]]
      collapsed_trial[[g$group_name]] <- rowMeans(collapsed_trial[,colnames(collapsed_trial) %in% g$group_conditions, drop=FALSE])
    }
    
    fst::write_fst(
      collapsed_trial, 
      path = file.path(dirname_viewer, paste0(analysis_prefix, '-collapsed_trial_epoch_trial_onset_', now, '.fst')),
      compress = 99
    )
    
    # data.table::fwrite(collapsed_trial,
    #                    file.path(dirname_viewer, paste0(analysis_prefix, '-collapsed_trial-_epoch_trial_onset_', now, '.csv')),
    #                    append = FALSE)
  }
  
  return(normalizePath(file.path(dirname, fname)))
}
