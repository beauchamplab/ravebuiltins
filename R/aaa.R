# Imports from other packages

#' @import dipsaus
#' @import rutabaga
#' @import rave
#' @import shiny
#' @import stringr
#' @import lme4
#' @import lmerTest
#' @import emmeans
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom magrittr extract2
#' @importFrom magrittr extract
#' @importFrom magrittr set_rownames
#' @importFrom magrittr set_colnames
#' @importFrom magrittr equals
#' @import rlang
#'
#' @importFrom methods is
#' @importFrom methods getMethod
#' @import circular
#' 
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom grDevices palette
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices rgb
#' @importFrom grDevices adjustcolor
#' 
#' @import graphics
#' 
#' 
#' @importFrom stats median
#' @importFrom stats median.default
#' @importFrom stats pt
#' @importFrom stats quantile
#' @importFrom stats p.adjust
#' @importFrom stats symnum
#' @importFrom stats density
#' 
#' @importFrom fst read_fst
#' @importFrom fst write_fst
#' 
NULL
 
# Add global variables to pass check
..async_quo = NULL
..async_var = NULL
.palettes = NULL
BASELINE = NULL
FREQUENCY = NULL
Frequency = NULL
TIME_RANGE = NULL
Time = NULL
draw_time_baseline = NULL
is_clean = NULL
label.col = NULL
x = NULL
xax = NULL
xlab = NULL
y = NULL
yax = NULL
ylab = NULL
data = NULL






#' Function to load all dev funtions and wrap them within an environment
#' @param expose_functions logical indicating whether to expose all dev functions to the global environment
#' @param reload logical, do you want to fast-reload the package before load the functions?
#' @export
dev_ravebuiltins <- function(expose_functions = FALSE, reload = TRUE){
  .__rave_context__. = 'rave_module_debug'
  .__rave_package__. = 'ravebuiltins'
  if(reload){
    env <- rave::reload_module_package(expose_functions)
  }else{
    if(expose_functions){
      env = globalenv()
    }else{
      env = new.env(parent = globalenv())
    }
    rave::load_rave_module_package(env, 'rave_module_debug')
  }
  rave::rave_context(spos = 1L, tenv = globalenv())
  env
}

rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4

rave_axis_tcl = -0.3

RAVE_ROI_KEY = 'VAR_IS_ROI_'

rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')
rave_color_ramp_dark_palette <- colorRampPalette(c('#13547a', 'black', '#ff758c'), interpolate='linear', space='Lab')

..dark_blue_to_red <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff", 
                        "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))

..light_blue_to_light_red <- c(..dark_blue_to_red[5:1], 'black', ..dark_blue_to_red[11:7])
..light_blue_to_gray_to_light_red <- c(..dark_blue_to_red[5:1], '#1E1E1E', ..dark_blue_to_red[11:7])

rave_color_ramp_palette <- colorRampPalette(..dark_blue_to_red, interpolate='linear', space='Lab')
rave_heat_map_colors <- rave_color_ramp_palette(1001)

rave_color_ramp_dark_palette <- colorRampPalette(..light_blue_to_light_red, interpolate='linear', space='Lab')
rave_color_ramp_gray_palette <- colorRampPalette(..light_blue_to_gray_to_light_red, interpolate='linear', space='Lab')

rave_heat_map_dark_colors <- rave_color_ramp_dark_palette(1001)
rave_heat_map_gray_colors <- rave_color_ramp_gray_palette(1001)

# put this here for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('orange', 'dodgerblue3', 'darkgreen', 'orangered', 'brown', 'purple3')

# thos function is aware of shiny status and always returns 1.0 if shiny is not running
get_cex_for_multifigure <- function() {
  cex_multiplier <- 1
  if(shiny::isRunning()) {
    if(any(par('mfrow') > 2)) {
      cex_multiplier = 1/0.66
    } else if (all(par('mfrow') == 2)) {
     cex_multiplier <- 1/0.88
    }
  }
  return (cex_multiplier)
}


# Internal use, not exported
.rave_axis <- function(side, at, tcl=rave_axis_tcl, labels=at, las=1, cex.axis=rave_cex.axis,
                      cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), col, col.axis, ...) {
  
  # if the color isn't specified, then we are free to set the color to what we want.
  # let's set it to be black, unless the background color is black, then we'll do white
  col %?<-% get_foreground_color()
  col.axis %?<-% col
  
  ## overrule the font scaling if we're writing to a PDF... eventually we'll make this better
  if(shiny_is_running()) {
    if('pdf' == names(dev.cur())) {
      cex.axis = 1
      cex.lab = 1
      mgpy = c(3,.5,0)
      mgpx = c(3,.4,0)
    } 
  }
  
  rutabaga::ruta_axis(
    side = side,
    at = at,
    tcl = tcl,
    labels = labels,
    las = las,
    cex.axis = cex.axis*get_cex_for_multifigure(),
    cex.lab = cex.lab*get_cex_for_multifigure(),
    mgpy = mgpy,
    mgpx = mgpx,
    col=col, col.axis=col.axis,
    ...
  )
}

rave_axis.default = .rave_axis
rave_axis <- rave::rave_context_generics('rave_axis', .rave_axis)

rave_axis.rave_running_local = function(side, at, ..., cex.axis=1,
                                        cex.lab=1, mgpx=c(3,.4,0), mgpy=c(3,.5,0)) {
  rave_axis.default(side=side, at=at,
                    cex.axis=cex.axis, cex.lab=cex.lab, ...)
}

.rave_title <- function(main, cex=rave_cex.main, col, font=1) {
  if(missing(col)) {
    col = if(par('bg') == 'black') {
      'white'
    } else if (par('bg') == '#1E1E1E'){
      'gray70'
    } else {
      'black'
    }
  }
  
  title(main=list(main, cex=cex*get_cex_for_multifigure(), col=col, font=font))
}
rave_title.default = .rave_title
rave_title <- rave::rave_context_generics('rave_title', .rave_title)

rave_title.rave_running_local <- function(main, ..., cex=1) {
  rave_title.default(main=main, cex=cex)
}

.rave_axis_labels <- function(xlab=NULL, ylab=NULL, col=NULL, cex.lab=rave_cex.lab, line=NA, ...) {
  col %?<-% get_foreground_color()
  
  ## overrule the font scaling if we're writing to a PDF... eventually we'll make this better
  if('pdf' == names(dev.cur())) {
    if (cex.lab > 1)
      cex.lab = 1
    
    print('in RAL')
    
    if(!is.null(ylab)) {
      yline = if(is.na(line)) {
        2.5 
      } else {
        line
      }
      title(xlab=NULL, ylab=ylab,
            cex.lab=cex.lab*get_cex_for_multifigure(),
            col.lab=col, line=yline, ...)
    }
    
    if(!is.null(xlab)) {
      xline = if(is.na(line)) {
        1.5 
      } else {
        line
      }
      
      title(xlab=xlab, ylab=NULL,
            cex.lab=cex.lab*get_cex_for_multifigure(),
            col.lab=col, line=xline, ...)
    }
    
    return()
  }
  
  title(xlab=xlab, ylab=ylab, cex.lab=cex.lab*get_cex_for_multifigure(), col.lab=col, line=line, ...)
}

rave_axis_labels.default= .rave_axis_labels
rave_axis_labels <- rave::rave_context_generics('rave_axis_labels', .rave_axis_labels)

rave_axis_labels.rave_running_local <- function(..., line=-101, xlab=NULL, ylab=NULL, cex.lab=1) {
  if(!is.null(xlab)) {
    if(line < -100) line = 1.5
    rave_axis_labels.default(..., xlab=xlab, ylab=NULL, line=line, cex.lab=cex.lab)
  }
  if(!is.null(ylab)) {
    if(line < -100) line=2.5
    rave_axis_labels.default(..., ylab=ylab, xlab=NULL, line=line, cex.lab=cex.lab)
  }
}

default_plot <- function() {
  plot_clean(1, 1, type='n', main='No Conditions Specified')
}

ebars.x = function(x, y, sem, length = 0.05, code = 3, ...) {
  arrows(x - sem, y, x + sem, y, angle = 90, code = code, length = length, ...)
}

ebars.y = function(x, y, sem, length = 0.05, up = T, down = T, code = 2, ...) {
  if (up) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y + sem), angle = 90, code = code, length = length, ...)
  }
  if (down) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y - sem), angle = 90, code = code, length = length, ...)
  }
}

plus_minus <- #rutabaga::plus_minus
function (x, d) 
{
  if (missing(d) & is.matrix(x)) {
    d <- x[, 2]
    x <- x[, 1]
    
    if(any(is.na(d))) d[is.na(d)] = 0
  }
  c(x - d, x + d)
}

abs_cdiff <- function(m) {
  if(!is.matrix(m))
    return (1)

  abs(apply(m, 1, diff))
}


se <- function(x, na.rm=FALSE) sd(x, na.rm=na.rm) / sqrt(sum(not_NA(x)))


# needed to simplify long expressions
colDiff <- function(m, ord=1:2) m[,ord[1]] - m[,ord[2]]

# 0-1 scale the data so we can manage the plot ranges easily
scl01 <- function(x) (x-min(x)) / diff(range(x))

# relying on a generic here
pretty.fres <- function(fres) {
  # don't save intermediate results back into fres or else it changes the type into character,
  # messing up following lines
  c(
    # R2
    ifelse(fres[1] < 0.01, '<0.01', round(fres[1],2)),
    #F stat
    ifelse(fres[2] < 0.01, '<0.01', round(fres[2],1)),
    #p value
    format(fres[3], digits=1)
  ) %>% `class<-`(c('fres', 'character'))
}


pretty.tres <- function(tres) {
  mapply(format, tres, digits=c(2,2,1)) %>%
    set_names(c('m', 't', 'p')) %>% `class<-`(c('tres', 'character'))
}

as.title <- function(res, ...) {
  UseMethod('as.title')
}

as.title.fres <- function(res, ...) {
  bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(res[1]) ~ ',' ~ F == .(res[2]) * ','~ p==.(res[3]))
}

as.title.tres <- function(res,...) {
  bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(res[1]) * ',' ~ t == .(res[2]) * ',' ~ p==.(res[3]))
}


# allow color cycling
get_color <- function(ii) {
  group_colors[(ii - 1) %% length(group_colors) + 1]
}

rave_colors <- list('BASELINE_WINDOW'='gray60', 'ANALYSIS_WINDOW' = 'salmon2', 'GROUP'=group_colors,
                    'TRIAL_TYPE_SEPARATOR'='gray40', 'DARK_GRAY' = '#1E1E1E', 'BAR_PLOT_ALPHA' = 0.7)
rave_colors[tolower(names(rave_colors))] = rave_colors


get_shifted_tensor <- function(raw_tensor, shift_amount, new_range, dimnames, varnames, shift_idx=3, shift_by=1, data_env = rave::getDefaultDataRepository()) {
  stopifnot(exists("module_tools", envir = data_env))
  mt = data_env$module_tools
    
  shifted_array = dipsaus::shift_array(raw_tensor, shift_idx = 3, shift_by = 1, shift_amount = shift_amount)
  
  shifted_tensor = ECoGTensor$new(data = shifted_array, dim=dim(raw_tensor), dimnames = dimnames, varnames = varnames, hybrid=FALSE)
  
  # here we're going to start time at the right spot, but then we're going to let it go past the "correct" time into the NA territory
  # then we'll subset later. this avoids having a dimname with length unequal to the dimension it's naming
  shifted_tensor$dimnames$Time = round(seq(from=new_range[1], by = 1/mt$get_sample_rate(), length.out = dim(raw_tensor)[3L]), 7)
  
  shifted_tensor = shifted_tensor$subset(Time = Time %within% new_range)
  
  return(shifted_tensor)
}

get_events_data <- function(epoch_event_types, data_env = rave::getDefaultDataRepository()) {
  stopifnot(exists("module_tools", envir = data_env))
  mt = data_env$module_tools
  
  ep = mt$get_meta('trials')
  ev = ep[c('Trial', 'Time', 'Condition')]
  
  eet = epoch_event_types %>% str_subset('Trial Onset', negate = TRUE)
  if(length(eet) > 0) {
    ev[eet] = ep[,'Event_' %&% eet]
    
    ev[eet] %<>% lapply(function(x) {
      as.numeric(x) - as.numeric(ep$Time)
    })
  }
  
  ev %<>% as.data.frame
  
  return(ev)
}

signed_floor <- function(x) {
  sign(x)*floor(abs(x))
}

determine_available_shift <- function(event_of_interest, available_time, epoch_information, data_env = rave::getDefaultDataRepository()) {
  stopifnot(exists("module_tools", envir = data_env))
  sr = data_env$module_tools$get_sample_rate()
  
  # round(range(as.numeric(epoch_information[[event_of_interest]]) - as.numeric(epoch_information$Time)), 7)
  # the events file is assumed to already be referenced to the trial start time
  time_range = round(range(epoch_information[[event_of_interest]]),7)
  
  # we want to be conservative, so we want to shrink the range as needed. so we use signed_floor
  signed_floor(sr * (available_time-time_range))/sr
}

determine_shift_amount <- function(available_shift, event_time, data_env = rave::getDefaultDataRepository()) {
  stopifnot(exists("module_tools", envir = data_env))
  mt = data_env$module_tools
  
  new_range_ind = abs(round(available_shift*mt$get_sample_rate()))
  
  # we need to add one here to account for 0
  new_0_ind = 1 + round(mt$get_sample_rate()*(event_time - min(mt$get_power()$dimnames$Time)))
  
  return(new_0_ind - new_range_ind[1])
}

is.blank <- function(x){
  isTRUE(x == '')
}


### UI impl to share the PDF exporting code where possible

customDownloadButton <- function(outputId, label='Download', class=NULL, icon_lbl="download", ...) {
  tags$a(id = outputId,
         class = paste("btn btn-default shiny-download-link", class),
         href = "", target = "_blank", download = NA, 
         icon(icon_lbl), label, ...)
}


custom_plot_download_impl <- function(module_id, choices, selected=choices[1], w=4,h=3) {
  force(choices); force(selected); force(w); force(h)
  
  return(function()  {
    ns <- shiny::NS(module_id)
    
    tagList(div(class='rave-grid-inputs',
                div(style='flex-basis: 100%', selectInput(ns('custom_plot_select'), label = "Choose Graph",
                                                          choices = choices, selected =selected)),
                div(style='flex-basis: 25%', numericInput(ns('custom_plot_width'), label='width (inches)', value=w, min=5, step = 1)),
                div(style='flex-basis: 25%', numericInput(ns('custom_plot_height'), label='height (inches)', value=h, min=3, step = 1)),
                div(style='flex-basis: 25%', selectInput(ns('custom_plot_file_type'), label='File Type',
                                                         choices=c('pdf', 'jpeg', 'png', 'tiff', 'svg'), selected='pdf')),
                div(style='flex-basis:100%', checkboxInput(ns('save_hires_plot_to_server'), label='Save a copy on the server')),
                
                div(style='flex-basis: 100%', customDownloadButton(ns('btn_custom_plot_download'),
                                                                   label = "Download Graph", icon_lbl = 'file-image'))
    ))
  })
}

build_file_output_args <- function(ftype, width, height, outfile) {
  args = list(width=width, height=height, filename=outfile)
  if(ftype == 'jpeg') {
    args$units = 'in'
    args$quality=99
    args$res = 300
  } else if(ftype == 'pdf') {
    args$useDingbats = FALSE
    args$file = outfile
    args$filename = NULL
  } else if (ftype == 'tiff') {
    args$units = 'in'
    args$compression = 'lzw'
    args$res = 300
  }
  
  return(args)
}

which.equal <- function(needle, haystack) which(needle == haystack)

# append the number of electrodes in each label onto the label
make_label_with_count <- function(lbl) {
  tbl = table(lbl)
  
  new_lbl = paste0(names(tbl), '[',tbl,']')
  
  attr(new_lbl, 'map') = sapply(lbl, which.equal, names(tbl))
  
  return(new_lbl)
}

# remove the [5] or [10] showing the # available electrodes
# from the end of the freesurfer labels
remove_count_from_label <- function(str) {
  stringr::str_remove_all(str, '\\[[0-9]*\\]$')
}


# put this in one place, so we're removing hemi labels consistently
remove_hemisphere_labels <- function(str) {
  stringr::str_replace_all(str, 
                           c('L ' = '', 'R ' = ''))
}

remove_gyrus_sulcus_labels <- function(str) {
  stringr::str_replace_all(str,
                           c('GS ' = '', 'G ' ='', 'S '=''))
}



