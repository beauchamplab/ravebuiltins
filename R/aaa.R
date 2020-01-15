# Imports from other packages

#' @import dipsaus
#' @import rutabaga
#' @import rave
#' @import shiny
#' @importFrom magrittr %>%
#' @import stringr
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom magrittr extract2
#' @importFrom magrittr extract
#' @importFrom magrittr set_rownames
#' @importFrom magrittr equals
#' @import rlang
#' @import lmerTest
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
#' 
#' @import graphics
#' 
#' @importFrom stats median
#' @importFrom stats median.default
#' @importFrom stats pt
#' @importFrom stats quantile
#' @importFrom stats p.adjust
#' @importFrom stats symnum
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
rave_axis <- function(side, at, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis,
                      cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), col, col.axis, ...) {
  
  # if the color isn't specified, then we are free to set the color to what we want.
  # let's set it to be black, unless the background color is black, then we'll do white
  col %?<-% get_foreground_color()
  col.axis %?<-% col
  
  ruta_axis(
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

default_plot <- function() {
  plot_clean(1, 1, type='n', main='No Conditions Specified')
}


ebars.x = function(x, y, sem, length = 0.05, ...) {
  arrows(x - sem, y, x + sem, y, angle = 90, code = 3, length = length, ...)
}

ebars.y = function(x, y, sem, length = 0.05, up = T, down = T, code = 2, ...) {
  if (up) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y + sem), angle = 90, code = code, length = length, ...)
  }
  if (down) {
    arrows(x0 = x, y0 = as.numeric(y), y1 = as.numeric(y - sem), angle = 90, code = code, length = length, ...)
  }
}

plus_minus <- rutabaga::pm


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
                    'TRIAL_TYPE_SEPARATOR'='gray40')

rave_title <- function(main, cex=rave_cex.main, col, font=1) {
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


is.blank <- function(x){
  isTRUE(x == '')
}
