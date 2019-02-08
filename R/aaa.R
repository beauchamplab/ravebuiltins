# Imports from other packages

#' @importFrom rutabaga %&%
#' @importFrom rutabaga clip_x
#' @importFrom rutabaga do_if
#' @importFrom rutabaga ebar_polygon
#' @importFrom rutabaga ebars
#' @importFrom rutabaga format_f
#' @importFrom rutabaga format_stat
#' @importFrom rutabaga get_data_range
#' @importFrom rutabaga get_f
#' @importFrom rutabaga get_list_elements
#' @importFrom rutabaga get_t
#' @importFrom rutabaga getAlphaRGB
#' @importFrom rutabaga jitr
#' @importFrom rutabaga m_sd
#' @importFrom rutabaga m_se
#' @importFrom rutabaga not_NA
#' @importFrom rutabaga not_null
#' @importFrom rutabaga pm
#' @importFrom rutabaga rave_barplot
#' @importFrom rutabaga round_range
#' @importFrom rutabaga stretch
#' @importFrom rutabaga trim
#' @importFrom rutabaga trimmed.mean
#' @importFrom rutabaga trimmed.mse
#' @importFrom rutabaga ruta_axis
#' @importFrom rutabaga plot_clean
#' 
#' @importFrom rave %?<-%
#' 
#' @importFrom rlang quo
#' 
#' @importFrom methods is
#' 
#' @import shiny
#' @import rave
#' @import stringr
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang quo_squash
#' @import magrittr



rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4



rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')

rave_heat_map_colors <- rave_color_ramp_palette(1001)

# put this hear for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('orange', 'dodgerblue3', 'darkgreen', 'orangered', 'brown', 'purple3')

# Internal use, not exported
rave_axis <- function(side, at, tcl=-0.3, labels=at, las=1, cex.axis=rave_cex.axis, cex.lab=rave_cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), ...) {
  ruta_axis(
    side = side,
    at = at,
    tcl = tcl,
    labels = labels,
    las = las,
    cex.axis = cex.axis,
    cex.lab = cex.lab,
    mgpy = mgpy,
    mgpx = mgpx,
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

rave_main <- function(main, cex=rave_cex.main, col='black', font=1) {
  title(main=list(main, cex=cex, col=col, font=font))
}
