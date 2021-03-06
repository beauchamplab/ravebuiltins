# This function helps you get rid of dev check warnings, however, you should avoid using this function as cran check will fail you
get_from_package <- function (f, pkg, internal = FALSE, ifNotFound = NULL, check = TRUE) 
{
  if (!check || package_installed(pkg)) {
    export_f = ifelse(internal, ":::", "::")
    f = do.call(export_f, list(pkg = pkg, name = f))
    return(f)
  }
  return(ifNotFound)
}

package_installed <- function (pkg) 
{
    system.file("", package = pkg) != ""
}

col2hex <- function(col, alpha = NULL, prefix = '#'){
  col = grDevices::col2rgb(col, alpha = FALSE) / 255
  col = grDevices::rgb(red = col[1,], green = col[2,], blue = col[3,], alpha = alpha)
  stringr::str_replace(col, '^[^0-9A-F]*', prefix)
}

htmltable_coefmat <- function(
  x, caption = NULL, digits = max(3L, getOption("digits") - 2L), 
  signif.stars = getOption("show.signif.stars"), 
  signif.legend = signif.stars, 
  dig.tst = max(1L, min(5L, digits - 1L)), 
  k = 3,
  cs.ind = 1:k, tst.ind = k + 1,
  zap.ind = integer(), 
  nc = ncol(x), 
  P.values = NULL, 
  has.Pvalue = nc >= 4L && length(cn <- colnames(x)) && 
    substr(cn[nc], 1L, 3L) %in% c("Pr(", "p-v"), 
  eps.Pvalue = .Machine$double.eps, 
  na.print = "NA", quote = FALSE, right = TRUE, ...
){
  if (is.null(d <- dim(x)) || length(d) != 2L) 
    stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  if (is.null(P.values)) {
    scp <- getOption("show.coef.Pvalues")
    if (!is.logical(scp) || is.na(scp)) {
      warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
      scp <- TRUE
    }
    P.values <- has.Pvalue && scp
  } else if (P.values && !has.Pvalue) {
    stop("'P.values' is TRUE, but 'has.Pvalue' is not")
  }
  
  if (has.Pvalue && !P.values) {
    d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
    nc <- nc - 1
    has.Pvalue <- FALSE
  } else {
    xm <- data.matrix(x)
  }
  k <- nc - has.Pvalue - ifelse (missing(tst.ind), 1, length(tst.ind)) 
  if (!missing(cs.ind) && length(cs.ind) > k) {
    stop("wrong k / cs.ind")
  }
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 0])) 
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- format(round(coef.se, max(1L, digits - 
                                                  digmin)), digits = digits)
    }
  }
  if (length(tst.ind)) 
    Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
                            digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc)))) 
    for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
  ok[, tst.ind] <- FALSE
  okP <- if (has.Pvalue) ok[, -nc] else ok
  x1 <- Cf[okP]
  dec <- getOption("OutDec")
  if (dec != ".") x1 <- chartr(dec, ".", x1)
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1L, 
                                                                    digits - 1L))
  }
  if (any(ina)) Cf[ina] <- na.print
  if (P.values) {
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
      warning("option \"show.signif.stars\" is invalid: assuming TRUE")
      signif.stars <- TRUE
    }
    if (any(okP <- ok[, nc])) {
      pv <- as.vector(xm[, nc])
      Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, 
                                 eps = eps.Pvalue)
      signif.stars <- signif.stars && any(pv[okP] < 0.1)
      if (signif.stars) {
        Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "))
        Cf <- cbind(Cf, format(Signif))
      }
    } else signif.stars <- FALSE
  } else signif.stars <- FALSE
  
  # Make Cf a table
  # print.default(Cf, quote = quote, right = right, na.print = na.print, ...)
  re = list()
  tags = shiny::tags
  rnames = rownames(Cf)
  
  if(length(caption) != 1){
    caption = NULL
  }
  sleg = ''
  if (signif.stars && signif.legend) {
    if ((w <- getOption("width")) < nchar(sleg <- attr(Signif, "legend"))){
      sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
    } 
    # cat("---\nSignif. codes:  ", sleg, sep = "", fill = w + 4 + max(nchar(sleg, "bytes") - nchar(sleg)))
    re$signif = sleg
    sleg = tagList(
      br(),
      sprintf(' - Signif. codes: %s', sleg)
    )
  }
  
  re$table = tags$div(
    class = 'table-responsive',
    tags$table(
      class = 'table table-striped table-sm',
      tags$caption(caption, ' ', tags$small(sleg)),
      tags$thead(
        tags$tr(
          lapply(c('', colnames(Cf)), tags$th)
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(Cf)), function(ii){
          v = c(rnames[ii], Cf[ii,]); names(v) = NULL
          tags$tr(lapply(v, tags$td))
        })
      )
    )
  )
  
  re
}
