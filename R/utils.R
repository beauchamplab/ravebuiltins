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

htmltable_mat <- function(mat, ...) {
  re = list()
  tags = shiny::tags
  
  re$table = tags$div(
    class = 'table-responsive',
    tags$table(
      class = 'table table-striped table-sm',
      tags$thead(
        tags$tr(
          lapply(colnames(mat), tags$th)
        )
      ),
      tags$tbody(
        
        apply(mat, 1, function(m) {
          tags$tr(
            lapply(m, tags$td)
          )
        })
      )
    )
  )
  return(re)
}

ravebuiltins_check_installation <- function(...) {
  res <- ravebuiltins_finalize_installation(check_only = TRUE, upgrade = 'always')
  
  str <- FALSE
  if(any(res)) {
    msg = "Ravebuiltins is missing: " %&% paste(names(res), collapse=', ')
    
    attr(str, 'message') <- msg
    
  } else {
    return (T)
  }
  
  return(str)
}


ravebuiltins_finalize_installation <- function(upgrade=c('ask', 'always', 'never'), ...,
                                               new_timeout=60*10, check_only=FALSE) {
  old_to <- options('timeout')
  
  on.exit({
    options(timeout=old_to)
  }, add=TRUE)
  
  options(timeout = new_timeout)
  
  upgrade = match.arg(upgrade)
  
  mk.path <- function(...) {
    normalizePath(file.path(...), mustWork = FALSE)
  }

  paths <- c(
    'dsd' = mk.path(rave::rave_options('data_dir'), 'demo', 'DemoSubject'),
    'srd' = mk.path(rave::rave_options('raw_data_dir'), 'DemoSubject'),
    'dgd' = mk.path(rave::rave_options('data_dir'), 'demo', '_project_data', 'power_explorer', 'exports', 'YAB_demo_export-20210525-072121.fst'),
    'rmrk' = mk.path(rave::rave_options('data_dir'), '..', 'others', 'ravebuiltins', 'markdown', 'powerexplorer-pptx.Rmd')
  )
  
  checks <- list(
    'Demo Subject Data' = dir.exists(paths['dsd']) && dir.exists(paths['srd']),
    # 'Demo Subject Raw Data' = dir.exists(paths['srd']),
    'Demo Group Data' = file.exists(paths['dgd']),
    'RMarkdown Templates' = file.exists(paths['rmrk'])
  )
  
  needs <- mapply(function(ck, nm) {
    if(ck) {
      if(upgrade == 'ask') {
        return (dipsaus::ask_yesno("Re-install " %&% nm %&% "?"))
      } else if(upgrade == 'never') {
        return (FALSE)
      }
    }
    return (TRUE)
  }, checks, names(checks))
  
  
  
  if(check_only) {
    names(needs) <- names(checks)
    return (needs)
  }
  
  warn_dl <- function(nm, url, to) {
    dipsaus::cat2(level='WARNING',
                  sprintf('Unable to download %s...\n\t\tTry to download them from here: %s \n \t\tand unzip into here %s\n',
                    nm, url, to))
  }
  
  ### Demo subject data files
  if(needs[1]) {
    dipsaus::cat2(level='DEFAULT', 'Downloading demo subject data...')
    rave::download_subject_data("https://github.com/beauchamplab/rave/releases/download/v0.1.9-beta/DemoSubjectFull.zip",
                                replace_if_exists = TRUE)
  }
  
  ## Demo group data files
  if(needs[2]) {
      url_str = "https://github.com/beauchamplab/rave/releases/download/v0.1.9-beta/DemoGroupData.zip"
      res = utils::download.file(url_str,
                          destfile = f <- tempfile(fileext = ".zip"))
      extract_to = mk.path(rave::rave_options('data_dir'), 'demo')
      if(res==0) {
        utils::unzip(overwrite = TRUE, f, exdir = extract_to)
      } else {
        warn_dl('Demo Group Data', url_str, extract_to)
      }
  }
  
  
  if(needs[3]) {
    rt_url = "https://github.com/beauchamplab/rave/releases/download/v0.1.9-beta/rmarkdown_templates.zip"
    res = utils::download.file(rt_url,
                               destfile = f <- tempfile(fileext = ".zip"))
    if(res==0) {
      extract_to = mk.path('~', 'rave_data', 'others')
      raveio::dir_create2(extract_to)
      utils::unzip(f, exdir = extract_to)
    } else {
      warn_dl("RMarkdown templates", rt_url, extract_to)
    }
  }
}
