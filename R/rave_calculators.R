# fast ways to do common operations (this should get folded into the electrode class at some point?)
# some of them depend on RAVE globals (FREQUENCIES, TIME_RANGE, etc)

.fast_z <- function(x) {
    .l <- length(x)
    .m <- sum(x) / .l

    (x - .m) / sqrt((sum(x^2) - sum(x)^2 / .l)/(.l-1))
}

### Methods to aggregate vectors
electrode_transform <- function(method='none') {
    switch(method,
           'amplitude' = sqrt,
           'z-score' = .fast_z,
           'max-scale' = function(x) {
               x/max(x)
           },
           '0-1 scale' = function(x) {
               (x - min(x)) / diff(range(x))
           },
           'rank' = rank,
           IDENTITY_TRANSFORM
    )
}
IDENTITY_TRANSFORM <- function(x) x

get_by <- function(x, FUN, ..., negate=FALSE) {
  ind <- FUN(x, ...)
  if(negate)
    ind <- !ind
  
  x[ind]
}



# first take mean over frequency
# then grab mean and SE across trials
.fast_mean <- function(x) sum(x)/length(x)

# don't do NA checking here to keep the speed
# if you're doing this on a matrix, check out .fast_column_se and .colMeans
.fast_mse <- function(x) {
    C_cov <- get_from_package('C_cov', 'stats', internal = TRUE, check = FALSE)
    
    # apparently sum(x) / length(x) is faster than mean(x) -- because of the use of generics and input checking?
    c(sum(x)/length(x),
      sqrt(.Call(C_cov, x, NULL, 4, FALSE)/length(x)))
}

.fast_range <- function(x) c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))

.fast_pearson <- function(x,y) {
    C_cor <- get_from_package('C_cor', 'stats', internal = TRUE, check = FALSE)
    .Call(C_cor, x, y, 4, FALSE)
}

.fast_one_sample_t <- function(y, sided=1) {
    a <- .fast_mse(y)
    sided*(pt(a[1]/a[2], length(y)-1, lower.tail = FALSE))
}


.fast_one_sample_tscore <- function(y) {
    a <- .fast_mse(y)
    return(a[1]/a[2])
}


# returns the tail probability from a t-test (against 0) for each *ROW* in the supplied
# matrix
# Because we usually want  t > t*, we take the upper tail probability, lower.tail=FALSE
# by default we return a one-sided p-value, but you can multiply by 2 using sided=2
.fast_one_sample_t_pval_mat <- function(ymat, sided=1L, lower.tail=FALSE) {
    sided*pt(.fast_one_sample_tscore_mat(ymat), dim(ymat)[2L]-1L, lower.tail=lower.tail)
}

# returns the t-score from a t-test (against 0) for each *ROW* in the supplied matrix
.fast_one_sample_tscore_mat <- function(ymat) {
    .rowMeans(ymat, nrow(ymat), ncol(ymat)) / .fast_column_se(t(ymat))
}



# selecting out the diagonal from the cov matrix is faster than
# per-column se calculation,
# even if it means a transpose from the calling function
.fast_column_se <- function(y) {
    
    C_cov <- get_from_package('C_cov', 'stats', internal = TRUE, check = FALSE)
    
    sqrt(
        .fast_diagonal(.Call(C_cov, y, NULL, 4L, FALSE))/dim(y)[1L]
    )
}

.fast_column_sd <- function(y) {
    
    C_cov <- get_from_package('C_cov', 'stats', internal = TRUE, check = FALSE)
    
    sqrt(
        .fast_diagonal(.Call(C_cov, y, NULL, 4, FALSE))
    )
}

#ripped this from base::diag, thanks!
.fast_diagonal <- function(y) {
    y[1L + 0L:(min(dim(y)) - 1L) * (dim(y)[1L] + 1L)]
}

.fast_se <- function(x) {
    C_cov <- get_from_package('C_cov', 'stats', internal = TRUE, check = FALSE)
    
    sqrt(.Call(C_cov, x, NULL, 4L, FALSE)/length(x))
}


# x is trimmed from RIGHT
# y is trimmed from LEFT
get_lagged_vectors <- function(x, y, amt) {
  cbind(
    x[1:(length(x)-amt)],
    y[(amt+1):length(y)]
  )
}


## 
lagged_cor <- function(x, y, method='pearson', len=20) {
  stopifnot(length(x) == length(y))
  
  if (len > length(x))
    len <- length(x)-3
  
  stopifnot(len < length(x))
  
  # sapply(0:len, function(r) {
  #   xS <- x[1:(length(x)-r)]
  #   yS <- y[(r+1):length(y)]
  #   cor(xS,yS, method=method)
  # })
  
  sapply(0:len, function(r) {
    cor(get_lagged_vectors(x, y, r), method=method)[2,1]
  })
}

