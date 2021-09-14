## PQN normalization
## The reference suggests to apply integral normalization prior to PQN so
## consider to add that.
.normalize.pqn <- function(x, dat_in, dat_out, ref_ids = NULL,
                 min_frac = 0.5, type = c("mean", "median")) {
  dat_m <- .verify_and_exract_input(x, dat_in)
  ## unnecessary match.arg
  type <- match.arg(type)
  if (dat_out %in% assayNames(x)) {
    stop("'dat_out' must not be one of assayNames(x): ",
         assayNames(x))
  }
  if (is.null(ref_ids)) {
    ref_m <- dat_m
  } else {
    if (!all(ref_ids %in% colnames(x))) {
      non_match <- setdiff(ref_ids, colnames(x))
      stop("Reference samples not found in colnames(x): ",
           non_match, call. = FALSE)
    } else {
      idx <- which(colnames(x) %in% ref_ids)
      ref_m <- dat_m[ , idx, drop = FALSE]
    }
  }
  idx_to_keep <- .idx_to_keep_by_missing(ref_m, "feature", min_frac)
  ref_m_sub <- ref_m[idx_to_keep, , drop = FALSE]
  dat_m_sub <- dat_m[idx_to_keep, , drop = FALSE]
  ref_summary <- .mat_stats(ref_m_sub, margin = 1, type = type)
  quotients <- apply(dat_m_sub, 2, function(x) x / ref_summary)
  medians <- .mat_stats(quotients, margin = 2, type = "median")
  poplin_data(x, dat_out) <- sweep(dat_m, 2, medians, FUN = "/")
  x
}

.idx_to_keep_by_missing <- function(m, margin = c("sample", "feature"),
                                    min_frac) {
  margin <- match.arg(margin)
  if (margin == "sample") {
    non_missing_frac <- colSums(!is.na(m)) / nrow(m)
    which(non_missing_frac >= min_frac)
  } else {
    non_missing_frac <- rowSums(!is.na(m)) / ncol(m)
    which(non_missing_frac >= min_frac)
  }
}

.mat_stats <- function(m, margin, type = c("mean", "median")) {
  type <- match.arg(type)
  switch(
    type,
    mean = apply(m, margin, mean, na.rm = TRUE),
    median = apply(m, margin, median, na.rm = TRUE)
  )
}

## other spectral function normalization methods
.normalize.sum <- function(x, dat_in, dat_out,
                           restrict = FALSE, rescale = FALSE) {
  .column_based_norm(
    x = x, dat_in = dat_in, dat_out = dat_out,
    restrict = restrict, rescale = rescale, normalizer = "sum"
  )
}

.normalize.mean <- function(x, dat_in, dat_out,
                            restrict = FALSE, rescale = FALSE) {
  .column_based_norm(
    x = x, dat_in = dat_in, dat_out = dat_out,
    restrict = restrict, rescale = rescale, normalizer = "mean"
  )

}

.normalize.median <- function(x, dat_in, dat_out,
                              restrict = FALSE, rescale = FALSE) {
  .column_based_norm(
    x = x, dat_in = dat_in, dat_out = dat_out,
    restrict = restrict, rescale = rescale, normalizer = "median"
  )
}

.normalize.mad <- function(x, dat_in, dat_out,
                           restrict = FALSE, rescale = FALSE) {
  .column_based_norm(
    x = x, dat_in = dat_in, dat_out = dat_out,
    restrict = restrict, rescale = rescale, normalizer = "mad"
  )
}

.normalize.euclidean <- function(x, dat_in, dat_out,
                                 restrict = FALSE, rescale = FALSE) {
  .column_based_norm(
    x = x, dat_in = dat_in, dat_out = dat_out,
    restrict = restrict, rescale = rescale, normalizer = "euclidean"
  )
}

##' @importFrom stats mad
.column_based_norm <- function(x, normalizer = c("sum", "mean", "median",
                                                 "mad", "euclidean"),
                         dat_in, dat_out, restrict = FALSE, rescale = FALSE) {
  dat_m <- .verify_and_exract_input(x, dat_in)
  if (dat_out %in% assayNames(x)) {
    stop("'dat_out' must not be one of assayNames(x): ",
         assayNames(x))
  }
  ## unnecessary match.arg
  normalizer <- match.arg(normalizer)
  if (restrict) {
    dat_m_sub <- na.omit(dat_m)
  } else {
    dat_m_sub <- dat_m
  }
  scale_factors <- switch(
    normalizer,
    sum = colSums(dat_m_sub, na.rm = TRUE),
    mean = colMeans(dat_m_sub, na.rm = TRUE),
    median = apply(dat_m_sub, 2, median, na.rm = TRUE),
    mad = apply(dat_m_sub, 2, mad, na.rm = TRUE),
    euclidean = apply(dat_m_sub, 2, function(x) sqrt(sum(x**2, na.rm = TRUE)))
    )
  if (rescale) {
    scale_factors <- scale_factors / median(scale_factors)
  }
  poplin_data(x, dat_out) <- sweep(dat_m, 2, scale_factors, FUN = "/")
  x
}


################################################################################
## Cyclic LOESS normalization (taken from limma package 09/13/2021)
################################################################################
.normalize.cyclicloess <- function(x, dat_in, dat_out, weights = NULL,
                                   span = 0.7, iterations = 3, method = "fast") {
  dat_m <- .verify_and_exract_input(x, dat_in)
  poplin_data(x, dat_out) <- .normalizeCyclicLoess(
    dat_m, weights = weights, span = span,
    iterations = iterations, method = method
  )
  x
}

##	LOESS FUNCTIONS
##' @importFrom stats lm.wfit loess
.loessFit <- function(y, x, weights = NULL, span = 0.3, iterations = 4L,
                     min.weight = 1e-5, max.weight = 1e5,
                     equal.weights.as.null = TRUE, method = "weightedLowess")
  ## Fast lowess fit for univariate x and y allowing for weights
  ## Uses lowess() if weights=NULL and weightedLowess(), locfit.raw() or loess() otherwise
  ## Gordon Smyth
  ## 28 June 2003.  Last revised 14 January 2015.
{
  ## Check x and y
	n <- length(y)
	if(length(x) != n) stop("y and x have different lengths")
	out <- list(fitted=rep(NA, n),residuals=rep(NA, n))

	obs <- is.finite(y) & is.finite(x)
	xobs <- x[obs]
	yobs <- y[obs]
	nobs <- length(yobs)

  ## If no good obs, exit straight away
	if(nobs == 0) return(out)

  ## Check span
	if(span < 1/nobs) {
		out$fitted[obs] <- y[obs]
		out$residuals[obs] <- 0
		return(out)
	}

  ## Check min.weight
	if(min.weight < 0) min.weight <- 0

  ## Check weights
	if(!is.null(weights)) {
		if(length(weights) != n) stop("y and weights have different lengths")
		wobs <- weights[obs]
		wobs[is.na(wobs)] <- 0
		wobs <- pmax(wobs,min.weight)
		wobs <- pmin(wobs,max.weight)
    ## If weights all equal, treat as NULL
		if(equal.weights.as.null) {
			r <- range(wobs)
			if(r[2] - r[1] < 1e-15) weights <- NULL
		}
	}

  ## If no weights, so use classic lowess algorithm
	if(is.null(weights)) {
		o <- order(xobs)
		lo <- lowess(x = xobs, y = yobs, f = span, iter = iterations - 1L)
		out$fitted[obs][o] <- lo$y
		out$residuals[obs] <- yobs - out$fitted[obs]
		return(out)
	}

  ## Count number of observations with positive weights (must always be positive)
	if(min.weight > 0)
		nwobs <- nobs
	else 
		nwobs <- sum(wobs > 0)

  ## Check whether too few obs to estimate lowess curve
	if(nwobs < (4 + 1 / span)) {
		if(nwobs == 1L) {
			out$fitted[obs] <- yobs[wobs > 0]
			out$residuals[obs] <- yobs-out$fitted[obs]
		} else {
			fit <- lm.wfit(cbind(1, xobs), yobs, wobs)
			out$fitted[obs] <- fit$fitted
			out$residuals[obs] <- fit$residuals
		}
		return(out)
	}

	## Need to compute lowess with unequal weights
	method <- match.arg(method, c("weightedLowess","locfit","loess"))
	switch(method,
         "weightedLowess" = {
           fit <- weightedLowess(x = xobs, y = yobs, weights = wobs, span = span,
                                 iterations = iterations, npts=200)
           out$fitted[obs] <- fit$fitted
           out$residuals[obs] <- fit$residuals
         },
         "locfit" = {
           ## Check for locfit package
           if(!requireNamespace("locfit", quietly = TRUE)) {
             stop("locfit required but is not installed (or can't be loaded)")
           }
           ## Weighted lowess with robustifying iterations
           biweights <- rep(1, nobs)
           for (i in 1:iterations) {
             fit <- locfit::locfit(yobs ~ xobs,
                                   weights = wobs*biweights,
                                   alpha = span, deg = 1)
             res <- residuals(fit, type = "raw")
             s <- median(abs(res))
             biweights <- pmax(1 - (res / (6 * s))^2, 0)^2
           }
           out$fitted[obs] <- fitted(fit)
           out$residuals[obs] <- res
         },
         "loess" = {
           ## Suppress warning "k-d tree limited by memory"
           oldopt <- options(warn = -1)
           on.exit(options(oldopt))
           bin <- 0.01
           fit <- loess(yobs ~ xobs, weights = wobs, span = span, degree = 1,
                        parametric = FALSE, normalize = FALSE,
                        statistics = "approximate", surface = "interpolate",
                        cell = bin / span, iterations = iterations,
                        trace.hat = "approximate")
           out$fitted[obs] <- fit$fitted
           out$residuals[obs] <- fit$residuals
         }
         )
	out
}


.normalizeCyclicLoess <- function(x, weights = NULL, span = 0.7,
                                  iterations = 3, method = "fast")
	## Cyclic loess normalization of columns of matrix
	## incorporating probes weights.
	## Yunshun (Andy) Chen and Gordon Smyth
	## 14 April 2010.  Last modified 24 Feb 2012.
{
	x <- as.matrix(x)
	method <- match.arg(method, c("fast","affy","pairs"))
	n <- ncol(x)
	if (method == "pairs") {
		for (k in 1:iterations)
      for (i in 1:(n - 1))
        for (j in (i + 1):n) {
          m <- x[, j] - x[, i]
          a <- .5 * (x[, j] + x[, i])
          f <- .loessFit(m, a, weights = weights, span = span)$fitted
          x[, i] <- x[, i] + f / 2
          x[, j] <- x[, j] - f / 2		
        }
	}
	if (method == "fast") {
		for (k in 1:iterations) {
			a <- rowMeans(x, na.rm = TRUE)
			for (i in 1:n) {
				m <- x[, i] - a
				f <- .loessFit(m, a, weights = weights, span = span)$fitted
				x[, i] <- x[, i] - f
			}
		}
	}
	if (method == "affy") {
		g <- nrow(x)
		for (k in 1:iterations) {
			adjustment <- matrix(0, g, n)
			for (i in 1:(n - 1))
        for (j in (i + 1):n) {
          m <- x[, j] - x[, i]
          a <- .5 * (x[, j] + x[, i])
          f <- .loessFit(m, a, weights = weights, span = span)$fitted
          adjustment[, j] <- adjustment[, j] + f
          adjustment[, i] <- adjustment[, i] - f
        }
			x <- x - adjustment / n
		}
	}
	x
}
