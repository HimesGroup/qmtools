.poplin_normalize <- function(x,
                              method = c("pqn",  "sum", "mean", "median",
                                             "mad", "euclidean",
                                             "cyclicloess", # sample-based
                                             "auto", "range", "pareto",
                                             "vast", "level", # metabolite-based
                                             "vsn"),
           ...) {
    method <- match.arg(method)
    .normalize_fun_dispatch(x, method = method, ...)
}


.normalize_fun_dispatch <- function(x, method, ...) {
  switch(
    method,
    pqn = .poplin_normalize_pqn(x = x, ...),
    sum = .poplin_normalize_sum(x = x, ...),
    mean = .poplin_normalize_mean(x = x, ...),
    mad = .poplin_normalize_mad(x = x, ...),
    median = .poplin_normalize_median(x = x, ...),
    euclidean = .poplin_normalize_euclidean(x = x, ...),
    cyclicloess = .poplin_normalize_cyclicloess(x = x, ...),
    auto = .poplin_normalize_auto(x = x, ...),
    range = .poplin_normalize_range(x = x, ...),
    pareto = .poplin_normalize_pareto(x = x, ...),
    vast = .poplin_normalize_vast(x = x, ...),
    level = .poplin_normalize_level(x = x, ...),
    vsn = .poplin_normalize_vsn(x = x, ...)
  )
}


## PQN normalization
## The reference suggests to apply integral normalization prior to PQN so
## consider to add that.
.poplin_normalize_pqn <- function(x, ref_samples = NULL, min_frac = 0.5,
                                 type = c("mean", "median")) {
  type <- match.arg(type)
  if ((is.null(ref_samples))) {
    ref <- x
  } else {
    if (!(is.character(ref_samples) || is.numeric(ref_samples))) {
      stop ("'ref_samples' must be a vector of character or integer.")
    } else {
      if (is.character(ref_samples) &&
          !(all(ref_samples %in% colnames(x)))) {
        non_match <- setdiff(ref_samples, colnames(x))
        stop("Reference samples not found in colnames(x): ",
             non_match, call. = FALSE)
      } else if (is.numeric(ref_samples) &&
                 !(all(ref_samples >= 1 & ref_samples <= ncol(x)))) {
        stop("Subscript out of bound. 'ref_samples' must be within [1, ncol(x)].")
      } else {
        ref <- x[, ref_samples, drop = FALSE]
      }
    }
  }
  idx_to_keep <- .idx_to_keep_by_missing(ref, "feature", min_frac)
  ref_sub <- ref[idx_to_keep, , drop = FALSE]
  x_sub <- x[idx_to_keep, , drop = FALSE]
  ref_summary <- .mat_stats(ref_sub, margin = 1, type = type)
  quotients <- apply(x_sub, 2, function(x) x / ref_summary)
  medians <- .mat_stats(quotients, margin = 2, type = "median")
  sweep(x, 2, medians, FUN = "/")
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
.poplin_normalize_sum <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "sum"
  )
}

.poplin_normalize_mean <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "mean"
  )
}

.poplin_normalize_median <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "median"
  )
}

.poplin_normalize_mad <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "mad"
  )
}

.poplin_normalize_euclidean <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "euclidean"
  )
}

##' @importFrom stats mad
.normalize_columns <- function(x, method = c("sum", "mean", "median",
                                                 "mad", "euclidean"),
                               restrict = FALSE, rescale = FALSE) {
  method <- match.arg(method)
  if (restrict) {
    x_sub <- na.omit(x)
  } else {
    x_sub <- x
  }
  scale_factors <- switch(
    method,
    sum = colSums(x_sub, na.rm = TRUE),
    mean = colMeans(x_sub, na.rm = TRUE),
    median = apply(x_sub, 2, median, na.rm = TRUE),
    mad = apply(x_sub, 2, mad, na.rm = TRUE),
    euclidean = apply(x_sub, 2, function(x) sqrt(sum(x**2, na.rm = TRUE)))
    )
  if (rescale) {
    scale_factors <- scale_factors / median(scale_factors)
  }
  sweep(x, 2, scale_factors, FUN = "/")
}


################################################################################
## Cyclic LOESS normalization (taken from limma package 09/13/2021)
################################################################################
.poplin_normalize_cyclicloess <- function(x, pre_log2, weights = NULL, span = 0.7,
                                         iterations = 3, type = "fast") {
  if (pre_log2) {
    x <- log2(x)
  }
  .normalizeCyclicLoess(x, weights = weights, span = span,
                        iterations = iterations, type = type)
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
                                  iterations = 3, type = "fast")
	## Cyclic loess normalization of columns of matrix
	## incorporating probes weights.
	## Yunshun (Andy) Chen and Gordon Smyth
	## 14 April 2010.  Last modified 24 Feb 2012.
{
	x <- as.matrix(x)
	type <- match.arg(type, c("fast","affy","pairs"))
	n <- ncol(x)
	if (type == "pairs") {
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
	if (type == "fast") {
		for (k in 1:iterations) {
			a <- rowMeans(x, na.rm = TRUE)
			for (i in 1:n) {
				m <- x[, i] - a
				f <- .loessFit(m, a, weights = weights, span = span)$fitted
				x[, i] <- x[, i] - f
			}
		}
	}
	if (type == "affy") {
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

#################################################################################
## Feature scaler
#################################################################################
.auto_scale <- function(x, ...) {
  (x - mean(x, ...)) / sd(x, ...)
}

.range_scale <- function(x, ...) {
  (x - mean(x, ...)) / (max(x, ...) - min(x, ...))
}

.pareto_scale <- function(x, ...) {
  (x - mean(x, ...)) / sqrt(sd(x, ...))
}

.vast_scale <- function(x, ...) {
  .auto_scale(x, ...) * (mean(x, ...) / sd(x, ...))
}

.level_scale <- function(x, ...) {
  (x - mean(x, ...)) / mean(x, ...)
}

.poplin_normalize_auto <- function(x) {
  t(apply(x, 1, .auto_scale, na.rm = TRUE))
}

.poplin_normalize_range <- function(x) {
  t(apply(x, 1, .range_scale, na.rm = TRUE))
}

.poplin_normalize_pareto <- function(x) {
  t(apply(x, 1, .pareto_scale, na.rm = TRUE))
}

.poplin_normalize_vast <- function(x) {
  t(apply(x, 1, .vast_scale, na.rm = TRUE))
}

.poplin_normalize_level <- function(x) {
  t(apply(x, 1, .level_scale, na.rm = TRUE))
}

#################################################################################
## VSN: simply provides interface
#################################################################################
.poplin_normalize_vsn <- function(x, meanSdPlot = FALSE, ...) {
  if (!requireNamespace("vsn", quietly = TRUE)) {
    stop("Package 'vsn' is required. Please install and try again.")
  }
  out <- suppressMessages(vsn::vsnMatrix(x = x, ...))
  if (meanSdPlot) {
    if (!requireNamespace("hexbin", quietly = TRUE)) {
      stop("Package 'hexbin' is required to produce a meanSdPlot. ",
           "Please install and try again.")
    } else {
      vsn::meanSdPlot(out)
    }
  }
  Biobase::exprs(out)
}

