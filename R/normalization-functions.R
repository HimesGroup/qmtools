##' @importFrom stats cor fitted lowess median model.matrix na.omit
##' @importFrom stats prcomp predict residuals sd
.poplin_normalize <- function(x,
                              method = c("pqn",  "sum", "mean", "median",
                                         "mad", "cyclicloess", "vsn", "scale"),
                              ...) {
  method <- match.arg(method)
  .normalize_fun_dispatch(x, method = method, ...)
}


.normalize_fun_dispatch <- function(x, method, ...) {
  switch(
    method,
    pqn = .normalize_pqn(x = x, ...),
    sum = .normalize_sum(x = x, ...),
    mean = .normalize_mean(x = x, ...),
    mad = .normalize_mad(x = x, ...),
    median = .normalize_median(x = x, ...),
    cyclicloess = .normalize_cyclicloess(x = x, ...),
    vsn = .normalize_vsn(x = x, ...),
    scale = .normalize_scale(x = x, ...)
  )
}


## PQN normalization
## The reference suggests to apply integral normalization prior to PQN so
## consider to add that.
.normalize_pqn <- function(x, ref_samples = NULL, min_frac = 0.5,
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
        stop(
          "Subscript out of bound. 'ref_samples' must be within [1, ncol(x)]."
        )
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
.normalize_sum <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "sum"
  )
}

.normalize_mean <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "mean"
  )
}

.normalize_median <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "median"
  )
}

.normalize_mad <- function(x, restrict = FALSE, rescale = FALSE) {
  .normalize_columns(
    x = x, restrict = restrict, rescale = rescale, method = "mad"
  )
}

.normalize_euclidean <- function(x, restrict = FALSE, rescale = FALSE) {
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
.normalize_cyclicloess <- function(x, pre_log2, weights = NULL, span = 0.7,
                                   iterations = 3,
                                   type = c("fast", "affy", "pairs")) {
  type <- match.arg(type)
  if (pre_log2) {
    x <- log2(x)
  }
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package 'limma' is required. Please install and try again.")
  }
  limma::normalizeCyclicLoess(x, weights = weights, span = span,
                              iterations = iterations, method = type)
}

#################################################################################
## VSN: simply provides interface
#################################################################################
.normalize_vsn <- function(x, meanSdPlot = FALSE, ...) {
  if (!requireNamespace("vsn", quietly = TRUE)) {
    stop("Package 'vsn' is required. Please install and try again.")
  }
  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop("Package 'Biobase' is required. Please install and try again.")
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

.normalize_auto <- function(x) {
  t(apply(x, 1, .auto_scale, na.rm = TRUE))
}

.normalize_range <- function(x) {
  t(apply(x, 1, .range_scale, na.rm = TRUE))
}

.normalize_pareto <- function(x) {
  t(apply(x, 1, .pareto_scale, na.rm = TRUE))
}

.normalize_vast <- function(x) {
  t(apply(x, 1, .vast_scale, na.rm = TRUE))
}

.normalize_level <- function(x) {
  t(apply(x, 1, .level_scale, na.rm = TRUE))
}

.normalize_scale <- function(x, type = c("auto", "range", "pareto",
                                         "vast", "level")) {
  type <- match.arg(type)
  switch(
    type,
    auto = .normalize_auto(x),
    range = .normalize_range(x),
    pareto = .normalize_pareto(x),
    vast = .normalize_vast(x),
    level = .normalize_level(x)
  )
}

