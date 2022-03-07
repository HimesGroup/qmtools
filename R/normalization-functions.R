##' Probabilistic quotient normalization (PQN)
##'
##' Performs probabilistic quotient normalization (PQN) on a matrix-like object
##' where rows present features and columns represent samples.
##'
##' For the calculation of quotients, a reference spectrum needs to be obtained
##' from a median or mean spectrum based on all spectra of the study or a subset
##' of the study. Feature intensities are normalized by the median of quotients.
##' See Dieterle et al. (2006) for details.
##'
##' @param x A matrix-like object.
##' @param ref_samples A vector of sample names or indices specifying reference
##'   samples for the calculation of quotients. Must be a subset of
##'   \code{colnames(x)} if it is a character vector. If \code{NULL}, all
##'   samples are used.
##' @param min_frac A numeric value between 0 and 1 specifying a minimum
##'   proportion of reference samples for features to be included in the
##'   calculation of a reference spectrum.
##' @param type A method to compute a reference spectrum. Either "median" or
##'   "mean".
##' @return A matrix of the same dimension as \code{x} containing the normalized
##'   intensities.
##'
##' @references
##'
##' Dieterle F, Ross A, Schlotterbeck G, Senn H. Probabilistic quotient
##' normalization as robust method to account for dilution of complex biological
##' mixtures. Application in 1H NMR metabonomics. Anal Chem. 2006 Jul
##' 1;78(13):4281-90. doi: 10.1021/ac051632c. PMID: 16808434.
##'
##' @seealso See [normalizeIntensity] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##' 
##' @export
normalizePQN <- function(x, ref_samples = NULL, min_frac = 0.5,
                         type = c("median", "mean")) {
  ## The reference paper suggests to apply integral normalization prior to PQN
  ## so consider to add that.
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  type <- match.arg(type)
  if (is.null(ref_samples)) {
    quotients <- .get_quotients(x, ref = x, min_frac = min_frac, type = type)
  } else {
    quotients <- .get_quotients(x, ref = x[, ref_samples, drop = FALSE],
                                min_frac = min_frac, type = type)
  }
  medians <- apply(quotients, 2, median, na.rm = TRUE)
  sweep(x, 2, medians, FUN = "/")
}

.get_quotients <- function(x, ref, min_frac, type) {
  if (min_frac > 0) {
    non_missing_frac <- rowSums(!is.na(m)) / ncol(m)
    idx_to_keep <- which(non_missing_frac >= min_frac)
    ref_sub <- ref[idx_to_keep, , drop = FALSE]
    x_sub <- x[idx_to_keep, , drop = FALSE]
    if (type == "median") {
      ref_summary <- apply(ref_sub, 1, median, na.rm = TRUE)
    } else {
      ref_summary <- rowMeans(ref_sub, na.rm = TRUE)
    }
    apply(x_sub, 2, function(x) x / ref_summary)
  } else {
    if (type == "median") {
      ref_summary <- apply(ref, 1, median, na.rm = TRUE)
    } else {
      ref_summary <- rowMeans(ref, na.rm = TRUE)
    }
    apply(x, 2, function(x) x / ref_summary)
  }
 }

##' Scale along columns (samples)
##'
##' Function to scale a matrix of intensity data along the columns (samples).
##'
##' Sample intensities are divided by the column sums ("div.sum"), means
##' ("div.mean"), medians ("div.median"), or median absolute deviations
##' ("div.mad").
##' 
##' @param x A matrix-like object.
##' @param type A scaling method to use.
##' @param restrict A logical specifying whether only features that are common
##'   to all samples are used in the calculation of scaling factors.
##' @param rescale A logical specifying whether the normalized intensities are
##'   re-scaled by multiplying the median of normalization factors to make look
##'   similar to the original scale.
##' @return A matrix of the same dimension as \code{x} containing the scaled
##'   intensities.
##'
##' @seealso See [normalizeIntensity] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##' 
##' @export
scaleCols <- function(x,
                      type = c("div.sum", "div.mean", "div.median", "div.mad"),
                      restrict = FALSE, rescale = FALSE) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  type <- match.arg(type)
  if (restrict) {
    scale_factors <- .get_scale_factors(na.omit(x), type)
  } else {
    scale_factors <- .get_scale_factors(x, type)
  }
  if (rescale) {
    scale_factors <- scale_factors / median(scale_factors)
  }
  sweep(x, 2, scale_factors, FUN = "/")
}

.get_scale_factors <- function(x, type) {
  switch(
    type,
    div.sum = colSums(x, na.rm = TRUE),
    div.mean = colMeans(x, na.rm = TRUE),
    div.median = apply(x, 2, median, na.rm = TRUE),
    div.mad = apply(x, 2, mad, na.rm = TRUE)
  )
}

##' Scale along rows (features)
##' 
##' Function to scale a matrix of intensity data along the rows (features), as
##' described in van den Berg et al. (2006).
##'
##' This function will do the following:
##' 
##'   - Auto scaling (unit variance scaling): each feature is mean-centered
##'     and divided by its standard deviation.
##'   - Range scaling: each feature is mean-centered and divided by its range.
##'   - Pareto scaling: each feature is mean-centered and divided by the
##'     square root of its standard deviation.
##'   - Vast scaling (variance stability scaling): it is an extension of auto
##'     scaling, using the product of standard deviation and coefficient of
##'     variation as a scale factor.
##'   - Level scaling: each feature is mean-centered and divided by its mean.
##'   - Sum scaling: each feature is divided by its sum.
##'   - Max scaling: each feature is divided by its maximum.
##'
##' @param x A matrix-like object.
##' @param type A scaling method to use.
##' @return A matrix of the same dimension as \code{x} containing the scaled
##'   intensities.
##'
##' @references
##' 
##' van den Berg RA, Hoefsloot HC, Westerhuis JA, Smilde AK, van der Werf MJ.
##' Centering, scaling, and transformations: improving the biological
##' information content of metabolomics data. BMC Genomics. 2006 Jun 8;7:142.
##' doi: 10.1186/1471-2164-7-142. PMID: 16762068; PMCID: PMC1534033.
##'
##' @seealso See [normalizeIntensity] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' @export
scaleRows <- function(x, type = c("auto", "range", "pareto",
                                  "vast", "level", "sum", "max")) {
  type <- match.arg(type)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  switch(
    type,
    auto = t(apply(x, 1, .auto_scale, na.rm = TRUE)),
    range = t(apply(x, 1, .range_scale, na.rm = TRUE)),
    pareto = t(apply(x, 1, .pareto_scale, na.rm = TRUE)),
    vast = t(apply(x, 1, .vast_scale, na.rm = TRUE)),
    level = t(apply(x, 1, .level_scale, na.rm = TRUE)),
    sum = MsCoreUtils::normalize_matrix(x, method = "sum"),
    max = MSCoreUtils::normalize_matrix(x, method = "max")
  )
}

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

################################################################################
## Cyclic LOESS normalization
################################################################################
.normalize_cyclicloess <- function(x, type = c("fast", "affy", "pairs"),
                                   ...) {
  type <- match.arg(type)
  .verify_package("limma")
  limma::normalizeCyclicLoess(x, method = type, ...)
}
