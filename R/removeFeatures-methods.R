##' Feature Filtering methods
##'
##' Removes Features based on missing values, QC and blank samples. See the
##' details below.
##'
##' The method argument can be one of "missing", "blankratio", "rsd", "icc".
##'
##' * "missing" removes features based on proportions of missing values. Users
##' can specify one or more groups in samples. For multiple groups, a feature is
##' retained if there is at least one group with a proportion of non-missing
##' values above a cut-off.
##'
##' * For "blankratio", QC/blank intensity ratios are calculated for features
##' present at the blank samples. Features with a ratio below a cut-off will
##' be discarded.
##'
##' * "rsd" calculates a relative standard deviation (also known as
##' coefficient of variation) for each feature using QC samples. Features with a
##' RSD above a cut-off will be removed.
##'
##' * "icc" calculates an intraclass correlation coefficient (ICC) for each
##' feature using both biological and QC samples to identify how much of the
##' total variation is explained by biological variability, as described in
##' Schiffman, C et al (2019). Features with an ICC below a cut-off will be
##' removed.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param method A string specifying which filtering method to use.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param ... Arguments passed to a specific filtering method.
##' @return A matrix or \linkS4class{SummarizedExperiment} object.
##' 
##' @references
##'
##' Schiffman, C., Petrick, L., Perttula, K. et al. Filtering procedures for
##' untargeted LC-MS metabolomics data. BMC Bioinformatics 20, 334 (2019).
##' https://doi.org/10.1186/s12859-019-2871-9
##'
##' @seealso See [removeMiss], [removeBlankRatio], [removeRSD], and [removeICC]
##'     for the underlying functions that do work.
##'
##' @name removeFeatures
##'
##' @examples
##'
##' data(faahko_se)
##'
##' g <- colData(faahko_se)$sample_group
##'
##' ## SummarizedExperiment object
##' se <- removeFeatures(faahko_se, i = "raw", method = "missing",
##'                      group = g, cut = 0.9)
##'
##' ## Matrix
##' m <- assay(faahko_se, i = "raw")
##' removeFeatures(m, method = "missing", group = g, levels = "WT", cut = 0.9)
##' 
##'
NULL

##' @rdname removeFeatures
setMethod(
    "removeFeatures", "ANY",
    function(x, method = c("missing", "blankratio", "rsd", "icc"),
             ...) {
        idx_to_keep <- .removeFeatures(x, method = method, ...)
        x[idx_to_keep, ]
    }
)

##' @rdname removeFeatures
setMethod(
    "removeFeatures", "SummarizedExperiment",
    function(x, method = c("missing", "blankratio", "rsd", "icc"),
             i, ...) {
        idx_to_keep <- .removeFeatures(assay(x, i), method = method, ...)
        x[idx_to_keep, ]
    }
)

.removeFeatures <- function(x,
                            method = c("missing", "blankratio", "rsd", "icc"),
                            ...) {
    method <- match.arg(method)
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    switch(
        method,
        missing = .removeMiss(x, ...),
        blankratio = .removeBlankRatio(x, ...),
        rsd = .removeRSD(x, ...),
        icc = .removeICC(x, ...)
    )
}
