##' k-nearest neighbor imputation
##' 
##' Performs k-nearest neighbor (kNN) imputation on a matrix-like object where
##' rows represent features and columns represent samples. This function finds
##' k-nearest neighbors using either Gower distance or Euclidean distance.
##'
##' The kNN imputation based on Euclidean distance typically requires
##' standardization of input data to avoid variance-based weighting of variables
##' (make variables on similar scales). When Gower distance is used, the
##' imputation can be done with original units (would get the same result with
##' the standardized input on a different scale). The `type` "gower" utilizes
##' the [VIM::kNN] and "euclidean" uses the [impute::impute.knn].
##'
##' @param x A matrix-like object.
##' @param k An integer specifying the number of nearest neighbors to be used in
##'   imputation.
##' @param type A string specifying the distance metric to be used. Either
##'   "gower" or "euclidean".
##' @param by A string specifying whether the imputation is performed by
##'   k-nearest features or by k-nearest samples. Either "feature" or "sample".
##' @param scale A logical specifying whether `x` needs to be standardized prior
##'   to the imputation when Euclidean distance is used. The imputed values are
##'   re-transformed so that they are on the original scales.
##' @param ... Arguments passed to [VIM::kNN] (Gower distance) or
##'   [impute::impute.knn] (Euclidean distance).
##' @return A matrix of the same dimension as \code{x} containing the imputed
##'   intensities.
##' 
##' @references
##'
##' Trevor Hastie, Robert Tibshirani, Balasubramanian Narasimhan and Gilbert Chu
##' (2021). impute: impute: Imputation for microarray data. R package version
##' 1.66.0.
##'
##' Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM.
##' Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07
##'
##' @seealso
##'
##' See [imputeIntensity] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' See [VIM::kNN] and [missForest::missForest] for the underlying functions
##' that do work.
##'
##' @examples
##'
##' m <- assay(faahko_se, "raw")
##' imputeKNN(m)
##'
##' @export
imputeKNN <- function(x, k = 10, type = c("gower", "euclidean"),
                      by = c("feature", "sample"), scale = FALSE, ...) {
    type <- match.arg(type)
    by <- match.arg(by)
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (type == "gower") {
        .impute_knn_gower(x, k = k, by = by, ...)
    } else {
        .impute_knn_euclidean(x, k = k, by = by, scale = scale, ...)
    }
}

.impute_knn_gower <- function(x, k, by = c("feature", "sample"), ...) {
    if (by == "feature") {
        out <- VIM::kNN(x, k = k, ...)[, seq_len(ncol(x))]
        ## VIM package internally converts x as data.table, which drops rownames
        rownames(out) <- rownames(x)
    } else {
        out <- t(VIM::kNN(t(x), k = k, ...))[seq_len(nrow(x)), ]
        colnames(out) <- colnames(x)
    }
    as.matrix(out)
}

.impute_knn_euclidean <- function(x, k, by = c("feature", "sample"),
                                  scale = FALSE, ...) {
    ## Min-Max scaling later?
    .verify_package("impute")
    if (by == "feature") {
        if (scale) {
            xs <- scale(x)
            res <- MsCoreUtils::impute_knn(xs, k = k, ...)
            .scale_recover(res, attr(xs, "scaled:center"),
                           attr(xs, "scaled:scale"))
        } else{
            MsCoreUtils::impute_knn(x, k = k, ...)
        }
    } else {
        if (scale) {
            xs <- scale(t(x))
            res <- MsCoreUtils::impute_knn(xs, k = k, ...)
            t(.scale_recover(res, attr(xs, "scaled:center"),
                             attr(xs, "scaled:scale")))
        } else{
            t(MsCoreUtils::impute_knn(t(x), k = k, ...))
        }
    }
}

.scale_recover <- function(x, centers, scales) {
    res <- sweep(x, 2, scales, "*")
    sweep(res, 2, centers, "+")
}

################################################################################
## imputation_mixed
################################################################################
## Slight modification in MSCoreUtils::imputed_mixed to accommodate new methods.
.impute_mixed <- function(x, randna, mar, mnar, ...) {
    if (missing(randna))
        stop("Mixed imputation requires 'randna' argument. See ?impute_mixed.",
             call. = FALSE)
    stopifnot(is.logical(randna))
    if (missing(mar))
        stop("Mixed imputation requires 'mar' argument. See ?impute_mixed.",
             call. = FALSE)
    if (missing(mnar))
        stop("Mixed imputation requires 'mnar' argument. See ?impute_mixed.",
             call. = FALSE)
    if (length(randna) != nrow(x))
        stop("`nrow(x)`and length of randna must be equal.",
             call. = FALSE)
    x[randna, ] <- .imputeIntensity(x[randna, ], method = mar, ...)
    x[!randna, ] <- .imputeIntensity(x[!randna, ], method = mnar, ...)
    x
}

