##' Imputation methods
##'
##' Performs a variety of data imputation methods on a matrix-like object or
##' \linkS4class{SummarizedExperiment} object. The methods include k-Nearest
##' Neighbors (kNN), Random Forest (RF), and many others from the
##' [MsCoreUtils::impute_matrix]. See the details below.
##'
##' The method argument can be one of "knn", "rf", "bpca", "QRILC", "MLE",
##' "MinDet", "MinProb", "min", "zero", "mixed", "nbavg", "with", "none". Please
##' choose one that best describes the nature of missing data. While this
##' function provides several simple imputation methods, they may only work
##' under restrictive assumptions.
##'
##' * "knn" performs kNN imputation based on the Gower distance or Euclidean
##' distance. See [imputeKNN] for details.
##'
##' * "rf" performs random forest imputation using the [missForest::missForest],
##' as described in Stekhoven D. J., & Buehlmann, P. (2012). This method is not
##' sensitive to monotonic transformations of the intensity matrix.
##'
##' * For the other method arguments, please refer to the
##' [MsCoreUtils::impute_matrix]. Briefly,
##'
##'   - "bpca": Bayesian PCA missing value imputation.
##'   - "QRILC": Quantile regression approach for the imputation of
##'      left-censored missing data.
##'   - "MLE": Maximum likelihood-based imputation.
##'   - "MinDet": Deterministic minimal value approach for the imputation of
##'      left-censored data.
##'   - "MinProb": Stochastic minimal value approach for the imputation of
##'      left-censored data.
##'   - "min": Replace the missing values with the smallest non-missing value in
##'     the data.
##'   - "zero": Replace the missing values with 0.
##'   - "mixed": Mixed imputation applying two methods.
##'   - "nbavg": Average neighbour imputation for fractions collected along a
##'      fractionation/separation gradient.
##'   - "with": Replace the missing values with a user-provided value.
##'   - "none": Reserved for the "mixed" method.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param method A string specifying which imputation method to use.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param name A string specifying the name to be used to store the imputed
##'   intensities in \code{x} when \code{x} is a SummarizedExperiment object. If
##'   not specified, a matrix containing the imputed intensities is returned.
##' @param ... Arguments passed to a specific imputation method.
##' @return A matrix or \linkS4class{SummarizedExperiment} object of the same
##'   dimension as \code{x} containing the imputed intensities.
##'
##' @references
##'
##' Laurent Gatto, Johannes Rainer and Sebastian Gibb (2021).
##' MsCoreUtils: Core Utils for Mass Spectrometry Data. R package version
##' 1.4.0. https://github.com/RforMassSpectrometry/MsCoreUtils
##'
##' Stekhoven D. J., & Buehlmann, P. (2012). MissForest - non-parametric missing
##' value imputation for mixed-type data. Bioinformatics, 28(1), 112-118.
##'
##' @seealso See [imputeKNN], [missForest::missForest], and
##'   [MsCoreUtils::impute_matrix] for the underlying functions that do work.
##'
##' @name imputeIntensity
##'
##' @examples
##'
##' data(faahko_se)
##'
##' ## SummarizedExperiment object
##' se <- imputeIntensity(faahko_se, i = "raw", name = "imp1", method = "knn")
##' assayNames(se)
##'
##' ## Matrix
##' m <- assay(faahko_se, i = "raw")
##' imputeIntensity(m, method = "min")
##'
NULL

##' @rdname imputeIntensity
setMethod(
    "imputeIntensity", "ANY",
    function(x, method = c("knn", "rf", "bpca", "QRILC", "MLE",
                           "MinDet", "MinProb", "min", "zero",
                           "mixed", "nbavg", "with", "none"),
             ...) {
        .imputeIntensity(x, method = method, ...)
    }
)

##' @rdname imputeIntensity
setMethod(
    "imputeIntensity", "SummarizedExperiment",
    function(x, method = c("knn", "rf", "bpca", "QRILC", "MLE",
                           "MinDet", "MinProb", "min", "zero",
                           "mixed", "nbavg", "with", "none"),
             i, name, ...) {
        if (missing(name)) {
            .imputeIntensity(assay(x, i), method = method, ...)
        } else {
            assay(x, name) <- .imputeIntensity(assay(x, i), method = method,
                                               ...)
            x
        }
    }
)

.imputeIntensity <- function(x,
                             method = c("knn", "rf", "bpca", "QRILC", "MLE",
                                        "MinDet", "MinProb", "min", "zero",
                                        "mixed", "nbavg", "with", "none"),
                             ...) {
    method <- match.arg(method)
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    switch(
        method,
        knn = imputeKNN(x, ...),
        rf = {
            .verify_package("missForest")
            t(missForest::missForest(t(x), ...)$ximp)
        },
        bpca = {
            .verify_package("pcaMethods")
            MsCoreUtils::impute_bpca(x, ...)
        },
        QRILC = {
            .verify_package("imputeLCMD")
            MsCoreUtils::impute_matrix(x, method = "QRILC", ...)
        },
        MLE = {
            .verify_package("norm")
            MsCoreUtils::impute_mle(x, ...)
        },
        MinDet = {
            .verify_package("imputeLCMD")
            MsCoreUtils::impute_matrix(x, method = "MinDet", ...)
        },
        MinProb = {
            .verify_package("imputeLCMD")
            MsCoreUtils::impute_matrix(x, method = "MinProb", ...)
        },
        min = MsCoreUtils::impute_min(x, ...),
        zero = MsCoreUtils::impute_zero(x, ...),
        ## mixed = MsCoreUtils::impute_mixed(x, ...),
        mixed = .impute_mixed(x, ...),
        nbavg = MsCoreUtils::impute_neighbour_average(x, ...),
        with = MsCoreUtils::impute_with(x, ...),
        none = x
    )
}
