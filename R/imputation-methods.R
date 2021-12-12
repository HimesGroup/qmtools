##' Imputation methods
##'
##' Missing values are frequently found in metabolomics data. The \pkg{poplin}
##' package provides a few options to handle them.
##' [poplin_impute] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{impute_knn}}:}{
##' k-nearest neighbor (KNN) imputation
##' }
##' \item{\code{\link{impute_pca}}:}{
##' principal component analysis (PCA) imputation
##' }
##' \item{\code{\link{impute_randomforest}}:}{
##' random forest imputation
##' }
##' \item{\code{\link{impute_simple}}:}{
##' simple univariate imputation (e.g., half-minimum, mean, median)
##' }
##' }
##' @param x A matrix or \linkS4class{poplin} object.
##' @param method A imputation method. Default is 'pqn'.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ... Argument passed to a specific imputation method.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name poplin_impute
##' @family imputation methods
setMethod(
  "poplin_impute",
  "matrix",
  function(x, method = c("knn", "pca", "randomforest", "simple"), ...) {
    .poplin_impute(x, method = method, ...)
  }
)

##' @rdname poplin_impute
setMethod(
  "poplin_impute",
  "poplin",
  function(x, method = c("knn", "pca", "randomforest", "simple"),
           poplin_in, poplin_out, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_data(x, poplin_out) <- .poplin_impute(m, method = method, ...)
    x
  }
)

##' K-nearest neighbor (KNN) imputation
##'
##' Apply k-nearest neighbor (KNN) imputation to a matrix or
##' \linkS4class{poplin} object. This is an interface to the [VIM::kNN] from the
##' \pkg{VIM} package. Since it is based on Gower's distance, standardization of
##' input data prior to KNN imputation would not affect the result.
##'
##' @references
##' Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM.
##' Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07
##'
##' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its
##' Properties. Biometrics, 27(4), 857–871. https://doi.org/10.2307/2528823
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param by Imputation by k-nearest features or by k-nearest samples.
##' @param ... Additional argument passed to [VIM::kNN].
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_knn
##' @family imputation methods
setMethod(
  "impute_knn",
  "matrix",
  function(x, by = c("feature", "sample"),  ...) {
    .impute_knn(x, by = by, ...)
  }
)

##' @rdname impute_knn
setMethod(
  "impute_knn",
  "poplin",
  function(x, poplin_in, poplin_out, by = c("feature", "sample"), ...) {
    .poplin_extract_and_assign(x, .impute_knn,
                               poplin_in, poplin_out,
                               by = by, ...)
  }
)

##' Random forest imputation
##'
##' Apply random forest imputation to a matrix or \linkS4class{poplin} object.
##' This is an interface to the [missForest::missForest] from the
##' \pkg{missForest} package. Since random forest is a tree-based method, it can
##' be performed with raw intensities - invariant to monotonic transformations.
##'
##' @references
##'
##' Daniel J. Stekhoven (2013). missForest: Nonparametric Missing Value
##' Imputation using Random Forest. R package version 1.4.
##'
##' Stekhoven D. J., & Buehlmann, P. (2012). MissForest - non-parametric missing
##' value imputation for mixed-type data. Bioinformatics, 28(1), 112-118.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ... Additional argument passed to [missForest::missForest].
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_randomforest
##' @family imputation methods
setMethod(
  "impute_randomforest",
  "matrix",
  function(x, ...) {
    .impute_randomforest(x, ...)
  }
)

##' @rdname impute_randomforest
setMethod(
  "impute_randomforest",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .impute_randomforest,
                               poplin_in, poplin_out, ...)
  }
)

##' Principal component analysis (PCA) imputation
##'
##' Apply PCA imputation to a matrix or \linkS4class{poplin} object. This is a
##' interface to the [pcaMethods::pca] from the \pkg{pcaMethods} package. Here,
##' features are interpreted as variables and samples as observations.
##' Pre-processing of input (centering, scaling) may be necessary. See the
##' documentation of [pcaMethods::pca] and [pcaMethods::prep]. Note that the PCA
##' imputation could yield negative feature values that need to be
##' post-processed.
##'
##' @references
##' Stacklies, W., Redestig, H., Scholz, M., Walther, D. and Selbig, J.
##' pcaMethods -- a Bioconductor package providing PCA methods for incomplete
##' data. Bioinformatics, 2007, 23, 1164-1167
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param type A method for performing PCA.
##' @param ... Additional argument passed to [pcaMethods::pca].
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_pca
##' @family imputation methods
setMethod(
  "impute_pca",
  "matrix",
  function(x, type = c("bpca", "ppca", "nipals", "svdImpute"), ...) {
    .impute_pca(x, type = type, ...)
  }
)

##' @rdname impute_pca
setMethod(
  "impute_pca",
  "poplin",
  function(x, poplin_in, poplin_out,
           type = c("bpca", "ppca", "nipals", "svdImpute"), ...) {
    .poplin_extract_and_assign(x, .impute_pca,
                               poplin_in, poplin_out,
                               type = type, ...)
  }
)

##' Simple univariate imputation
##'
##' Apply univariate imputation to a matrix or \linkS4class{poplin} object. The
##' supported methods include
##' \itemize{
##' \item Half-minimum imputation: for each feature, missing values are replaced
##' with half the observed minimum.
##' \item Median imputation: for each feature, missing values are replaced with
##' the median of non-missing values.
##' \item Mean imputation: for each feature, missing values are replaced with
##' the mean of non-missing values.
##' }
##' @references
##' Wei, R., Wang, J., Su, M. et al. Missing Value Imputation Approach for Mass
##' Spectrometry-based Metabolomics Data. Sci Rep 8, 663 (2018).
##' https://doi.org/10.1038/s41598-017-19120-0
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param type A method for doing univariate imputation.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_simple
##' @family imputation methods
setMethod(
  "impute_simple",
  "matrix",
  function(x, type = c("halfmin", "median", "mean")) {
    .impute_simple(x, type = type)
  }
)

##' @rdname impute_simple
setMethod(
  "impute_simple",
  "poplin",
  function(x, poplin_in, poplin_out, type = c("halfmin", "median", "mean")) {
    .poplin_extract_and_assign(x, .impute_simple,
                               poplin_in, poplin_out, type = type)
  }
)

setMethod(
  "impute_halfmin",
  "matrix",
  function(x) {
    .impute_halfmin(x)
  }
)

setMethod(
  "impute_halfmin",
  "poplin",
  function(x, poplin_in, poplin_out) {
    .poplin_extract_and_assign(x, .impute_halfmin,
                               poplin_in, poplin_out)
  }
)

setMethod(
  "impute_median",
  "matrix",
  function(x, ...) {
    .impute_median(x, ...)
  }
)

setMethod(
  "impute_median",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .impute_median,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "impute_mean",
  "matrix",
  function(x, ...) {
    .impute_mean(x, ...)
  }
)

setMethod(
  "impute_mean",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_mean,
                               poplin_in, poplin_out, ...)
  }
)
