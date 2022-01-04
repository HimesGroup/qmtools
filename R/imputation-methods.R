##' Imputation methods
##'
##' Missing values are frequently found in metabolomics data. The \pkg{poplin}
##' package provides a few options to handle them.
##' [poplin_impute] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{impute_randomforest}}:}{
##' random forest imputation
##' }
##' \item{\code{\link{impute_knn}}:}{
##' k-nearest neighbor (KNN) imputation
##' }
##' \item{\code{\link{impute_pca}}:}{
##' principal component analysis (PCA) imputation
##' }
##' \item{\code{\link{impute_univariate}}:}{
##' univariate imputation (e.g., half-minimum, mean, median)
##' }
##' }
##' @param x A matrix or \linkS4class{poplin} object.
##' @param method The imputation method to be used, defaulting to "knn".
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout Character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ... Arguments passed to a specific imputation method.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name poplin_impute
##' @aliases
##' poplin_impute
##' poplin_impute,matrix-method
##' poplin_impute,poplin-method
##' @family imputation methods
##' @examples
##'
##' data(faahko_poplin)
##'
##' if (requireNamespace("VIM", quietly = TRUE)) {
##'  ## poplin object
##'  poplin_impute(faahko_poplin, method = "knn", xin = "raw", xout = "knn")
##' 
##'  ## matrix
##'  m <- poplin_raw(faahko_poplin, "raw")
##'  poplin_impute(m, method = "knn")
##' }
setMethod(
  "poplin_impute",
  "matrix",
  function(x, method = c("knn", "randomforest", "pca", "univariate"), ...) {
    .poplin_impute(x, method = method, ...)
  }
)

##' @rdname poplin_impute
setMethod(
  "poplin_impute",
  "poplin",
  function(x, method = c("knn", "randomforest", "pca", "univariate"),
           xin, xout, ...) {
    m <- .verify_and_extract_input(x, xin)
    poplin_data(x, xout) <- .poplin_impute(m, method = method, ...)
    x
  }
)

##' Random forest imputation
##'
##' Apply random forest imputation to a matrix or \linkS4class{poplin} object.
##' This is an interface to the \link[missForest]{missForest} function from the
##' \pkg{missForest} package. Since the random forest is a tree-based method, it
##' is invariant to monotonic transformations.
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
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout Character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ... Additional arguments passed to \link[missForest]{missForest}.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_randomforest
##' @aliases
##' impute_randomforest
##' impute_randomforest,matrix-method
##' impute_randomforest,poplin-method
##' @family imputation methods
##' @examples
##'
##' data(faahko_poplin)
##'
##' if (requireNamespace("missForest", quietly = TRUE)) {
##'   ## poplin object
##'   impute_randomforest(faahko_poplin, xin = "raw", xout = "rf")
##' 
##'   ## matrix
##'   m <- poplin_raw(faahko_poplin, xin = "raw")
##'   impute_randomforest(m)
##' }
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
  function(x, xin, xout, ...) {
    .poplin_extract_and_assign(x, .impute_randomforest,
                               xin, xout, ...)
  }
)

##' K-nearest neighbor (KNN) imputation
##'
##' Apply k-nearest neighbor (KNN) imputation to a matrix or
##' \linkS4class{poplin} object. This is an interface to the \link[VIM]{kNN}
##' function from the \pkg{VIM} package. Since it is based on Gower's distance,
##' Standardization of input data before imputation would not affect the result.
##'
##' @references
##' Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM.
##' Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07
##'
##' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its
##' Properties. Biometrics, 27(4), 857–871. https://doi.org/10.2307/2528823
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout Character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param by Character controlling whether the imputation is performed by
##'   k-nearest features or by k-nearest samples. Either "feature" or "sample".
##' @param ... Additional arguments passed to \link[VIM]{kNN}.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_knn
##' @aliases
##' impute_knn
##' impute_knn,matrix-method
##' impute_knn,poplin-method
##' @family imputation methods
##' @examples
##'
##' data(faahko_poplin)
##' 
##' if (requireNamespace("VIM", quietly = TRUE)) {
##'   ## poplin object
##'   impute_knn(faahko_poplin, xin = "raw", xout = "knn")
##' 
##'   ## matrix
##'   m <- poplin_raw(faahko_poplin, "raw")
##'   impute_knn(m)
##' }
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
  function(x, xin, xout, by = c("feature", "sample"), ...) {
    .poplin_extract_and_assign(x, .impute_knn,
                               xin, xout,
                               by = by, ...)
  }
)

##' Principal component analysis (PCA) imputation
##'
##' Apply PCA imputation to a matrix or \linkS4class{poplin} object. This is a
##' interface to the \link[pcaMethods]{pca} function from the \pkg{pcaMethods}
##' package. Here, features are interpreted as variables and samples as
##' observations. Pre-processing of input (centering, scaling) may be necessary.
##' See the documentation of \link[pcaMethods]{pca}. Note that the PCA
##' imputation could yield negative feature values that need to be
##' post-processed.
##'
##' @references
##' Stacklies, W., Redestig, H., Scholz, M., Walther, D. and Selbig, J.
##' pcaMethods -- a Bioconductor package providing PCA methods for incomplete
##' data. Bioinformatics, 2007, 23, 1164-1167
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout Character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param type The method to perform PCA.
##' @param ... Additional arguments passed to \link[pcaMethods]{pca}.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_pca
##' @aliases
##' impute_pca
##' impute_pca,matrix-method
##' impute_pca,poplin-method
##' @family imputation methods
##' @examples
##'
##' data(faahko_poplin)
##'
##' if (requireNamespace("pcaMethods", quietly = TRUE)) {
##'   ## poplin object
##'   impute_pca(faahko_poplin, xin = "raw", xout = "ppca", type = "ppca",
##'              center = TRUE, scale = "uv")
##'
##'   ## matrix
##'   m <- poplin_raw(faahko_poplin, "raw")
##'   impute_pca(m, type = "ppca", center = TRUE, scale = "uv")
##' }
setMethod(
  "impute_pca",
  "matrix",
  function(x, type = c("nipals", "bpca", "ppca", "svdImpute"), ...) {
    .impute_pca(x, type = type, ...)
  }
)

##' @rdname impute_pca
setMethod(
  "impute_pca",
  "poplin",
  function(x, xin, xout,
           type = c("nipals", "bpca", "ppca", "svdImpute"), ...) {
    .poplin_extract_and_assign(x, .impute_pca,
                               xin, xout,
                               type = type, ...)
  }
)

##' Univariate imputation
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
##' 
##' @references
##' Wei, R., Wang, J., Su, M. et al. Missing Value Imputation Approach for Mass
##' Spectrometry-based Metabolomics Data. Sci Rep 8, 663 (2018).
##' https://doi.org/10.1038/s41598-017-19120-0
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout Character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param type Character specifying the method for univariate imputation.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the imputed intensities.
##' @name impute_univariate
##' @aliases
##' impute_univariate
##' impute_univariate,matrix-method
##' impute_univariate,poplin-method
##' @family imputation methods
##' @examples
##'
##' data(faahko_poplin)
##' 
##' ## poplin object
##' impute_univariate(faahko_poplin, xin = "raw", xout = "halfmin",
##'                   type = "halfmin")
##' ## matrix
##' m <- poplin_raw(faahko_poplin, "raw")
##' impute_univariate(m, type = "median")
setMethod(
  "impute_univariate",
  "matrix",
  function(x, type = c("halfmin", "median", "mean")) {
    .impute_univariate(x, type = type)
  }
)

##' @rdname impute_univariate
setMethod(
  "impute_univariate",
  "poplin",
  function(x, xin, xout, type = c("halfmin", "median", "mean")) {
    .poplin_extract_and_assign(x, .impute_univariate,
                               xin, xout, type = type)
  }
)
