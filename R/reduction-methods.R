##' Dimension reduction methods
##'
##' In metabolomics, dimension reduction methods are often used for modeling
##' and visualization.
##' [poplin_reduce] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{reduce_pca}}:}{
##' principal component analysis (PCA)
##' }
##' \item{\code{\link{reduce_plsda}}:}{
##' partial least squares-discriminant analysis (PLS-DA)
##' }
##' \item{\code{\link{reduce_tsne}}:}{
##' t-distributed stochastic neighbor embedding
##' }
##' }
##' @param x a matrix or \linkS4class{poplin} object.
##' @param method the dimension reduction method to be used, defaulting to "pca".
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param y a factor vector for discrete outcome required for PLS-DA. Ignored
##'   otherwise.
##' @param ncomp output dimensionality.
##' @param ... argument passed to a specific dimension reduction method.
##' @return a matrix or \linkS4class{poplin} object with the same number of
##'   rows as \code{ncol(x)} containing the dimension reduction result.
##' @name poplin_reduce
##' @aliases
##' poplin_reduce
##' poplin_reduce,matrix-method
##' poplin_reduce,poplin-method
##' @family data reduction methods
##' @examples
##' 
##' ## poplin object
##' out <- poplin_reduce(faahko_poplin, method = "pca",
##'                      xin = "knn_cyclic", xout = "pca")
##' summary(poplin_reduced(out, "pca"))
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn_cyclic")
##' poplin_reduce(m, method = "pca")
##' summary(out)
setMethod(
  "poplin_reduce",
  "matrix",
  function(x, method = c("pca", "tsne", "plsda"), y, ncomp = 2, ...) {
    .poplin_reduce(x, method = method, y = y, ncomp = ncomp, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce",
  "poplin",
  function(x, method = c("pca", "tsne", "plsda"), xin, xout,
           y, ncomp = 2, ...) {
    m <- .verify_and_extract_input(x, xin)
    poplin_reduced(x, xout) <- .poplin_reduce(m, method = method, y = y,
                                                    ncomp = ncomp, ...)
    x
  }
)

##' Principal component analysis (PCA)
##'
##' Apply PCA to a matrix or \linkS4class{poplin} object. For the data without
##' missing values, PCA is performed via singular value decomposition.
##' Otherwise, PCA is performed using the non-linear iterative partial least
##' squares (NIPALS) algorithm via the \link[pcaMethods]{nipalsPca} function
##' from the \pkg{pcaMethods} package. NIPALS PCA can handle a small amount of
##' missing values.
##'
##' @references
##'
##' Wold, H. (1966). Estimation of principal components and related models by
##' iterative least squares. In P. R. Krishnajah (Ed.), Multivariate analysis
##' (pp. 391-420). NewYork: Academic Press.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ncomp output dimensionality.
##' @param center logical indicating mean-centering prior to PCA.
##' @param scale logical indicating unit variance scaling prior to PCA.
##' @param ... additional arguments passed to \link[pcaMethods]{nipalsPca}.
##' @return a poplin.pca or \linkS4class{poplin} object with the same number of
##'   rows as \code{ncol(x)} containing the dimension reduction result.
##'   poplin.pca is a matrix containing custom attributes used to summarize and
##'   visualize the PCA result.
##' @name reduce_pca
##' @aliases
##' reduce_pca
##' reduce_pca,matrix-method
##' reduce_pca,poplin-method
##' @family data reduction methods
##' @examples
##' 
##' ## poplin object
##' out <- reduce_pca(faahko_poplin, xin = "knn_cyclic", xout = "pca")
##' summary(poplin_reduced(out, "pca"))
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn_cyclic")
##' out <- reduce_pca(m, ncomp = 3)
##' summary(out)
setMethod(
  "reduce_pca",
  "matrix",
  function(x, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduce_pca(x, ncomp = ncomp, center = center, scale = scale, ...)
  }
)

##' @rdname reduce_pca
setMethod(
  "reduce_pca",
  "poplin",
  function(x, xin, xout, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduced_extract_and_assign(x, .reduce_pca,
                                xin, xout, ncomp = ncomp,
                                center = center, scale = scale, ...)
  }
)

##' t-distributed stochastic neighbor embedding (t-SNE)
##'
##' Apply t-SNE to a matrix or \linkS4class{poplin} object. This is an interface
##' to the \link[Rtsne]{Rtsne} function from the \pkg{Rtsne} package. t-SNE is
##' well-suited for visualizing high-dimensional data by giving each data point
##' a location in a two or three-dimensional map.
##'
##' @references
##'
##' L.J.P. van der Maaten and G.E. Hinton. Visualizing High-Dimensional Data
##' Using t-SNE. Journal of Machine Learning Research 9(Nov):2579-2605, 2008.
##'
##' L.J.P. van der Maaten. Accelerating t-SNE using Tree-Based Algorithms.
##' Journal of Machine Learning Research 15(Oct):3221-3245, 2014.
##'
##' Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding
##' using a Barnes-Hut Implementation, URL: https://github.com/jkrijthe/Rtsne
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ncomp output dimensionality.
##' @param normalize logical controlling whether the input matrix is
##'   mean-centered and scaled so that the largest absolute of the centered
##'   matrix is equal to unity. See \link[Rtsne]{normalize_input} for details.
##' @param ... additional arguments passed to \link[Rtsne]{Rtsne}.
##' @return a poplin.tsne matrix or \linkS4class{poplin} object with the same
##'   number of rows as \code{ncol(x)} containing the dimension reduction
##'   result.
##' @return a poplin.tsne or \linkS4class{poplin} object with the same number of
##'   rows as \code{ncol(x)} containing the dimension reduction result.
##'   poplin.tsne is a matrix containing custom attributes used to summarize and
##'   visualize the t-SNE result.
##' @name reduce_tsne
##' @aliases
##' reduce_tsne
##' reduce_tsne,matrix-method
##' reduce_tsne,poplin-method
##' @family data reduction methods
##' @examples
##' 
##' if (requireNamespace("Rtsne", quietly = TRUE)) {
##'   ## poplin object
##'   out <- reduce_tsne(faahko_poplin, xin = "knn_cyclic", xout = "tsne",
##'                      normalize = TRUE, perplexity = 3)
##'   summary(poplin_reduced(out, "tsne"))
##' 
##'   ## matrix
##'   m <- poplin_data(faahko_poplin, "knn_cyclic")
##'   out <- reduce_tsne(m, normalize = TRUE, perplexity = 3, ncomp = 3)
##'   summary(out)
##' }
setMethod(
  "reduce_tsne",
  "matrix",
  function(x, ncomp = 2, normalize = TRUE, ...) {
    .reduce_tsne(x, ncomp = ncomp, normalize = normalize, ...)
  }
)

##' @rdname reduce_tsne
setMethod(
  "reduce_tsne",
  "poplin",
  function(x, xin, xout, ncomp = 2, normalize = TRUE, ...) {
    .reduced_extract_and_assign(x, .reduce_tsne,
                                xin, xout,
                                ncomp = ncomp, normalize = normalize, ...)
  }
)

##' Partial least squares-discriminant analysis (PLS-DA)
##'
##' Apply PLS-DA to a matrix or \linkS4class{poplin} object. It performs
##' standard PLS for classification using the \link[pls]{plsr} function from the
##' \pkg{pls} package.
##'
##' @references
##'  Kristian Hovde Liland, Bjørn-Helge Mevik and Ron Wehrens (2021). pls:
##'  Partial Least Squares and Principal Component Regression. R package version
##'  2.8-0. https://CRAN.R-project.org/package=pls
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param y a factor vector for discrete outcome.
##' @param ncomp output dimensionality.
##' @param center logical indicating mean-centering prior to PLS-DA.
##' @param scale logical indicating unit variance scaling prior to PLS-DA.
##' @param ... additional arguments passed to \link[pls]{plsr}.
##' @return a poplin.plsda or \linkS4class{poplin} object with the same number of
##'   rows as \code{ncol(x)} containing the dimension reduction result.
##'   poplin.plsda is a matrix containing custom attributes used to summarize and
##'   visualize the PLS-DA result.
##' @name reduce_plsda
##' @aliases
##' reduce_plsda
##' reduce_plsda,matrix-method
##' reduce_plsda,poplin-method
##' @family data reduction methods
##' @examples
##' 
##' if (requireNamespace("pls", quietly = TRUE)) {
##'   ## response vector
##'   y <- factor(colData(faahko_poplin)$sample_group, levels = c("WT", "KO"))
##'
##'   ## poplin object
##'   out <- reduce_plsda(faahko_poplin, xin = "knn_cyclic", xout = "plsda", y = y)
##'   summary(poplin_reduced(out, "plsda"))
##'
##'   ## matrix
##'   m <- poplin_data(faahko_poplin, "knn_cyclic")
##'   out <- reduce_plsda(m, y = y, ncomp = 3)
##'   summary(out)
##' }
setMethod(
  "reduce_plsda",
  "matrix",
  function(x, y, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduce_plsda(x, y = y, ncomp = ncomp,
                  center = center, scale = scale, ...)
  }
)

##' @rdname reduce_plsda
setMethod(
  "reduce_plsda",
  "poplin",
  function(x, xin, xout, y,
           ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduced_extract_and_assign(x, .reduce_plsda,
                                xin, xout,
                                y = y, ncomp = ncomp,
                                center = center, scale = scale, ...)
  }
)
