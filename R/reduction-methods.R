##' Dimension reduction methods
##'
##' In metabolomics, dimension reduction methods are often used for modeling
##' and visualization.
##' [poplin_reduce] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{poplin_reduce_pca}}:}{
##' principal component analysis (PCA)
##' }
##' \item{\code{\link{poplin_reduce_plsda}}:}{
##' partial least squares-discriminant analysis (PLS-DA)
##' }
##' \item{\code{\link{poplin_reduce_tsne}}:}{
##' t-distributed stochastic neighbor embedding
##' }
##' }
##' @param x A matrix or \linkS4class{poplin} object.
##' @param method A dimension reduction method. Default is 'pca'.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param y A factor vector for discrete outcome required for PLS-DA. Ignored
##'   otherwise.
##' @param ncomp Output dimensionality.
##' @param ... Argument passed to a specific dimension reduction method.
##' @return A matrix or \linkS4class{poplin} object with the same number of rows
##'   as \code{ncol(x)} containing the dimension reduction result.
##' @name poplin_reduce
##' @family data reduction methods
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
  function(x, method = c("pca", "tsne", "plsda"), poplin_in, poplin_out,
           y, ncomp = 2, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_reduced(x, poplin_out) <- .poplin_reduce(m, method = method, y = y,
                                                    ncomp = ncomp, ...)
    x
  }
)

##' Principal component analysis (PCA)
##'
##' Apply PCA to a matrix or \linkS4class{poplin} object. For the data without
##' missing values, PCA is performed via a singular value decomposition.
##' Otherwise, Bayesian PCA is performed using [pcaMethods::bpca] from the
##' \pkg{pcaMethods} package. Note that Bayesian PCA does not force
##' orthogonality between factor loadings.
##'
##' @references
##' Shigeyuki Oba, Masa-aki Sato, Ichiro Takemasa, Morito Monden, Ken-ichi
##' Matsubara, Shin Ishii, A Bayesian missing value estimation method for gene
##' expression profile data, Bioinformatics, Volume 19, Issue 16, 1 November
##' 2003, Pages 2088–2096, https://doi.org/10.1093/bioinformatics/btg287
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ncomp Output dimensionality.
##' @param center A logical indicating mean-centering prior to PCA.
##' @param scale A logical indicating unit variance scaling prior to PCA.
##' @param ... Additional arguments passed to [pcaMethods::bpca].
##' @return A poplin.pca matrix or \linkS4class{poplin} object with the same
##'   number of rows as \code{ncol(x)} containing the dimension reduction
##'   result.
##' @name poplin_reduce_pca
##' @family data reduction methods
setMethod(
  "poplin_reduce_pca",
  "matrix",
  function(x, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .poplin_reduce_pca(x, ncomp = ncomp, center = center, scale = scale, ...)
  }
)

##' @rdname poplin_reduce_pca
setMethod(
  "poplin_reduce_pca",
  "poplin",
  function(x, poplin_in, poplin_out, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_pca,
                                poplin_in, poplin_out,
                                ncomp = ncomp, center = center, scale = scale, ...)
  }
)

##' t-distributed stochastic neighbor embedding (t-SNE)
##'
##' Apply t-SNE to a matrix or \linkS4class{poplin} object. This is an interface
##' to the [Rtsne::Rtsne] from the \pkg{Rtsne} package. t-SNE is well-suited for
##' visualizing high-dimensional data by giving each data point a location in a
##' two or three-dimensional map.
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
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ncomp Number of components to calculate.
##' @param normalize if \code{TRUE}, an input matrix is mean-centered and scaled
##'   so that the largest absolute of the centered matrix is equal to unity. See
##'   [Rtsne::normalize_input] for details.
##' @param ... Additional argument passed to [Rtsne::Rtsne].
##' @return A poplin.tsne matrix or \linkS4class{poplin} object with the same
##'   number of rows as \code{ncol(x)} containing the dimension reduction
##'   result.
##' @name poplin_reduce_tsne
##' @family data reduction methods
setMethod(
  "poplin_reduce_tsne",
  "matrix",
  function(x, ncomp = 2, normalize = TRUE, ...) {
    .poplin_reduce_tsne(x, ncomp = ncomp, normalize = normalize, ...)
  }
)

##' @rdname poplin_reduce_tsne
setMethod(
  "poplin_reduce_tsne",
  "poplin",
  function(x, poplin_in, poplin_out, ncomp = 2, normalize = TRUE, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_tsne,
                                poplin_in, poplin_out,
                                ncomp = ncomp, normalize = normalize, ...)
  }
)

##' Partial least squares-discriminant analysis (PLS-DA)
##'
##' Apply PLS-DA to a matrix or \linkS4class{poplin} object. It performs
##' standard PLS for classification using [pls::plsr]. If the \pkg{pls} is not
##' installed, this function will stop with a note about install the package.
##'
##' @references
##'  Kristian Hovde Liland, Bjørn-Helge Mevik and Ron Wehrens (2021). pls:
##'  Partial Least Squares and Principal Component Regression. R package version
##'  2.8-0. https://CRAN.R-project.org/package=pls
##' 
##' @param x A matrix or \linkS4class{poplin} object.
##' @param method A dimension reduction method. Default is 'pca'.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param y A factor vector for discrete outcome.
##' @param ncomp Output dimensionality.
##' @param center A logical indicating mean-centering prior to PLS-DA.
##' @param scale A logical indicating unit variance scaling prior to PLS-DA.
##' @param ... Additional argument passed to [pls::plsr].
##' @return A poplin.plsda matrix or \linkS4class{poplin} object with the same
##'   number of rows as \code{ncol(x)} containing the dimension reduction
##'   result.
##' @name poplin_reduce_plsda
##' @family data reduction methods
setMethod(
  "poplin_reduce_plsda",
  "matrix",
  function(x, y, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .poplin_reduce_plsda(x, y = y, ncomp = ncomp,
                         center = center, scale = scale, ...)
  }
)

##' @rdname poplin_reduce_plsda
setMethod(
  "poplin_reduce_plsda",
  "poplin",
  function(x, poplin_in, poplin_out, y,
           ncomp = 2, center = TRUE, scale = FALSE, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_plsda,
                                poplin_in, poplin_out,
                                y = y, ncomp = ncomp,
                                center = center, scale = scale, ...)
  }
)
