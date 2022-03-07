##' Dimension reduction methods
##'
##' Performs dimensionality reduction on a matrix-like object or
##' \linkS4class{SummarizedExperiment} object.
##'
##' Currently, principal component analysis (PCA), t-distributed stochastic
##' neighbor embedding (t-SNE), and partial least squares-discriminant analysis
##' (PLS-DA) are supported. For the method argument,
##'
##' `pca` performs PCA using singular value decomposition. If there is any
##' missing value, the non-linear iterative partial least squares (NIPALS)
##' algorithm is used instead using the [pcaMethods::nipalsPca]. See [reducePCA]
##' for details.
##'
##' `tsne` performs t-SNE using the [Rtsne::Rtsne]. See [reduceTSNE] for
##' details.
##' 
##' `plsda` performs PLS-DA using a standard PLS model for classification with
##' the [pls::plsr]. See [reducePLSDA] for details.
##' 
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param method A string specifying which dimension-reduction method to use.
##' @param xin Character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param y A factor vector for the information about each sample's group.
##' @param ncomp A integer specifying the number of components extract.
##' @param ... Arguments passed to a specific dimension-reduction method.
##' @return A matrix containing custom attributes related to the
##'   dimension-reduction method used.
##'
##' @references
##' 
##' Wold, H. (1966). Estimation of principal components and related models by
##' iterative least squares. In P. R. Krishnajah (Ed.), Multivariate analysis
##' (pp. 391-420). NewYork: Academic Press.
##'
##' Stacklies, W., Redestig, H., Scholz, M., Walther, D. and Selbig, J.
##' pcaMethods -- a Bioconductor package providing PCA methods for incomplete
##' data. Bioinformatics, 2007, 23, 1164-1167
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
##' Kristian Hovde Liland, Bjørn-Helge Mevik and Ron Wehrens (2021). pls:
##' Partial Least Squares and Principal Component Regression. R package version
##' 2.8-0. https://CRAN.R-project.org/package=pls
##'
##' @seealso See [reducePCA], [reduceTSNE], and [reducePLSDA] for the underlying
##'   functions that do the work for the underlying functions that do the work.
##'
##' @name reduceIntensity
##' 
##' @examples
##'
##' data(faahko_poplin)
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
NULL

##' @rdname reduceIntensity
setMethod(
  "reduceIntensity", "ANY",
  function(x, method = c("pca", "tsne", "plsda"), y, ncomp = 2, ...) {
    .reduceIntensity(x, method = method, y = y, ncomp = ncomp, ...)
  }
)

##' @rdname reduceIntensity 
setMethod(
  "reduceIntensity", "SummarizedExperiment",
  function(x, method = c("pca", "tsne", "plsda"), y, ncomp = 2,
           xin, xout, ...) {
    if (missing(xout)) {
      .reduceIntensity(assay(x, xin), method = method, ...)
    } else {
      assay(x, xout) <- .reduceIntensity(assay(x, xin), method = method, ...)
      x
    }
  }
)

.reduceIntensity <- function(x, method = c("pca", "tsne", "plsda"), y,
                            ncomp = 2, ...) {
  method <- match.arg(method)
  if (length(ncomp) != 1 || ncomp < 1) {
    stop("'ncomp' must be a positive integer of length 1.")
  }
  switch(
    method,
    pca = reducePCA(x, ncomp = ncomp, ...),
    tsne = reduceTSNE(x, ncomp = ncomp, ...),
    plsda = reducePLSDA(x, y = y, ncomp = ncomp, ...)
  )
}
