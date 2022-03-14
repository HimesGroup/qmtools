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
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
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
##' @name reduceFeatures
##'
##' @examples
##'
##' data(faahko_se)
##'
##' ## SummarizedExperiment object
##' res_pca <- reduceFeatures(faahko_se, i = "knn_vsn", method = "pca")
##' summary(res_pca)
##'
##' ## Matrix
##' y <- factor(colData(faahko_se)$sample_group)
##' m <- assay(faahko_se, i = "knn_vsn")
##' res_plsda <- reduceFeatures(m, method = "plsda", y = y, ncomp = 3)
##' summary(res_plsda)
##'
NULL

##' @rdname reduceFeatures
setMethod(
    "reduceFeatures", "ANY",
    function(x, method = c("pca", "tsne", "plsda"), ncomp = 2, y, ...) {
        .reduceFeatures(x, method = method, ncomp = ncomp, y, ...)
    }
)

##' @rdname reduceFeatures
setMethod(
    "reduceFeatures", "SummarizedExperiment",
    function(x, method = c("pca", "tsne", "plsda"), ncomp = 2,
             i, y, ...) {
        .reduceFeatures(assay(x, i), method = method, ncomp = ncomp, y, ...)
    }
)

.reduceFeatures <- function(x, method = c("pca", "tsne", "plsda"),
                            ncomp = 2, y, ...) {
    method <- match.arg(method)
    if (length(ncomp) != 1 || ncomp < 1) {
        stop("'ncomp' must be a positive integer of length 1.")
    }
    switch(
        method,
        pca = reducePCA(x, ncomp = ncomp, ...),
        tsne = reduceTSNE(x, ncomp = ncomp, ...),
        plsda = reducePLSDA(x, ncomp = ncomp, y = y, ...)
    )
}
