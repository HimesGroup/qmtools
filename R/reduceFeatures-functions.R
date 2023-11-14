##' Principal component analysis (PCA)
##'
##' Performs PCA on a matrix-like object where rows represent features and
##' columns represents samples.
##'
##' For the data without missing values, PCA is performed with the transpose of
##' `x` via singular value decomposition. Otherwise, PCA is performed with the
##' transpose of `x` using the non-linear iterative partial least squares
##' (NIPALS) algorithm via the [pcaMethods::nipalsPca]. The function returns a
##' `reduced.pca` object that is a matrix with custom attributes to summarize
##' (via [summary]) and visualize (via [plotReduced]) the PCA result. The custom
##' attributes include the following:
##'
##' * `method`: The method used to reduce the dimension of data.
##' * `ncomp`: The number of components extracted.
##' * `R2`: A vector indicating the amount of variance explained by each
##' principal component.
##' * `R2cum`: A vector of cumulative R2.
##' * `loadings`: A matrix of variable loadings.
##' * `sdev`: A vector indicating the standard deviations of the principal
##' components.
##' * `centered`: A logical indicating whether the data was mean-centered
##' prior to PCA.
##' * `scaled`: A logical indicating whether the data was scaled prior to PCA.
##'
##' @param x A matrix-like object.
##' @param ncomp An integer specifying the number of components to extract.
##' @param center A logical specifying whether `x` needs to be mean-centered
##'   prior to PCA.
##' @param scale A logical specifying whether the unit variance scaling needs to
##'   be performed on `x` prior to PCA.
##' @param ... Additional arguments passed to [pcaMethods::nipalsPca]. Ignored
##'   if `x` has no missing values.
##' @return A reduced.pca object with the same number of rows as \code{ncol(x)}
##'   containing the dimension reduction result.
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
##' @seealso See [reduceFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' See [plotReduced] for visualization.
##'
##' See [pcaMethods::nipalsPca] for the underlying function that does the work.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' m <- assay(faahko_se, "knn_vsn")
##' res <- reducePCA(m, ncomp = 3)
##' summary(res)
##'
##' @export
reducePCA <- function(x, ncomp = 2, center = TRUE, scale = FALSE, ...) {
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (ncomp > min(dim(x))) {
        stop("'ncomp' must be <= min(dim(x))")
    }
    xt <- t(x) # transpose matrix;
    if (center || scale) {
        xt <- scale(xt, center = center, scale = scale)
    }
    if (any(is.infinite(x))) {
        stop("Infinite value(s) in 'x'.")
    }
    if (!anyNA(x)) {
        out <- .pca_svd(xt, ncomp = ncomp)
    } else {
        miss_pct <- 100 * sum(is.na(x)) / prod(dim(x))
        message("Missing value(s) in 'x'.\n")
        message(format(miss_pct, digits = 2), "% of values are missing. ")
        message("Please consider missing value imputation.\n")
        if (!requireNamespace("pcaMethods", quietly = TRUE)) {
            stop("Package 'pcaMethods' is required to perform ",
                 "PCA with missing values. ",
                 "Please install and try again or impute missing values.")
        } else {
            message("Performing NIPALS PCA...\n")
            out <- .pca_nipals(xt, ncomp = ncomp)
        }
    }
    attr(out, "centered") <- center
    attr(out, "scaled") <- scale
    class(out) <- c("reduced.pca", class(out))
    out
}

.pca_svd <- function(x, ncomp) {
    pc <- prcomp(x, center = FALSE, scale. = FALSE)
    imp <- summary(pc)$importance
    out <- pc$x[, seq_len(ncomp)]
    attr(out, "method") <- "PCA (SVD)"
    attr(out, "ncomp") <- ncomp
    attr(out, "R2") <- imp[2, seq_len(ncomp)]
    attr(out, "R2cum") <- imp[3, seq_len(ncomp)]
    attr(out, "loadings") <- pc$rotation[, seq_len(ncomp)]
    attr(out, "sdev") <- pc$sdev[seq_len(ncomp)]
    out
}

.pca_nipals <- function(x, ncomp, ...) {
    res <- pcaMethods::nipalsPca(Matrix = x, nPcs = ncomp, ...)
    out <- res@scores
    colnames(out) <- paste0("PC", seq_len(ncol(out)))
    rownames(out) <- rownames(x)
    colnames(res@loadings) <- paste0("PC", seq_len(ncol(out)))
    rownames(res@loadings) <- colnames(x)
    attr(out, "method") <- "PCA (NIPALS)"
    attr(out, "ncomp") <- ncomp
    attr(out, "R2") <- c(res@R2cum[1], diff(res@R2cum))
    attr(out, "R2cum") <- res@R2cum
    attr(out, "loadings") <- res@loadings
    attr(out, "sdev") <- apply(res@scores, 2, sd)
    out
}


##' t-distributed stochastic neighbor embedding (t-SNE)
##'
##' Performs t-SNE on a matrix-like object where rows represent features and
##' columns represent samples.
##'
##' t-SNE is well-suited for visualizing high-dimensional data by giving each
##' data point a location in a two or three-dimensional map. This function
##' performs t-SNE with the transpose of `x` using [Rtsne::Rtsne] and returns a
##' `reduced.tsne` object that is a matrix with custom attributes to summarize
##' (via [summary]) and visualize (via [plotReduced]) the t-SNE result. The
##' custom attributes include the following:
##'
##' * `method`: The method used to reduce the dimension of data.
##' * `ncomp`: The number of components extracted.
##' * `perplexity`: The perplexity parameter used.
##' * `theta`: The speed/accuracy trade-off parameter used.
##' * `normalized`: A logical indicating whether the data was normalized prior
##' to t-SNE.
##'
##' @param x A matrix-like object.
##' @param ncomp A integer specifying the number of components to extract. Must
##'   be either 1, 2, or 3.
##' @param normalize A logical specifying whether the input matrix is
##'   mean-centered and scaled so that the largest absolute of the centered
##'   matrix is equal to unity. See [Rtsne::normalize_input] for details.
##' @param ... Additional arguments passed to [Rtsne::Rtsne].
##' @return A reduced.tsne object with the same number of rows as \code{ncol(x)}
##'   containing the dimension reduction result.
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
##' @seealso See [reduceFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' See [plotReduced] for visualization.
##'
##' See [Rtsne::Rtsne] for the underlying function that does the work.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' m <- assay(faahko_se, "knn_vsn")
##' res <- reduceTSNE(m, perplexity = 3)
##' summary(res)
##'
##' @export
reduceTSNE <- function(x, ncomp = 2, normalize = TRUE, ...) {
    .verify_package("Rtsne")
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (any(!is.finite(x))) {
        stop("infinite or missing values in 'x'.")
    }
    xt <- t(x) # transpose matrix;
    if (normalize) {
        xt <- Rtsne::normalize_input(xt)
    }
    res <- Rtsne::Rtsne(xt, dims = ncomp, ...)
    out <- res$Y
    colnames(out) <- paste0("tSNE", seq_len(ncol(out)))
    rownames(out) <- colnames(x)
    attr(out, "method") <- "t-SNE"
    attr(out, "ncomp") <- ncomp
    attr(out, "perplexity") <- res$perplexity
    attr(out, "theta") <- res$theta
    attr(out, "costs") <- res$cost
    attr(out, "itercosts") <- res$itercost
    attr(out, "stop_lying_iter") <- res$stop_lying_iter
    attr(out, "mom_switch_iter") <- res$mom_switch_iter
    attr(out, "momentum") <- res$momentum
    attr(out, "final_momentum") <- res$final_momentum
    attr(out, "eta") <- res$eta
    attr(out, "exaggeration_factor") <- res$exaggeration_factor
    attr(out, "normalized") <- normalize
    class(out) <- c("reduced.tsne", class(out))
    out
}

##' Partial least squares-discriminant analysis (PLS-DA)
##'
##' Performs PLS-DA on a matrix-like object where rows represent features and
##' columns represent samples.
##'
##' This function performs standard PLS for classification with the transpose of
##' `x` using the [pls::plsr]. Since PLS-DA is a supervised method, users must
##' supply the information about each sample's group. Here, `y` must be a factor
##' so that it can be internally converted to an indicator matrix. The function
##' returns a `reduced.plsda` object that is a matrix with custom attributes to
##' summarize (via [summary]) and visualize (via [plotReduced]) the PLS-DA
##' result. The custom attributes include the following:
##'
##' * `method`: The method used to reduce the dimension of data.
##' * `ncomp`: The number of components extracted.
##' * `explvar`: A vector indicating the amount of X variance explained by
##' each component.
##' * `responses`: A vector indicating the levels of factor `y`.
##' * `predictors`: A vector of predictor variables.
##' * `coefficient`: An array of regression coefficients.
##' * `loadings`: A matrix of loadings.
##' * `loadings.weights`: A matrix of loading weights.
##' * `Y.observed`: A vector of observed responses.
##' * `Y.predicted`: A vector of predicted responses.
##' * `Y.scores`: A matrix of Y-scores.
##' * `Y.loadings`: A matrix of Y-loadings.
##' * `projection`: The projection matrix.
##' * `fitted.values`: An array of fitted values.
##' * `residuals`: An array of regression residuals.
##' * `vip`: An array of VIP (Variable Importance in the Projection)
##' coefficients.
##' * `centered`: A logical indicating whether the data was mean-centered prior
##' to PLS-DA.
##' * `scaled`: A logical indicating whether the data was scaled prior to
##' PLS-DA.
##'
##'
##' @param x A matrix-like object.
##' @param y A factor vector for the information about each sample's group.
##' @param ncomp A integer specifying the number of components to extract.
##' @param center A logical specifying whether `x` and `y` matrices need to be
##'   mean-centered prior to PLS-DA.
##' @param scale A logical specifying whether the unit variance scaling needs to
##'   be performed on `x` prior to PLS-DA.
##' @param ... Additional arguments passed to [pls::plsr].
##' @return A reduced.plsda object with the same number of rows as
##'   \code{ncol(x)} containing the dimension reduction result.
##'
##' @references
##'
##' Kristian Hovde Liland, Bjørn-Helge Mevik and Ron Wehrens (2021). pls:
##' Partial Least Squares and Principal Component Regression. R package version
##' 2.8-0. https://CRAN.R-project.org/package=pls
##'
##' @seealso See [reduceFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' See [plotReduced] for visualization.
##'
##' See [pls::plsr] for the underlying function that does the work.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' m <- assay(faahko_se, "knn_vsn")
##' y <- factor(colData(faahko_se)$sample_group)
##' res <- reducePLSDA(m, y = y)
##' summary(res)
##
##' @export
reducePLSDA <- function(x, y, ncomp = 2, center = TRUE, scale = FALSE,
                        ...) {
    .verify_package("pls")
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    if (!is.factor(y)) {
        stop("'y' must be a factor.")
    }
    if (any(!is.finite(y))) {
        stop("infinite or missing values in 'y'.")
    }
    if (any(!is.finite(x))) {
        stop("infinite or missing values in 'x'.")
    }
    xt <- t(x)
    y_levels <- levels(y)
    y_dummy <- model.matrix(~ y - 1)
    colnames(y_dummy) <- gsub("^y", "", colnames(y_dummy))

    d <- data.frame(y = I(y_dummy), x = I(xt), row.names = colnames(x))
    fit <- pls::plsr(y ~ x, data = d, ncomp = ncomp,
                     center = center, scale = scale, ...)
    out <- pls::scores(fit)
    pred_vals <- predict(fit, ncomp = fit$ncomp)
    y_predicted <- colnames(pred_vals)[apply(pred_vals, 1, which.max)]

    attr(out, "method") <- paste0("PLS-DA (", fit$method, ")")
    attr(out, "responses") <- pls::respnames(fit)
    attr(out, "predictors") <- pls::prednames(fit)
    attr(out, "coefficients") <- fit$coefficients
    attr(out, "loadings") <- fit$loadings
    attr(out, "loading.weights") <- fit$loading.weights
    attr(out, "Y.observed") <- y
    attr(out, "Y.predicted") <- y_predicted
    attr(out, "Y.scores") <- fit$Yscores
    attr(out, "Y.loadings") <- fit$Yloadings
    attr(out, "projection") <- fit$projection
    attr(out, "fitted.values") <- fitted(fit)
    attr(out, "residuals") <- residuals(fit)
    attr(out, "ncomp") <- fit$ncomp
    attr(out, "vip") <- .vip_mat(fit)
    attr(out, "centered") <- center
    attr(out, "scaled") <- scale
    attr(out, "validation") <- fit$validation
    class(out) <- c("reduced.plsda", "matrix", class(out))
    out
}

.vip_mat <- function(fit) {
  ncomp <- fit$ncomp
  w <- fit$loading.weights
  m <- cor(fit$model$y, fit$scores, use = "pairwise")**2
  vip <- do.call(cbind, lapply(1:ncomp, function(x) .vip_vec(w, m, x)))
  dimnames(vip) <- dimnames(w)
  vip
}

.vip_vec <- function(w, m, comp) {
  rd <- colSums(m[, 1:comp, drop = FALSE])
  as.vector(sqrt(nrow(w) * rd %*% t(w[, 1:comp]^2) / sum(rd)))
}
