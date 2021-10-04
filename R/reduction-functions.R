.poplin_reduce <- function(x, method = c("pca", "tsne", "plsda"), y, ncomp = 2, ...) {
  method <- match.arg(method)
  if (length(ncomp) != 1) {
    stop("'ncomp' must be a positive integer.")
  }
  switch(
    method,
    pca = .poplin_reduce_pca(x, ncomp = ncomp, ...),
    tsne = .poplin_reduce_tsne(x, ncomp = ncomp, ...),
    plsda = .poplin_reduce_tsne(x, y = y, ncomp = ncomp, ...)
  )
}

.poplin_reduce_pca <- function(x, ncomp = 2, center = TRUE, scale = FALSE, ...) {
  if (ncomp > min(dim(x))) {
    stop("'ncomp' must be <= min(dim(x))")
  }
  fun_call <- match.call()
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
    cat("Missing values(s) in 'x'.\n")
    if (!requireNamespace("pcaMethods", quietly = TRUE)) {
      stop(
        "Package 'pcaMethods' is required to perform PCA with missing values. ",
        "Please install and try again or impute missing values."
      )
    } else {
      cat("Performing Bayesian PCA...\n")
      out <- .pca_bayesian(xt, ncomp = ncomp)
    }
  }
  attr(out, "origD") <- dim(x)
  attr(out, "centered") <- center
  attr(out, "scaled") <- scale
  attr(out, "call") <- fun_call
  out <- poplin.matrix(out, "poplin.pca")
}

.pca_svd <- function(x, ncomp) {
  pc <- prcomp(x, center = FALSE, scale. = FALSE)
  imp <- summary(pc)$importance
  out <- pc$x[, 1:ncomp]
  attr(out, "method") <- "PCA (SVD)"
  attr(out, "ncomp") <- ncomp
  attr(out, "R2") <- imp[2, 1:ncomp]
  attr(out, "R2cum") <- imp[3, 1:ncomp]
  attr(out, "loadings") <- pc$rotation[, 1:ncomp]
  attr(out, "sdev") <- pc$sdev[1:ncomp]
  out
}

.pca_bayesian <- function(x, ncomp, ...) {
  res <- pcaMethods::bpca(Matrix = x, nPcs = ncomp, ...)
  out <- res@scores
  colnames(out) <- paste0("PC", 1:ncol(out))
  rownames(out) <- rownames(x)
  colnames(res@loadings) <- paste0("PC", 1:ncol(out))
  rownames(res@loadings) <- colnames(x)
  attr(out, "method") <- "PCA (Bayesian)"
  attr(out, "ncomp") <- ncomp
  attr(out, "R2") <- c(res@R2cum[1], diff(res@R2cum))
  attr(out, "R2cum") <- res@R2cum
  attr(out, "loadings") <- res@loadings
  attr(out, "sdev") <- apply(res@scores, 2, sd)
  out
}

.poplin_reduce_tsne <- function(x, ncomp = 2, normalize = TRUE, ...) {
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop("Package 'Rtsne' is required. Please install and try again.")
  }
  if (any(!is.finite(x))) {
    stop("infinite or missing values in 'x'.")
  }
  fun_call <- match.call()
  xt <- t(x) # transpose matrix;
  if (normalize) {
    xt <- Rtsne::normalize_input(xt)
  }
  res <- Rtsne::Rtsne(xt, dims = ncomp, ...)
  out <- res$Y
  colnames(out) <- paste0("tSNE", 1:ncol(out))
  rownames(out) <- colnames(x)
  attr(out, "method") <- "t-SNE"
  attr(out, "ncomp") <- ncomp
  attr(out, "perplexity") <- res$perplexity
  attr(out, "theta") <- res$theta
  attr(out, "eta") <- res$eta
  attr(out, "origD") <- dim(x)
  attr(out, "normalized") <- normalize
  attr(out, "call") <- fun_call
  poplin.matrix(out, "poplin.tsne")
}


.poplin_reduce_plsda <- function(x, y, ncomp = 2, center = TRUE, scale = FALSE,
                                 ...) {
  if (!requireNamespace("pls", quietly = TRUE)) {
    stop("Package 'pls' is required. Please install and try again.")
  }
  if (!is.factor(y)) {
    stop("'y' must be a factor.")
  }
  xt <- t(x)
  fun_call <- match.call()
  y_levels <- levels(y)
  y_dummy <- model.matrix(~ y - 1)
  colnames(y_dummy) <- gsub("^y", "", colnames(y_dummy))

  d <- data.frame(y = I(y_dummy), x = I(xt))
  fit <- pls::plsr(y ~ x, data = d, ncomp = ncomp,
                   center = center, scale = scale, ...)
  out <- pls::scores(fit)
  ## pred_vals <- predict(fit, ncomp = fit$ncomp)

  attr(out, "method") <- paste0("PLS-DA (", fit$method, ")")
  attr(out, "origD.X") <- dim(x)
  attr(out, "responses") <- pls::respnames(fit)
  attr(out, "predictors") <- pls::prednames(fit)
  attr(out, "coefficients") <- fit$coefficients
  attr(out, "loadings") <- fit$loadings
  attr(out, "loadings.weights") <- fit$loadings.weights
  attr(out, "Yscores") <- fit$Yscores
  attr(out, "Yloadings") <- fit$Yloadings
  attr(out, "projection") <- fit$projection
  attr(out, "fitted.values") <- fitted(fit)
  attr(out, "residuals") <- residuals(fit)
  attr(out, "ncomp") <- fit$ncomp
  attr(out, "centered") <- center
  attr(out, "scaled") <- scale
  attr(out, "validation") <- fit$validation
  attr(out, "call") <- fun_call
  poplin.matrix(out, "poplin.plsda")
}

