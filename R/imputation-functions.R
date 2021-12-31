.poplin_impute <- function(x,
                           method = c("knn", "randomforest", "pca", "univariate"),
                           ...) {
  method <- match.arg(method)
  switch(
    method,
    knn = .impute_knn(x, ...),
    randomforest = .impute_randomforest(x, ...),
    pca = .impute_pca(x, ...),
    univariate = .impute_univariate(x, ...)
    )
}

## Knn imputation
.impute_knn <- function(x, by = c("feature", "sample"), ...) {
  if (!requireNamespace("VIM", quietly = TRUE)) {
    stop("Package 'VIM' is required. Please install and try again.")
  }
  by <- match.arg(by)
  if (by == "feature") {
    out <- VIM::kNN(x, ...)[, 1:ncol(x)]
    ## VIM package internally converts x as data.table, which drops rownames
    rownames(out) <- rownames(x)
  } else {
    out <- t(VIM::kNN(t(x), ...))[1:nrow(x), ]
    colnames(out) <- colnames(x)
  }
  as.matrix(out)
}

## Random forest imputation
.impute_randomforest <- function(x, ...) {
  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("Package 'missForest' is required. Please install and try again.")
  }
  t(missForest::missForest(t(x), ...)$ximp)
}

## Bayesian PCA imputation
.impute_pca <- function(x, type = c("nipals", "bpca", "ppca", "svdImpute"), ...) {
  if (!requireNamespace("pcaMethods", quietly = TRUE)) {
    stop("Package 'pcaMethods' is required. Please install and try again.")
  }
  type <- match.arg(type)
  t(pcaMethods::pca(t(x), method = type, ...)@completeObs)
}

## Simple univariate imputation
.impute_univariate <- function(x, type = c("halfmin", "median", "mean")) {
  type <- match.arg(type)
  switch(
    type,
    halfmin = .impute_halfmin(x),
    median = .impute_median(x),
    mean = .impute_mean(x)
  )
}

.impute_halfmin <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- min(x, na.rm = TRUE) / 2
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}

.impute_median <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- median(x, na.rm = TRUE)
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}

.impute_mean <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}
