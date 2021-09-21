.poplin_impute <- function(x,
                           method = c("knn", "halfmin", "median",
                                      "mean", "pca", "randomforest"),
                           ...) {
  method <- match.arg(method)
  switch(
    method,
    knn = .poplin_impute_knn(x, ...),
    halfmin = .poplin_impute_halfmin(x, ...),
    median = .poplin_impute_median(x, ...),
    mean = .poplin_impute_mean(x, ...),
    pca = .poplin_impute_pca(x, ...),
    randomforest = .poplin_impute_randomforest(x, ...),
    )
}

.poplin_impute_halfmin <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- min(x, na.rm = TRUE) / 2
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}

.poplin_impute_median <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- median(x, na.rm = TRUE)
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}

.poplin_impute_mean <- function(x) {
  out <- apply(x, 1, function(x) {
    if (anyNA(x)) {
      val <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- val
    }
    x
  })
  t(out)
}

.poplin_impute_knn <- function(x, by = c("feature", "sample"), ...) {
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

.poplin_impute_pca <- function(x, ...) {
  if (!requireNamespace("pcaMethods", quietly = TRUE)) {
    stop("Package 'pcaMethods' is required. Please install and try again.")
  } 
  t(pcaMethods::pca(t(x), method = "bpca", ...)@completeObs)
}

.poplin_impute_randomforest <- function(x, ...) {
  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("Package 'missForest' is required. Please install and try again.")
  } 
  t(missForest::missForest(t(x), ...)$ximp)
}
