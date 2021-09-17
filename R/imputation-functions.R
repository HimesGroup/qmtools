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

##' @importFrom VIM kNN
.poplin_impute_knn <- function(x, by = c("feature", "sample"), ...) {
  by <- match.arg(by)
  if (by == "feature") {
    out <- kNN(x, ...)[, 1:ncol(x)]
    ## VIM package internally converts x as data.table, which drops rownames
    rownames(out) <- rownames(x)
  } else {
    out <- t(kNN(t(x), ...))[1:nrow(x), ]
    colnames(out) <- colnames(x)
  }
  out
}

##' @importFrom missMDA imputePCA
.poplin_impute_pca <- function(x, ...) {
  imputePCA(x, ...)$completeObs
}

##' @importFrom missForest missForest
.poplin_impute_randomforest <- function(x, ...) {
  t(missForest(t(x), ...)$ximp)
}
