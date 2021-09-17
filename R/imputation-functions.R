##' @importFrom VIM kNN
.knn <- function(x, by = c("feature", "sample"), ...) {
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

##' @importFrom missForest missForest
.randomforest <- function(x, ...) {
  t(missForest(t(x), ...)$ximp)
}
