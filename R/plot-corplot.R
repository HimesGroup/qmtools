##' @export
poplin_corplot <- function(x, ...) {
  UseMethod("poplin_corplot")
}

##' @export
##' @importFrom heatmaply ggheatmap
poplin_corplot.default <- function(x,
                     use = c("everything", "all.obs", "complete.obs",
                             "na.or.complete", "pairwise.complete.obs"),
                     method = c("pearson", "kendall", "spearman"),
                     showticklabels = c(TRUE, TRUE), ...) {
  use <- match.arg(use)
  method <- match.arg(method)
  m <- cor(x, use = use, method = method)
  ggheatmap(m, showticklabels = showticklabels, ...)
}

##' @export
poplin_corplot.poplin <- function(x, poplin_in , ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_corplot.default(m, ...)
}
