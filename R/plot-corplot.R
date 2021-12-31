##' Correlation plot
##'
##' Visualize correlations between samples or features. All values in a
##' correlation matrix are visualized by rectangles.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param unit character specifying whether a correlation matrix is
##'   computed based on samples or features.
##' @param use the method to compute correlations in the presence of missing
##'   values. Refer to \code{?cor} for details.
##' @param method character specifying which correlation coefficient is
##'   to be computed. Refer to \code{?cor} for details.
##' @param widths relative widths of heatmap and dendrogram.
##' @param heights relative heights of heatmap and dendrogram.
##' @param colors a vector of colors for heatmap.
##' @param label logical controlling whether cell values are shown.
##' @param digits the desired number of digits when \code{label = TRUE}.
##' @param grid_gap numeric specifying the gap between cells.
##' @param hide_colorbar logical controlling whether the color bar (legend) is
##'   hidden.
##' @param showticklabels a logical vector of length 2 (x-axis, y-axis). If
##'   \code{FALSE}, the ticks are removed from the sides of the plot.
##' @param row_dend_left logical controlling whether the row dendrogram is
##'   placed on the left on the plot.
##' @param ... additional arguments passed to [heatmaply::heatmaply].
##' @return gtable of aligned plots.
##' @name poplin_corplot
##' @examples
##'
##' ## poplin object
##' poplin_corplot(faahko_poplin, xin = "knn_cyclic")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn_cyclic")
##' poplin_corplot(m, label = TRUE)
NULL


##' @export
poplin_corplot <- function(x, ...) {
  UseMethod("poplin_corplot")
}

##' @rdname poplin_corplot
##' @export
poplin_corplot.default <- function(x, unit = c("sample", "feature"),
                     use = c("everything", "all.obs", "complete.obs",
                             "na.or.complete", "pairwise.complete.obs"),
                     method = c("pearson", "kendall", "spearman"),
                     widths = NULL, heights = NULL,
                     colors = viridis::viridis(n = 256, alpha = 1,
                                               begin = 0, end = 1,
                                               option = "viridis"),
                     label = FALSE, digits = 2,
                     grid_gap = 0,
                     hide_colorbar = FALSE,
                     showticklabels = c(TRUE, TRUE),
                     row_dend_left = FALSE, ...) {
  unit <- match.arg(unit)
  use <- match.arg(use)
  method <- match.arg(method)
  if (unit == "feature") {
    x <- t(x)
  }
  m <- cor(x, use = use, method = method)
  p <- heatmaply(m, colors = colors, grid_gap = grid_gap,
                 showticklabels = showticklabels,
                 row_dend_left = row_dend_left,
                 return_ppxpy = TRUE, plot_method = "ggplot", ...)
  if (label) {
    p$p <- p$p + geom_text(aes(label = format(!!quote(value), digits = digits)))
  }
  heatmaply:::arrange_plots(plots = p, widths = widths, heights = heights,
                            hide_colorbar = hide_colorbar,
                            row_dend_left = row_dend_left)
  ## ggheatmap(m, widths = widths, heights = heights,
  ##           grid_gap = grid_gap, colors = colors,
  ##           hide_colorbar = hide_colorbar, showticklabels = showticklabels,
  ##           row_dend_left = row_dend_left, ...)
}


##' @rdname poplin_corplot
##' @export
poplin_corplot.poplin <- function(x, xin , unit = c("sample", "feature"), 
                                  use = c("everything", "all.obs",
                                          "complete.obs", "na.or.complete",
                                          "pairwise.complete.obs"),
                                  method = c("pearson", "kendall", "spearman"),
                                  widths = NULL, heights = NULL,
                                  colors = viridis::viridis(n = 256, alpha = 1,
                                                            begin = 0, end = 1,
                                                            option = "viridis"),
                                  label = FALSE, digits = 2,
                                  grid_gap = 0, hide_colorbar = FALSE,
                                  showticklabels = c(TRUE, TRUE),
                                  row_dend_left = FALSE, ...) {
  m <- .verify_and_extract_input(x, xin)
  poplin_corplot.default(m, unit = unit, use = use, method = method,
                         widths = widths, heights = heights,
                         colors = colors, label = label, digits = digits,
                         grid_gap = grid_gap, hide_colorbar = hide_colorbar,
                         showticklabels = showticklabels,
                         row_dend_left = row_dend_left, ...)
}
