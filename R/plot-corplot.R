##' Correlation plot
##'
##' Visualize correlations between samples or features. All values in a
##' correlation matrix are visualized by rectangles.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param unit A character string specifying whether a correlation matrix is
##'   computed based on samples or features. One of "sample" or "feature".
##' @param use A method for computing correlations in the presence of missing
##'   values. Refer to \code{?cor} for details.
##' @param method A character string specifying which correlation coefficient is
##'   to be computed. Refer to \code{?cor} for details.
##' @param widths Relative widths of heatmap and dendrogram.
##' @param heights Relative heights of heatmap and dendrogram.
##' @param colors A vector of colors for heatmap.
##' @param label Logical controlling whether cell values are shown.
##' @param digits The desired number of digits when \code{label = TRUE}.
##' @param grid_gap Gap between cells.
##' @param hide_colorbar Logical controlling whether the color bar (legend) is
##'   hidden.
##' @param showticklabels A logical vector of length 2 (x-axis, y-axis). If
##'   \code{FALSE}, the ticks are removed from the sides of the plot.
##' @param row_dend_left Logical controlling whether the row dendrogram is
##'   placed on the left on the plot.
##' @param ... Additional arguments passed to [heatmaply::heatmaply].
##' @return gtable of aligned plots.
##' @name poplin_corplot
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
    p$p <- p$p + geom_text(aes(label = format(value, digits = digits)))
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
poplin_corplot.poplin <- function(x, poplin_in , unit = c("sample", "feature"), 
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
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_corplot.default(m, use = use, method = method,
                         widths = widths, heights = heights,
                         colors = colors, label = label, digits = digits,
                         grid_gap = grid_gap, hide_colorbar = hide_colorbar,
                         showticklabels = showticklabels,
                         row_dend_left = row_dend_left, ...)
}
