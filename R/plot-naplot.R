##' Missing value plot
##'
##' Visualize data for exploring missing value (NA) patterns. All values in a
##' data matrix are recoded (1: missing; 0: non-missing) and visualized by
##' rectangles.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
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
##' @return \code{gtable} of aligned plots.
##' @name poplin_naplot
NULL

##' @export
poplin_naplot <- function(x, ...) {
  UseMethod("poplin_naplot")
}

##' @rdname poplin_naplot
##' @export
##' @importFrom heatmaply is.na10 heatmaply
poplin_naplot.default <- function(x, widths = NULL, heights = NULL,
                                  colors = viridis::viridis(2),
                                  label = FALSE, digits = 2,
                                  grid_gap = 1, hide_colorbar = TRUE,
                                  showticklabels = c(TRUE, FALSE),
                                  row_dend_left = FALSE, ...) {
  p <- heatmaply(is.na10(x), colors = colors, grid_gap = grid_gap,
                 showticklabels = showticklabels,
                 row_dend_left = row_dend_left,
                 return_ppxpy = TRUE, plot_method = "ggplot", ...)
  if (label) {
    p$p <- p$p + geom_text(aes(label = format(value, digits = digits)))
  }
  heatmaply:::arrange_plots(plots = p, widths = widths, heights = heights,
                            hide_colorbar = hide_colorbar,
                            row_dend_left = row_dend_left)
  ## temporary issue in arrange_plots in hide_colorbar
  ## .arrange_plots(plots = p, widths = widths, heights = heights,
  ##                hide_colorbar = hide_colorbar, row_dend_left = row_dend_left)
}

##' @rdname poplin_naplot
##' @export
poplin_naplot.poplin <- function(x, poplin_in, widths = NULL, heights = NULL,
                                 colors = viridis::viridis(2),
                                 label = FALSE, digits = 2,
                                 grid_gap = 1,
                                 hide_colorbar = TRUE,
                                 showticklabels = c(TRUE, FALSE),
                                 row_dend_left = FALSE, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_naplot.default(m, widths = widths, heigths = heights,
                        colors = colors, label = label, digits = digits,
                        grid_gap = grid_gap, hide_colorbar = hide_colorbar,
                        showticklabels = showticklabels,
                        row_dend_left = row_dend_left, ...)
}
