##' @export
poplin_naplot <- function(x, ...) {
  UseMethod("poplin_naplot")
}

##' @export
##' @importFrom heatmaply is.na10 heatmaply
poplin_naplot.default <- function(x, widths = NULL, heights = NULL,
                          grid_gap = 1, colors = viridis::viridis(2),
                          hide_colorbar = TRUE, showticklabels = c(TRUE, FALSE),
                          row_dend_left = FALSE, ...) {
  p <- heatmaply(is.na10(x), grid_gap = grid_gap, colors = colors,
                 showticklabels = showticklabels,
                 row_dend_left = row_dend_left,
                 return_ppxpy = TRUE, plot_method = "ggplot", ...)
  heatmaply:::arrange_plots(plots = p, hide_colorbar = hide_colorbar)
  ## temporary issue in arrange_plots in hide_colorbar
  ## .arrange_plots(plots = p, widths = widths, heights = heights,
  ##                hide_colorbar = hide_colorbar, row_dend_left = row_dend_left)
}

##' @export
poplin_naplot.poplin <- function(x, poplin_in, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_naplot.default(m, ...)
}
