##' @export
##' @importFrom heatmaply is.na10 heatmaply
poplin_naplot <- function(x, widths = NULL, heights = NULL,
                          grid_gap = 1, colors = viridis::viridis(2),
                          hide_colorbar = TRUE, showticklabels = c(TRUE, FALSE),
                          row_dend_left = FALSE, ...) {
  p <- heatmaply(is.na10(x), grid_gap = grid_gap, colors = colors,
                 showticklabels = showticklabels,
                 row_dend_left = row_dend_left,
                 return_ppxpy = TRUE, plot_method = "ggplot", ...)
  ## heatmaply:::arrange_plots(plots = p)
  ## temporary issue in arrange_plots in hide_colorbar
  .arrange_plots(plots = p, widths = widths, heights = heights,
                 hide_colorbar = hide_colorbar, row_dend_left = row_dend_left)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.arrange_plots <- function(plots, widths = NULL, heights = NULL,
                          row_dend_left = FALSE, hide_colorbar = FALSE) {

  plots <- plots[!sapply(plots, is.null)]
  if (!row_dend_left) {
    plots$p <- plots$p + theme(legend.position = "left")
  }
  if (hide_colorbar) {
    plots$p <- plots$p + theme(legend.position = "none")
  }
  plots <- lapply(
    plots,
    function(x) x + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "npc"))
  )

  column_list <- list(plots$py, plots$pc, plots$p)
  ind_null_col <- sapply(column_list, is.null)

  row1_list <- list(
    plots$py, heatmaply:::ggplot_empty(), heatmaply:::ggplot_empty()
  )
  row2_list <- list(
    plots$pc, heatmaply:::ggplot_empty(), heatmaply:::ggplot_empty()
  )
  row3_list <- list(plots$p, plots$pr, plots$px)

  if (row_dend_left) {
    row3_list <- rev(row3_list)
    row2_list <- rev(row2_list)
    row1_list <- rev(row1_list)
  }
  plotlist <- c(
    row1_list,
    row2_list,
    row3_list
  )

  nrows <- sum(!ind_null_col)
  ind_remove_col <- rep(ind_null_col, each = length(plotlist) / 3)

  ind_null_row <- sapply(row3_list, is.null)
  ncols <- sum(!ind_null_row)
  ind_remove_row <- rep(ind_null_row, length.out = length(plotlist))
  plotlist <- plotlist[!(ind_remove_row | ind_remove_col)]

  default_widths <- heatmaply:::default_dims(plots$px, plots$pr)
  if (row_dend_left) default_widths <- rev(default_widths)
  egg::ggarrange(
    plots = plotlist,
    ncol = ncols,
    widths = widths %||% default_widths,
    heights = heights %||% rev(heatmaply:::default_dims(plots$py, plots$pc))
  )
}

