##' Missing value plot
##'
##' Visualizes missing values with a \link{SummarizedExperiment} object or
##' matrix of intensity data where rows represent features and columns represent
##' samples. All values in a data matrix are re-coded (1: missing; 0:
##' non-missing). The left panel displays the amount of missing values in each
##' samples. The right panel displays the pattern of missing values using a
##' heatmap with dendrograms.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param group A discrete variable to change colors of the barplot by sample
##'   groups.
##' @param dendrogram_row A logical specifying whether dendogram is computed and
##'   reordering is performed based on rows.
##' @param dendrogram_col A logical specifying whether dendogram is computed and
##'   reordering is performed based on columns.
##' @param colors A vector of colors for the heatmap.
##' @param hide_colorbar A logical specifying whether the color bar (legend) in
##'   the heatmap is hidden.
##' @param showticklabels A logical vector of length 2 (x-axis, y-axis)
##'   specifying whether the ticks are removed from the sides of the heatmap.
##' @param row_dend_left A logical controlling whether the row dendrogram is
##'   placed on the left on the heatmap.
##' @param k_row A numeric value specifying the desired number of groups by
##'   which to color the dendrogram's branches in the rows. If `NA`, then
##'   [dendextend::find_k] is used to deduce the optimal number of clusters.
##' @param k_col A numeric value specifying the desired number of groups by
##'   which to color the dendrogram's branches in the columns. If `NA`, then
##'   [dendextend::find_k] is used to deduce the optimal number of clusters.
##' @param ... Additional arguments passed to [heatmaply::heatmaply].
##'
##' @return A patchwork object of aligned ggplots
##'
##' @references
##'
##' Tal Galili, Alan O'Callaghan, Jonathan Sidi, Carson Sievert;
##' heatmaply: an R package for creating interactive cluster heatmaps for
##' online publishing, Bioinformatics, btx657,
##' https://doi.org/10.1093/bioinformatics/btx657
##'
##' @examples
##'
##' data(faahko_se)
##'
##' ## Sample group
##' g <- colData(faahko_se)$sample_group
##'
##' ## SummarizedExperiment object
##' plotMiss(faahko_se, i = 1, group = g)
##'
##' ## Matrix
##' m <- assay(faahko_se, i = 1)
##' plotMiss(m, group = g, dendrogram_col = TRUE)
##'
##' @export
plotMiss <- function(x, i, group,
                     dendrogram_row = TRUE,
                     dendrogram_col = FALSE,
                     colors = viridis::viridis(2),
                     hide_colorbar = TRUE,
                     showticklabels = c(TRUE, FALSE),
                     row_dend_left = FALSE,
                     k_row = 1, k_col = 1, ...) {
    if (is(x, "SummarizedExperiment")) {
        x <- assay(x, i)
    }
    xm <- heatmaply::is.na10(x)
    p_bar <- .na_barplot(xm, group = group)
    p <- heatmaply::heatmaply(xm, colors = colors,
                              showticklabels = showticklabels,
                              row_dend_left = row_dend_left,
                              Rowv = dendrogram_row, Colv = dendrogram_col,
                              k_row = k_row, k_col = k_col,
                              return_ppxpy = TRUE, plot_method = "ggplot",
                              ...)
    if (!row_dend_left) {
        p$p <- p$p + theme(legend.position = "left")
    }
    if (hide_colorbar) {
        p$p <- p$p + theme(legend.position = "none")
    }
    if (!dendrogram_row && !dendrogram_col) {
        p <- p$p
    } else {
        p <- .arrange_plots(p, row_dend_left = row_dend_left,
                            widths = c(0.8, 0.2), heights = c(0.1, 0.9),
                            dendrogram_row = dendrogram_row,
                            dendrogram_col = dendrogram_col)
    }
    p_bar + p + plot_layout(widths = c(0.4, 0.6))
}

.na_barplot <- function(m, group) {
    mc <- apply(m, 2, function(x) {
        100 * sum(x) / nrow(m)
    })
    d <- data.frame(id = factor(names(mc), levels = unique(names(mc))),
                    mc = mc)
    if (!missing(group)) {
        d$group <- group
    }
    if (missing(group)) {
        p <- ggplot(d, aes(x = !!quote(id), y = !!quote(mc))) +
            ggplot2::geom_col(fill = "skyblue")
    } else {
        p <- ggplot(d, aes(x = !!quote(id), y = !!quote(mc),
                           fill = !!quote(group))) +
            ggplot2::geom_col()
    }
    p +
        ylab("Missing values %") +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        theme_bw() +
        theme(legend.title = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
}
