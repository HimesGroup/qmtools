##' Correlation plot
##'
##' Visualizes correlations between samples or features with a
##' \linkS4class{SummarizedExperiment} or matrix-like object where rows
##' represent features and columns represent samples. A correlation matrix is
##' visualized using a heatmap with dendrograms.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param log2 A logical specifying whether feature intensities needs to be
##'   log2-transformed before calculating a correlation matrix.
##' @param type A string specifying whether a correlation matrix is computed
##'   based on samples or features.
##' @param use A string specifying which method to compute correlations in the
##'   presence of missing values. Refer to \code{?cor} for details.
##' @param method A string specifying which correlation coefficient is to be
##'   computed. See \code{?cor} for details.
##' @param dendrogram A logical specifying whether dendogram is computed and
##'   reordering is performed.
##' @param colors A vector of colors for the heatmap.
##' @param label A logical specifying whether cell values are shown.
##' @param digits A numeric value specifying the desired number of digits when
##'   \code{label = TRUE}.
##' @param widths A numeric vectors specifying relative widths of heatmap and
##'   dendrogram.
##' @param heights A numeric vectors specifying relative heights of heatmap and
##'   dendrogram.
##' @param hide_colorbar A logical specifying whether the color bar (legend) is
##'   hidden.
##' @param showticklabels A logical vector of length 2 (x-axis, y-axis)
##'   specifying whether the ticks are removed from the sides of the plot.
##' @param row_dend_left A logical controlling whether the row dendrogram is
##'   placed on the left on the plot.
##' @param k_row A numeric value specifying the desired number of groups by
##'   which to color the dendrogram's branches in the rows. If `NA`, then
##'   [dendextend::find_k] is used to deduce the optimal number of clusters.
##' @param k_col A numeric value specifying the desired number of groups by
##'   which to color the dendrogram's branches in the columns. If `NA`, then
##'   [dendextend::find_k] is used to deduce the optimal number of clusters.
##' @param ... Additional arguments passed to [heatmaply::heatmaply].
##' @return A patchwork object of aligned ggplots.
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
##' plotCorr(faahko_se, i = "knn_vsn", method = "spearman", k_col = 4)
##'
##' ## Matrix
##' m <- assay(faahko_se, "knn_vsn")
##' plotCorr(m[1:50, ], type = "feature", method = "spearman")
##'
##' @export
plotCorr <- function(x, i,
                     type = c("sample", "feature"), log2 = FALSE,
                     use = c("everything", "all.obs",
                             "complete.obs", "na.or.complete",
                             "pairwise.complete.obs"),
                     method = c("pearson", "kendall", "spearman"),
                     dendrogram = TRUE,
                     colors = scales::viridis_pal()(256),
                     label = FALSE, digits = 2,
                     widths = c(0.8, 0.2), heights = c(0.2, 0.8),
                     hide_colorbar = FALSE,
                     showticklabels = c(TRUE, TRUE),
                     row_dend_left = FALSE, k_row = 1, k_col = 1,
                     ...) {
    if (is(x, "SummarizedExperiment")) {
        x <- assay(x, i)
    }
    type <- match.arg(type)
    use <- match.arg(use)
    if (log2) {
        x <- log2(x)
    }
    if (type == "sample") {
        m <- cor(x, use = use, method = method)
    } else {
        m <- cor(t(x), use = use, method = method)
    }
    p <- heatmaply::heatmaply(m, colors = colors,
                              showticklabels = showticklabels,
                              row_dend_left = row_dend_left,
                              Rowv = dendrogram, Colv = dendrogram,
                              k_row = k_row, k_col = k_col,
                              return_ppxpy = TRUE, plot_method = "ggplot",
                              ...)
    ## p <- p[!sapply(p, is.null)]
    p <- p[!vapply(p, is.null, logical(1))]
    if (label) {
        p$p <- p$p + geom_text(aes(label = format(!!quote(value),
                                                  digits = digits)))
    }
    if (!row_dend_left) {
        p$p <- p$p + theme(legend.position = "left")
    }
    if (hide_colorbar) {
        p$p <- p$p + theme(legend.position = "none")
    }
    if (!dendrogram) {
        p$p
    } else {
        .arrange_plots(p, row_dend_left = row_dend_left,
                       widths = widths, heights = heights)
    }
}

.gg_empty <- function() {
    ggplot() +
        ggplot2::theme_void() +
        theme(plot.margin = grid::unit(rep(0L, 4), "npc"))
}

.arrange_plots <- function(p, row_dend_left, widths, heights,
                           dendrogram_row = TRUE,
                           dendrogram_col = TRUE) {
    p <- lapply(p, function(x) {
        x + theme(plot.margin = grid::unit(rep(0L, 4), "npc"))
    })
    row1_list <- list(p$py, .gg_empty())
    row2_list <- list(p$p, p$px)
    if (row_dend_left) {
        row1_list <- rev(row1_list)
        row2_list <- rev(row2_list)
        widths <- rev(widths)
    }
    if (dendrogram_col) {
        if (dendrogram_row) {
            row1_list <- .replace_null(row1_list)
            row2_list <- .replace_null(row2_list)
            pt <- row1_list[[1]] + row1_list[[2]] + plot_layout(widths = widths)
            pb <- row2_list[[1]] + row2_list[[2]] + plot_layout(widths = widths)
            (pt / pb) + plot_layout(heights = heights)
        } else {
            (row1_list[[1]] / row2_list[[1]]) + plot_layout(heights = heights)
        }
    } else {
        row2_list[[1]] + row2_list[[2]] + plot_layout(widths = widths)
    }
}

.replace_null <- function(plotlist) {
    lapply(plotlist, function(x) {
        x %||% .gg_empty()
    })
}
