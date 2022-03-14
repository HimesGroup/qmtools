##' Box plot
##'
##' Produces a box-and-whisker plot with a \linkS4class{SummarizedExperiment} or
##' matrix-like object where rows represent features and columns represent
##' samples.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param group A discrete variable to visualize the grouping structure.
##' @param log2 A logical specifying whether feature intensities needs to be
##'   log2-transformed before visualization.
##' @param violin A logical specifying whether a violin plot is shown instead of
##'   a boxplot.
##' @param ylab A string specifying the title of y-axis.
##' @return A ggplot object.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' ## Sample group
##' g <- colData(faahko_se)$sample_group
##'
##' ## SummarizedExperiment object
##' plotBox(faahko_se, i = "knn", group = g, log2 = TRUE) # before normalization
##'
##' ## Matrix
##' m <- assay(faahko_se, "knn_vsn")
##' plotBox(m, group = g) # after normalization
##'
##' @export
plotBox <- function(x, i, group, log2 = FALSE, violin = FALSE,
                    ylab = "Intensity") {
    if (is(x, "SummarizedExperiment")) {
        x <- assay(x, i)
    }
    if (log2) {
        x <- log2(x)
    }
    dt <- as.data.frame(t(x))
    cols <- names(dt)
    dt$id <- rownames(dt)
    if (!missing(group)) {
        dt$group <- group
    }
    dd <- reshape(dt, varying = cols, timevar = "feature",
                  times = cols, v.names = "value",
                  direction = "long", sep = "")
    dd$id <- factor(dd$id, levels = unique(dd$id))
    if (missing(group)) {
        p <- ggplot(dd, aes(x = !!quote(id), y = !!quote(value)))
    } else {
        p <- ggplot(dd, aes(x = !!quote(id), y = !!quote(value),
                            fill = !!quote(group)))
    }
    if (violin) {
        p <- p + geom_violin()
    } else {
        p <- p + geom_boxplot()
    }
    p +
        ylab(ylab) +
        theme_bw() +
        theme(legend.title = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
}
