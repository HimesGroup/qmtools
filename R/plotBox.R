##' Box plot
##'
##' Produces a box-and-whisker plot with a \linkS4class{SummarizedExperiment} or
##' matrix-like object where rows represent features and columns represent
##' samples.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param xin A string specifying which assay values to use when \code{x} is a
##'   SummarizedExperiment object.
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
##' data(faahko_poplin)
##'
##' ## sample group variable
##' group <- colData(faahko_poplin)$sample_group
##'
##' ## poplin object
##' poplin_boxplot(faahko_poplin, xin = "knn_cyclic", group = group)
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn_cyclic")
##' poplin_boxplot(m, group = group)
##' @export
##' @importFrom stats reshape
##' @importFrom ggplot2 geom_boxplot geom_violin element_text
plotBox <- function(x, xin, group, log2 = FALSE, violin = FALSE,
                    ylab = "Intensity") {
  if (is(x, "SummarizedExperiment")) {
    x <- assay(x, xin)
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
