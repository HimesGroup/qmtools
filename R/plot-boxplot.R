##' Box plot
##'
##' Produce a box-and-whisker plot of the feature intensity values.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object
##' @param group a discrete variable to visualize the grouping structure.
##' @param pre_log2 logical controlling whether feature intensities are
##'   log2-transformed before visualization.
##' @param violin logical controlling whether a violin plot is drawn instead of
##'   the boxplot.
##' @param ylab the title of y-axis of the plot.
##' @param ... reserved for future use.
##' @return a ggplot object.
##' @name poplin_boxplot
##' @examples
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
NULL

##' @export
poplin_boxplot <- function(x, ...) {
  UseMethod("poplin_boxplot")
}

##' @rdname poplin_boxplot
##' @export
##' @importFrom stats reshape
##' @importFrom ggplot2 geom_boxplot geom_violin
poplin_boxplot.default <- function(x, group, pre_log2 = FALSE, violin = FALSE,
                           ylab = "Intensity", ...) {
  ## convert wide to long format to draw fig
  if (pre_log2) {
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
    p <- ggplot(dd, aes(x = !!quote(id), y = !!quote(value), fill = !!quote(group)))
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
          axis.title.x = element_blank())
}

##' @rdname poplin_boxplot
##' @export
poplin_boxplot.poplin <- function(x, xin, group, pre_log2 = FALSE,
                                  violin = FALSE, ylab = "Intensity", ...) {
  m <- .verify_and_extract_input(x, xin)
  poplin_boxplot.default(m, group = group, pre_log2 = pre_log2,
                         violin = violin, ylab = ylab, ...)
}
