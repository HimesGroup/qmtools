##' Box plot
##'
##' Produce a box-and-whisker plot of the feature intensity values.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param group A discrete variable to visualize the grouping structure.
##' @param pre_log2 If \code{TRUE}, feature intensities are log2-transformed
##'   before plotting.
##' @param violin If \code{TRUE}, a violin plot is drawn instead of the boxplot.
##' @param ylab The title of y-axis of the plot.
##' @param ... Reserved for future use.
##' @return A ggplot object.
##' @name poplin_boxplot
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
poplin_boxplot.poplin <- function(x, poplin_in, group, pre_log2 = FALSE,
                                  violin = FALSE, ylab = "Intensity", ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_boxplot.default(m, group = group, pre_log2 = pre_log2,
                         violin = violin, ylab = ylab, ...)
}
