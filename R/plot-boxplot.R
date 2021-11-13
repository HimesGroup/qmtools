##' @export
poplin_boxplot <- function(x, ...) {
  UseMethod("poplin_boxplot")
}

##' @export
##' @importFrom stats reshape
##' @importFrom ggplot2 geom_boxplot geom_violin
poplin_boxplot.default <- function(x, group, log2 = FALSE, violin = FALSE,
                           ylab = "Intensity") {
  ## convert wide to long format to draw fig
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
    p <- ggplot(dd, aes(x = id, y = value))
  } else {
    p <- ggplot(dd, aes(x = id, y = value, fill = group))
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

##' @export
poplin_boxplot.poplin <- function(x, poplin_in, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_boxplot.default(m, ...)
}
