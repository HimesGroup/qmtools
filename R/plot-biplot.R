##' Biplot of dimension-reduced data
##'
##' Visualize an overlay of a score plot and a loading plot using the
##' [poplin_reduce] output.
##'
##' @param x A dimension-reduced data matrix produced by [poplin_reduce] or
##'   \linkS4class{poplin} object containing dimension-reduced data.
##' @param y A data matrix for loadings. Not required for the [reduce_pca] and
##'   [reduce_plsda] outputs.
##' @param poplin_in Name of a dimension-reduced data matrix to retrieve.
##' @param comp A numeric vector of length 2 indicating two components to plot.
##' @param group A discrete variable to visualize the grouping structure.
##' @param group_col A vector of colors with the same length of unique values in
##'   \code{group}.
##' @param point_size Numeric value controlling the size of points.
##' @param point_shape_by_group Logical controlling whether each group have
##'   different shapes of data points. Also can be a numeric vector with the
##'   same length of unique values in \code{group} to manually set point shapes.
##' @param label Logical controlling whether score labels are shown instead of
##'   points.
##' @param label_size Numeric value controlling the size of labels.
##' @param label_subset A character vector specifying a subset of score labels
##'   to display.
##' @param ellipse Logical controlling whether data ellipses are shown using
##'   [ggplot2::stat_ellipse].
##' @param xlab The title of x-axis of the plot.
##' @param ylab The title of y-axis of the plot.
##' @param title The main title of the plot.
##' @param legend Logical controlling whether the plot legend is shown.
##' @param arrow_len Numeric value controlling the length of arrow head.
##' @param arrow_col Character string indicating the color of arrows.
##' @param arrow_alpha Numeric value controlling the transparency of arrow.
##' @param arrow_label Logical controlling whether text labels for arrows are
##'   shown.
##' @param arrow_label_ext Numeric value controlling the scalar extension for
##'   arrow labels.
##' @param arrow_label_size Numeric value controlling the size of arrow labels.
##' @param arrow_label_col Character string indicating the color of arrow
##'   labels.
##' @param arrow_label_subset A character vector specifying a subset of arrow
##'   labels to display.
##' @param ... Arguments passed to the default method.
##' @return A ggplot object.
##' @seealso [poplin_reduce], [poplin_scoreplot].
##' @name poplin_biplot
NULL

##' @importFrom ggplot2 geom_segment scale_x_continuous scale_y_continuous sec_axis
##' @export
poplin_biplot <- function(x, ...) {
  UseMethod("poplin_biplot")
}

##' @rdname poplin_biplot
##' @export
poplin_biplot.default <- function(x, y, comp = 1:2, group,
                                  group_col = NULL,
                                  point_size = 1.5,
                                  point_shape_by_group = FALSE,
                                  label = FALSE, label_size = 3.88,
                                  label_subset = NULL,
                                  ellipse = FALSE,
                                  xlab = NULL, ylab = NULL,
                                  title = NULL, legend = TRUE,
                                  arrow_len = 0.2, arrow_col = "red",
                                  arrow_alpha = 0.3,
                                  arrow_label = TRUE, arrow_label_ext = 1.1,
                                  arrow_label_size = 3.88,
                                  arrow_label_col = "red",
                                  arrow_label_subset = NULL, ...) {
  p <- poplin_scoreplot(x, comp = comp, group = group,
                        group_col = group_col,
                        point_shape_by_group = point_shape_by_group,
                        point_size = point_size,
                        label = label, label_size = label_size,
                        label_subset = label_subset,
                        ellipse = ellipse,
                        xlab = xlab, ylab = ylab,
                        title = title, legend = legend)
  scalers <- 0.7 * c(
    (max(x[, comp[1]]) - min(x[, comp[1]])) / (max(y[, comp[1]]) - min(y, comp[1])),
    (max(x[, comp[2]]) - min(x[, comp[2]])) / (max(y[, comp[2]]) - min(y, comp[2]))
  )
  y <- sweep(y[, comp], 2, scalers, FUN = "*")
  y <- as.data.frame(y)
  comp_xend <- rlang::sym(names(y)[1])
  comp_yend <- rlang::sym(names(y)[2])
  p <- p + geom_segment(
             inherit.aes = FALSE,
             data = y,
             aes(x = 0, y = 0, xend = !!comp_xend, yend = !!comp_yend),
             arrow = grid::arrow(length = grid::unit(arrow_len, "cm")),
             col = arrow_col,
             alpha = arrow_alpha
           ) +
    scale_x_continuous(sec.axis = sec_axis( ~ . / scalers[1])) +
    scale_y_continuous(sec.axis = sec_axis( ~ . / scalers[2]))
  if (arrow_label) {
    y$label <- rownames(y)
    if (!is.null(arrow_label_subset)) {
      y$label <- with(y, ifelse(label %in% arrow_label_subset, label, ""))
    }
    y$x_adj <- y[, names(y)[1]] * arrow_label_ext
    y$y_adj <- y[, names(y)[2]] * arrow_label_ext
    p <- p + geom_text(
               inherit.aes = FALSE,
               ## data = y, aes(x = x_adj, y = y_adj, label = label),
               data = y, aes(x = !!quote(x_adj), y = !!quote(y_adj), label = label),
               col = arrow_label_col, size = arrow_label_size
             )
  }
  p
}

##' @rdname poplin_biplot
##' @export
## poplin_biplot.poplin.pca <- function(x, scale = 1, comp = 1:2, group, ...) {
poplin_biplot.poplin.pca <- function(x, comp = 1:2, group, label = FALSE, ...) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose two components within 1:ncol(x).")
  }
  comp <- sort(comp)
  n <- nrow(x)
  lam <- attr(x, "sdev")[comp] * sqrt(n)
  ## if (scale < 0 || scale > 1)
  ##   warning("'scale' is outside [0, 1]")
  ## if (scale != 0)
  ##   lam <- lam**scale
  ## else lam <- 1
  X <- t(t(x[, comp]) / lam)
  Y <- t(t(attr(x, "loadings")[, comp]) * lam)
  poplin_biplot.default(X, Y, comp = 1:2, group = group, label = label, ...)
}

##' @rdname poplin_biplot
##' @export
poplin_biplot.poplin.plsda <- function(x, comp = 1:2,
                                       group = attr(x, "Y.observed"),
                                       label = label, ...) {
  X <- x[, comp]
  Y <- attr(x, "loadings")[, comp]
  poplin_biplot.default(X, Y, comp = 1:2, group = group, label = label, ...)
}


##' @rdname poplin_biplot
##' @export
poplin_biplot.poplin <- function(x, poplin_in, comp = 1:2, group,
                                 label = FALSE, ...) {
  if (!(poplin_in %in% poplin_reduced_names(x))) {
    stop("'", poplin_in, "' is not found in the poplin object.\n",
         "Input must be one of poplin_reduced_names(x).")
  }
  m <- poplin_reduced(x, poplin_in)
  if (is.null(colnames(m))) {
    stop("colnames of 'poplin_reduced(x, poplin_in)' must be non-NULL.")
  }
  if (label && is.null(rownames(m))) {
    stop("rownames of 'poplin_reduced(x, poplin_in)' ",
         "'must be non-NULL if label = TRUE.")
  }
  poplin_biplot(m, comp = comp, group = group, label = label, ...)
  ## if (inherits(m, "poplin.pca")) {
  ##   poplin_biplot.poplin.pca(m, comp = comp, ...)
  ## } else if (inherits(m, "poplin.plsda")) {
  ##   poplin_biplot.poplin.plsda(m, comp = comp, ...)
  ## } else {
  ##   poplin_biplot.default(m, comp = comp, ...)
  ## }
}
