##' Score plot of dimension-reduced data
##'
##' Visualize the data onto a lower-dimensional space using the [poplin_reduce]
##' output.
##'
##' @param x A dimension-reduced data matrix produced by [poplin_reduce] or
##'   \linkS4class{poplin} object containing dimension-reduced data.
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
##' @param ... Arguments passed to the default method.
##' @return A ggplot object.
##' @seealso [poplin_reduce], [poplin_biplot].
##' @name poplin_scoreplot
NULL

##' @export
##' @importFrom ggplot2 ggplot aes geom_point geom_text stat_ellipse
##' @importFrom ggplot2 xlab ylab ggtitle theme_bw theme element_blank
##' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_shape_manual
poplin_scoreplot <- function(x, ...) {
  UseMethod("poplin_scoreplot")
}

##' @rdname poplin_scoreplot
##' @export
poplin_scoreplot.default <- function(x, comp = c(1, 2), group,
                                group_col = NULL,
                                point_size = 1.5,
                                point_shape_by_group = FALSE,
                                label = FALSE, label_size = 3.88,
                                label_subset = NULL,
                                ellipse = FALSE,
                                xlab = NULL, ylab = NULL,
                                title = NULL, legend = TRUE, ...) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose only two components within 1:ncol(x).")
  }
  if (!is.null(group_col)) {
    if (!missing(group) && length(group_col) != length(unique(group))) {
      stop("'group_col' must have the same length of unique values in 'group'.")
    }
  }
  comp <- sort(comp)
  x <- as.data.frame(x)
  if (is.null(colnames(x))) {
    stop("colnames(x) must be non-NULL.")
  } else {
    comp_x <- rlang::sym(colnames(x)[comp[1]])
    comp_y <- rlang::sym(colnames(x)[comp[2]])
  }
  if (missing(group)) {
    p <- ggplot(x, aes(x = !!comp_x, y = !!comp_y))
  } else {
    x$group <- factor(group, levels = unique(group))
    if (isFALSE(point_shape_by_group)) {
      p <- ggplot(x, aes(x = !!comp_x, y = !!comp_y,
                         group = group, col = group, fill = group))
    } else {
      p <- ggplot(x, aes(x = !!comp_x, y = !!comp_y,
                         group = group, col = group, fill = group,
                         shape = group))
      if (!isTRUE(point_shape_by_group)) {
        if (length(point_shape_by_group) == length(unique(group))) {
          p <- p + scale_shape_manual(values = point_shape_by_group)
        } else {
            stop("non-logical values of 'point_shape_by_group' must have ",
                 "the same length of unique values in 'group'.")
        }
      }
    }
    if (!is.null(group_col)) {
      p <- p + scale_color_manual(values = group_col)
    }
  }
  if (ellipse) {
    p <- p + stat_ellipse(geom = "polygon", alpha = 0.1)
    if (!is.null(group_col)) {
      p <- p + scale_fill_manual(values = group_col)
    }
  }
  if (label) {
    if (is.null(rownames(x))) {
      stop("rownames(x) must be non-NULL if label = TRUE.")
    } else {
      if (is.null(label_subset)) {
        point_label <- rownames(x)
      } else {
        point_label <- ifelse(rownames(x) %in% label_subset, rownames(x), "")
      }
      p <- p + geom_text(aes(label = point_label), show.legend = FALSE,
                         size = label_size)
    }
  } else {
    p <- p + geom_point(size = point_size)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!is.null(ylab)) {
    p <- p + ylab(ylab)
  }
  if (legend) {
    p + theme_bw() + theme(legend.title = element_blank())
  } else {
    p + theme_bw() + theme(legend.position = "none")
  }
}

##' @rdname poplin_scoreplot
##' @export
poplin_scoreplot.poplin.pca <- function(x, comp = c(1, 2),
                                        group, group_col = NULL, label = FALSE,
                                        xlab = NULL, ylab = NULL,
                                        ...) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose only two components within 1:ncol(x).")
  }
  if (is.null(colnames(x))) {
    stop("colnames(x) must be non-NULL.")
  }
  if (!is.null(group_col)) {
    if (!missing(group) && length(group_col) != length(unique(group))) {
      stop("'group_col' must have the same length of unique values in 'group'.")
    }
  }
  comp <- sort(comp)
  if (is.null(xlab)) {
    xlab <- paste0(
      colnames(x)[comp[1]], " (",
      prettyNum(attr(x, "R2")[comp[1]] * 100, digits = 4), "%)"
    )
  }
  if (is.null(ylab)) {
    ylab <- paste0(
      colnames(x)[comp[2]], " (",
      prettyNum(attr(x, "R2")[comp[2]] * 100, digits = 4), "%)"
    )
  }
  poplin_scoreplot.default(x = x, comp = comp, group = group,
                           group_col = group_col, label = label,
                           xlab = xlab, ylab = ylab, ...)
}

##' @rdname poplin_scoreplot
##' @export
poplin_scoreplot.poplin.plsda <- function(x, comp = c(1, 2),
                                          group = attr(x, "Y.observed"),
                                          group_col = NULL, label = FALSE, 
                                          xlab = NULL, ylab = NULL, ...) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose only two components within 1:ncol(x).")
  }
  if (is.null(colnames(x))) {
    stop("colnames(x) must be non-NULL.")
  }
  if (!is.null(group_col)) {
    if (length(group_col) != length(unique(group))) {
      stop("'group_col' must have the same length of ",
           "unique values in 'group'.")
    }
  }
  comp <- sort(comp)
  if (is.null(xlab)) {
    xlab <- paste0(
      colnames(x)[comp[1]], " (",
      prettyNum(attr(x, "explvar")[comp[1]], digits = 4), "%)")
  }
  if (is.null(ylab)) {
    ylab <- paste0(
      colnames(x)[comp[2]], " (",
      prettyNum(attr(x, "explvar")[comp[2]], digits = 4), "%)")
  }
  poplin_scoreplot.default(x = x, comp = comp, group = group,
                           group_col = group_col, label = label,
                           xlab = xlab, ylab = ylab, ...)
}

##' @rdname poplin_scoreplot
##' @export
poplin_scoreplot.poplin <- function(x, poplin_in, comp = 1:2,
                                    group, group_col = NULL, label = FALSE,
                                    xlab = NULL, ylab = NULL, ...) {
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
  poplin_scoreplot(m, comp = comp, group = group, group_col = group_col,
                   label = label, xlab = xlab, ylab = ylab, ...)
}
