##' Score plot of dimension-reduced data
##'
##' Function to visualize dimension-reduced data matrices mainly produced by
##' [reduceFeatures], including `reduced.pca`, `reduced.tsne`, and
##' `reduced.plsda` objects (or a matrix with the same structure).
##'
##' @param x A matrix containing the dimension-reduced data typically produced
##'   by [reduceFeatures].
##' @param comp A numeric vector of length 2 specifying the components to
##'   display.
##' @param biplot A logical specifying whether visualize an overlay of scores
##'   and loadings. Ignored if `x`is not a `reduced.pca` or `reduced.plsda`
##'   object.
##' @param group A discrete variable to visualize the grouping structure. For a
##'   `reduced.plsda` object, group information used to fit the PLS-DA is used
##'   if not specified.
##' @param group_col A vector of colors with the same length of unique values in
##'   \code{group}.
##' @param point_size A numeric value specifying the size of points.
##' @param point_shape_by_group A logical specifying whether each group has
##'   different shapes of data points. Also can be a numeric vector with the
##'   same length of unique values in \code{group} to manually set point shapes.
##' @param label A logical specifying whether score labels are shown instead of
##'   points.
##' @param label_size A numeric value controlling the size of labels.
##' @param label_subset A character vector specifying a subset of score labels
##'   to display.
##' @param ellipse A logical specifying whether data ellipses are shown.
##' @param xlab A string specifying the title of x-axis.
##' @param ylab A string specifying the title of y-axis.
##' @param title A string specifying the main title of the plot.
##' @param legend A logical specifying whether the plot legend is shown.
##' @param arrow_len A numeric value specifying the length of arrow head.
##' @param arrow_col A string specifying the color of arrows.
##' @param arrow_alpha A numeric value specifying the transparency of arrow.
##' @param arrow_label A logical specifying whether text labels for arrows are
##'   shown.
##' @param arrow_label_ext A numeric value specifying the scalar extension for
##'   arrow labels.
##' @param arrow_label_size A numeric value specifying the size of arrow labels.
##' @param arrow_label_col A string specifying the color of arrow labels.
##' @param arrow_label_subset A character vector specifying a subset of arrow
##'   labels to display.
##'
##' @return A ggplot object.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' ## Sample group
##' g <- colData(faahko_se)$sample_group
##'
##' ## PCA
##' pca_res <- reduceFeatures(faahko_se, i = "knn_vsn", method = "pca")
##'
##' ## Visualizes the result
##' plotReduced(pca_res, group = g)
##' plotReduced(pca_res, group = g, label = TRUE, ellipse = TRUE)
##'
##' @export
plotReduced <- function(x, comp = c(1, 2), biplot = FALSE,
                        group, group_col = NULL,
                        point_size = 1.5,
                        point_shape_by_group = FALSE,
                        label = FALSE, label_size = 3.88,
                        label_subset = NULL,
                        ellipse = FALSE,
                        xlab = NULL, ylab = NULL,
                        title = NULL, legend = TRUE,
                        arrow_len = 0.2, arrow_col = "orange",
                        arrow_alpha = 0.3,
                        arrow_label = TRUE, arrow_label_ext = 1.05,
                        arrow_label_size = 3.88,
                        arrow_label_col = "orange",
                        arrow_label_subset = NULL) {
    comp <- sort(comp)
    if (inherits(x, "reduced.pca")) {
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
        if (!biplot) {
            .scoreplot(x, comp = comp, group = group,
                       group_col = group_col,
                       point_shape_by_group = point_shape_by_group,
                       point_size = point_size,
                       label = label, label_size = label_size,
                       label_subset = label_subset,
                       ellipse = ellipse,
                       xlab = xlab, ylab = ylab,
                       title = title, legend = legend)
        } else {
            n <- nrow(x)
            lam <- attr(x, "sdev")[comp] * sqrt(n)
            ## Consider Scaling factor
            ## if (scale < 0 || scale > 1)
            ##   warning("'scale' is outside [0, 1]")
            ## if (scale != 0)
            ##   lam <- lam**scale
            ## else lam <- 1
            X <- t(t(x[, comp]) / lam)
            Y <- t(t(attr(x, "loadings")[, comp]) * lam)
            .biplot(x = X, y = Y, comp = c(1, 2), group = group,
                    group_col = group_col,
                    point_size = point_size,
                    point_shape_by_group = point_shape_by_group,
                    label = label, label_size = label_size,
                    label_subset = label_subset,
                    ellipse = ellipse,
                    xlab = xlab, ylab = ylab,
                    title = title, legend = legend,
                    arrow_len = arrow_len, arrow_col = arrow_col,
                    arrow_alpha = arrow_alpha,
                    arrow_label = arrow_label,
                    arrow_label_ext = arrow_label_ext,
                    arrow_label_size = arrow_label_size,
                    arrow_label_col = arrow_label_col,
                    arrow_label_subset = arrow_label_subset)
        }
    } else if (inherits(x, "reduced.plsda")) {
        if (missing(group)) {
            group <- attr(x, "Y.observed")
        }
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
        if (!biplot) {
            .scoreplot(x, comp = comp, group = group,
                       group_col = group_col,
                       point_shape_by_group = point_shape_by_group,
                       point_size = point_size,
                       label = label, label_size = label_size,
                       label_subset = label_subset,
                       ellipse = ellipse,
                       xlab = xlab, ylab = ylab,
                       title = title, legend = legend)
        } else {
            X <- x[, comp]
            Y <- attr(x, "loadings")[, comp]
            .biplot(x = X, y = Y, comp = c(1, 2), group = group,
                    group_col = group_col,
                    point_size = point_size,
                    point_shape_by_group = point_shape_by_group,
                    label = label, label_size = label_size,
                    label_subset = label_subset,
                    ellipse = ellipse,
                    xlab = xlab, ylab = ylab,
                    title = title, legend = legend,
                    arrow_len = arrow_len, arrow_col = arrow_col,
                    arrow_alpha = arrow_alpha,
                    arrow_label = arrow_label,
                    arrow_label_ext = arrow_label_ext,
                    arrow_label_size = arrow_label_size,
                    arrow_label_col = arrow_label_col,
                    arrow_label_subset = arrow_label_subset)
        }
    } else {
        .scoreplot(x, comp = comp, group = group,
                   group_col = group_col,
                   point_shape_by_group = point_shape_by_group,
                   point_size = point_size,
                   label = label, label_size = label_size,
                   label_subset = label_subset,
                   ellipse = ellipse,
                   xlab = xlab, ylab = ylab,
                   title = title, legend = legend)
    }
}

.scoreplot <- function(x, comp = c(1, 2), group,
                       group_col = NULL,
                       point_size = 1.5,
                       point_shape_by_group = FALSE,
                       label = FALSE, label_size = 3.88,
                       label_subset = NULL,
                       ellipse = FALSE,
                       xlab = NULL, ylab = NULL,
                       title = NULL, legend = TRUE) {
    if (max(comp) > ncol(x) || length(comp) != 2) {
        stop("Choose two components between 1 and ", ncol(x), ".")
    }
    if (!is.null(group_col)) {
        if (!missing(group) && length(group_col) != length(unique(group))) {
            stop("'group_col' must have the same length of ",
                 "unique values in 'group'.")
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
                    stop(
                        "non-logical values of 'point_shape_by_group' must ",
                        "have the same length of unique values in 'group'."
                    )
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
                point_label <- ifelse(rownames(x) %in% label_subset,
                                      rownames(x), "")
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

.biplot <- function(x, y, comp = c(1, 2), group,
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
                    arrow_label_subset = NULL) {
    p <- .scoreplot(x, comp = comp, group = group,
                    group_col = group_col,
                    point_shape_by_group = point_shape_by_group,
                    point_size = point_size,
                    label = label, label_size = label_size,
                    label_subset = label_subset,
                    ellipse = ellipse,
                    xlab = xlab, ylab = ylab,
                    title = title, legend = legend)
    scalers <- .get_loading_scalers(x, y, comp)
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
                     data = y, aes(x = !!quote(x_adj), y = !!quote(y_adj),
                                   label = label),
                     col = arrow_label_col, size = arrow_label_size
                 )
    }
    p
}

.get_loading_scalers <- function(x, y, comp, scalefactor = 0.7) {
    x_ranges <- c(
    (max(x[, comp[1]]) - min(x[, comp[1]])),
    (max(x[, comp[2]]) - min(x[, comp[2]]))
    )
    y_ranges <- c(
    (max(y[, comp[1]]) - min(y[, comp[1]])),
    (max(y[, comp[2]]) - min(y[, comp[2]]))
    )
    scalefactor * c(x_ranges[1] / y_ranges[1], x_ranges[2] / y_ranges[2])
}

