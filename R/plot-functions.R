##' @export
##' @importFrom ggplot2 ggplot aes aes_string geom_point geom_text stat_ellipse
##' @importFrom ggplot2 xlab ylab ggtitle theme_bw theme element_blank
plot_reduced <- function(x, ...) {
  UseMethod("plot_reduced")
}

##' @export
plot_reduced.default <- function(x, comp = c(1, 2), group,
                                label = FALSE, ellipse = FALSE,
                                title = NULL, legend = TRUE) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose two components within 1:ncol(x).")
  }
  comp <- sort(comp)
  x <- as.data.frame(x)

  if (is.null(colnames(x))) {
    stop("colnames(x) must be non-NULL.")
  } else {
    cols <- colnames(x)[comp]
  }
  if (missing(group)) {
    p <- ggplot(x, aes_string(x = cols[1], y = cols[2])) +
      geom_point()
  } else {
    x$group <- factor(group, levels = unique(group))
    p <- ggplot(x, aes_string(x = cols[1], y = cols[2], group = "group",
                              col = "group", fill = "group")) +
      geom_point()
  }
  if (ellipse) {
    p <- p + stat_ellipse(geom = "polygon", alpha = 0.1)
  }
  if (label) {
    if (is.null(rownames(x))) {
      stop("rownames(x) must be non-NULL if label = TRUE.")
    } else {
      p <- p + geom_text(aes(label = rownames(x)), show.legend = FALSE)
    }
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (legend) {
    p + theme_bw() + theme(legend.title = element_blank())
  } else {
    p + theme_bw() + theme(legend.position = "none")
  }
}

##' @export
plot_reduced.poplin.matrix.pca <- function(x, comp = c(1, 2), group,
                                                  label = FALSE, ellipse = FALSE,
                                                  title = NULL, legend = TRUE) {
  if (max(comp) > ncol(x) || length(comp) != 2) {
    stop("Choose two components within 1:ncol(x).")
  }
  comp <- sort(comp)
  R2 <- attr(x, "R2") # extract before conversion
  x <- as.data.frame(x)

  if (missing(group)) {
    p <- ggplot(x, aes(x = PC1, y = PC2)) +
      geom_point()
  } else {
    x$group <- factor(group, levels = unique(group))
    p <- ggplot(x, aes(x = PC1, y = PC2, group = group, col = group,
                       fill = group)) +
      geom_point()
  }
  if (ellipse) {
    p <- p + stat_ellipse(geom = "polygon", alpha = 0.1)
  }
  if (label) {
    if (is.null(rownames(x))) {
      stop("rownames(x) must be non-NULL if label = TRUE.")
    } else {
      p <- p + geom_text(aes(label = rownames(x)), show.legend = FALSE)
    }
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  x_var_exp <- R2[comp[1]] * 100
  y_var_exp <- R2[comp[2]] * 100
  xlabel <- paste0(colnames(imp)[comp[1]], " (",
                   prettyNum(x_var_exp, digits = 4), "%)")
  ylabel <- paste0(colnames(imp)[comp[1]], " (",
                   prettyNum(y_var_exp, digits = 4), "%)")
  p <- p + xlab(xlabel) + ylab(ylabel) + theme_bw()
  if (legend) {
    p + theme(legend.title = element_blank())
  } else {
    p + theme(legend.position = "none")
  }
}

##' @export
##' @importFrom stats reshape
##' @importFrom ggplot2 geom_boxplot geom_violin
plot_box <- function(x, group, log2 = FALSE, violin = FALSE) {
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
    ylab("Intensity") +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank())
}

##' @export
##' @importFrom heatmaply ggheatmap is.na10 heatmaply
plot_na <- function(x, grid_gap = 1, colors = c("gray80", "gray20"),
                    showticklabels = c(TRUE, FALSE),
                    ...) {

  p <- heatmaply(is.na10(x), grid_gap = grid_gap, colors = colors,
                 showticklabels = showticklabels,
                 hide_colorbar = TRUE, return_ppxpy = TRUE, ...)
  ## heatmaply:::arrange_plots(plots = p)
  ## temporary issue in arrange_plots
  .arrange_plots(plots = p, hide_colorbar = TRUE)
}

.arrange_plots <- function(
    plots, 
    row_dend_left = FALSE, hide_colorbar = TRUE) {

  plots <- plots[!sapply(plots, is.null)]
  if (!row_dend_left) {
    plots$p <- plots$p + theme(legend.position = "left")
  }
  if (hide_colorbar) {
    plots$p <- plots$p + theme(legend.position = "none")
  }
  plots <- lapply(plots, function(x) {
    x + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "npc"))
  })

  column_list <- list(plots$py, plots$pc, plots$p)
  ind_null_col <- sapply(column_list, is.null)

  row1_list <- list(plots$py, heatmaply:::ggplot_empty(), heatmaply:::ggplot_empty())
  row2_list <- list(plots$pc, heatmaply:::ggplot_empty(), heatmaply:::ggplot_empty())
  row3_list <- list(plots$p, plots$pr, plots$px)

  if (row_dend_left) {
    row3_list <- rev(row3_list)
    row2_list <- rev(row2_list)
    row1_list <- rev(row1_list)
  }
  plotlist <- c(
    row1_list,
    row2_list,
    row3_list
  )

  nrows <- sum(!ind_null_col)
  ind_remove_col <- rep(ind_null_col, each = length(plotlist) / 3)

  ind_null_row <- sapply(row3_list, is.null)
  ncols <- sum(!ind_null_row)
  ind_remove_row <- rep(ind_null_row, length.out = length(plotlist))
  plotlist <- plotlist[!(ind_remove_row | ind_remove_col)]

  egg::ggarrange(
    plots = plotlist,
    ncol = ncols,
    widths = .default_dims(plots$px, plots$pr),
    heights = rev(.default_dims(plots$py, plots$pc))
  )
}

.default_dims <- function(px, pr) {
  if (!is.null(px)) {
    if (is.null(pr)) {
      widths <- c(0.8, 0.2)
    } else {
      widths <- c(0.7, 0.1, 0.2)
    }
  } else {
    if (is.null(pr)) {
      widths <- 1
    } else {
      widths <- c(0.9, 0.1)
    }
  }
  widths
}

##' @export
plot_cor <- function(x,
                     use = c("everything", "all.obs", "complete.obs",
                             "na.or.complete", "pairwise.complete.obs"),
                     method = c("pearson", "kendall", "spearman"),
                     showticklabels = c(TRUE, TRUE), ...) {
  use <- match.arg(use)
  method <- match.arg(method)
  m <- cor(x, use = use, method = method)
  ggheatmap(m, showticklabels = showticklabels, ...)
}
