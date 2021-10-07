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
## plot_na <- function(x, grid_gap = 1, colors = c("gray80", "gray20"),
##                     showticklabels = c(TRUE, FALSE),
##                     ...) {

##   p <- heatmaply(is.na10(x), grid_gap = grid_gap, colors = colors,
##                  showticklabels = showticklabels,
##                  hide_colorbar = TRUE, return_ppxpy = TRUE, ...)
##   browser()
##   ## heatmaply:::arrange_plots(plots = p)
##   ## temporary issue in arrange_plots
##   .arrange_plots(plots = p, hide_colorbar = TRUE)
## }

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


