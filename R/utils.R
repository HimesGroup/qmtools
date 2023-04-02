##' @importClassesFrom SummarizedExperiment SummarizedExperiment
##' @importMethodsFrom SummarizedExperiment assay assay<-
##' @importMethodsFrom SummarizedExperiment rowData rowData<- colData colData<-
##' @importFrom grDevices dev.flush dev.hold
##' @importFrom graphics Axis box mtext par points rect strwidth text
##' @importFrom methods is
##' @importFrom stats cor cutree dist fitted hclust median model.matrix prcomp
##' @importFrom stats predict residuals sd reshape na.omit mad
##' @importFrom ggplot2 ggplot aes geom_point geom_text stat_ellipse
##' @importFrom ggplot2 xlab ylab ggtitle theme_bw theme element_blank
##' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_shape_manual
##' @importFrom ggplot2 geom_segment scale_x_continuous scale_y_continuous
##' @importFrom ggplot2 sec_axis geom_boxplot geom_violin element_text
##' @importFrom patchwork plot_layout
##' @importFrom stats as.formula
##' @importFrom limma normalizeCyclicLoess lmFit eBayes topTable makeContrasts
##'   contrasts.fit

.verify_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Package '", pkg, "' is required. Please install and try again.")
    }
}

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}
