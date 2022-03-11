##' Feature clustering
##'
##' Function to group LC-MS features according to their retention time and
##' intensity correlation across samples with a
##' \linkS4class{SummarizedExperiment}.
##'
##' For soft ionization methods (e.g., LC/ESI-MS) commonly used in metabolomics,
##' one or more ions could be generated from an individual compound upon
##' ionization. The redundancy of feature data needs to be addressed since we
##' typically interested in compounds rather than different ion species. This
##' function attempts to identify a group of features from the same compound
##' with the following steps:
##'
##' 1. Features are grouped by their retention times to identify co-eluting
##' compounds.
##'
##' 2. For each retention time-based group, features are further clustered by
##' patterns of the intensity correlations across samples to identify a subset
##' of features from the same compound.
##'
##' The retention time-based grouping is performed using a hierarchical
##' clustering via [hclust] or the methods available in the \pkg{MsFeatures}
##' package via [MsFeatures::groupClosest] and [MsFeatures::groupConsecutive].
##' For the \code{rt_grouping} = "hclust", complete-linkage clustering is
##' conducted using the Manhattan distance (i.e., difference in retention times)
##' where the distance between two clusters is defined as the difference in
##' retention times between the farthest pair of elements in the two clusters.
##' Group memberships are assigned by specifying the cut height for the distance
##' metric. For the "closest" and "consecutive", please refer to
##' \code{?MsFeatures::groupClosest} and \code{?MsFeatures::groupConsecutive}
##' for details.
##'
##' For the correlation-based grouping, \code{cor_grouping} = "louvain" creates
##' a undirected graph using feature correlations as an adjacency matrix.
##' Correlations below the cut-off specified by `cor_cut` will be ignored. The
##' Louvain algorithm is used to group features via [igraph::cluster_louvain].
##' For the "AbundanceSimilar", [MsFeatures::groupSimilarityMatrix] is used for
##' feature grouping. Please refer to \code{?MsFeatures::groupSimilarityMatrix}
##' for details.
##'
##' @param x A \linkS4class{SummarizedExperiment} object.
##' @param xin A string specifying which assay values to use.
##' @param rtime_var A string specifying the names of variable containing a
##'   numeric vector of retention times in `rowData(x)`.
##' @param rt_cut A numeric value specifying a cut-off for the retention-time
##'   based feature grouping.
##' @param cor_cut A numeric value specifying a cut-off for the
##'   correlation-based feature grouping.
##' @param rt_grouping A string specifying which method to use for the
##'   retention-time based feature grouping.
##' @param cor_grouping A string specifying which method to use for the
##'   correlation-based feature grouping.
##' @param cor_use A string specifying which method to compute correlations in
##'   the presence of missing values. Refer to \code{?cor} for details.
##' @param cor_method A string specifying which correlation coefficient is to be
##'   computed. See \code{?cor} for details.
##' @param log2 A logical specifying whether feature intensities needs to be
##'   log2-transformed before calculating a correlation matrix.
##' @return A \linkS4class{SummarizedExperiment} object with the grouping
##'   results added to columns "rtime_group" (initial grouping on retention
##'   times) and "feature_group" in its `rowData`.
##'
##' @references
##'
##' Johannes Rainer (2022). MsFeatures: Functionality for Mass Spectrometry
##' Features. R package version 1.3.0.
##' 'https://github.com/RforMassSpectrometry/MsFeatures
##'
##' Vincent D. Blondel, Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre:
##' Fast unfolding of communities in large networks. J. Stat. Mech. (2008)
##' P10008
##'
##' Csardi G, Nepusz T: The igraph software package for complex network
##' research, InterJournal, Complex Systems 1695. 2006. https://igraph.org
##'
##'
##' @seealso
##'
##' See [hclust], [cutree], [MsFeatures::groupClosest],
##' [MsFeatures::groupConsecutive], [MsFeatures::groupSimilarityMatrix], and
##' [igraph::cluster_louvain] for the underlying functions that do work.
##' 
##' 
##' @export
clusterFeatures <- function(x, xin, rtime_var = "rtime",
                            rt_cut = 10, cor_cut = 0.7,
                            rt_grouping = c("hclust", "closest", "consecutive"),
                            cor_grouping = c("louvain", "AbundanceSimilar",
                                             "none"),
                            cor_use = c("everything", "all.obs",
                                        "complete.obs", "na.or.complete",
                                        "pairwise.complete.obs"),
                            cor_method = c("pearson", "kendall", "spearman"),
                            log2 = FALSE) {
  if (!is(x, "SummarizedExperiment")) {
    stop("`x` must be a SummarizedExperiment object.")
  }
  if (!any(colnames(rowData(x)) == rtime_var)) {
    stop("Variable `", rtime_var, "` is not found in `rowData(x)`." )
  }
  rt_grouping <- match.arg(rt_grouping)
  cor_grouping <- match.arg(cor_grouping)
  rts <- rowData(x)[, rtime_var]
  rt_clus <- groupRT(rts, method = rt_grouping, cut = rt_cut)
  rt_clus <- .format_padding(rt_clus)
  rtime_group <- factor(paste0("FG.", rt_clus),
                        levels = unique(paste0("FG.", rt_clus)))
  rowData(x)$rtime_group <- rtime_group

  if (cor_grouping == "none") {
    x
  } else {
    m <- assay(x, xin)
    if (log2) {
      m <- log2(m)
    }
    ml <- split.data.frame(m, rtime_group)
    res <- lapply(ml, function(x) {
      groupCorr(x, use = cor_use, method = cor_method, type = cor_grouping,
                cut = cor_cut)
    })
    cor_clus <- unsplit(res, f = rtime_group)
    cor_clus <- .format_padding(cor_clus)
    rowData(x)$feature_group <- paste(rtime_group, cor_clus, sep = ".")
    x
  }
}

##' Helper to visualize feature groupings
##'
##' Visualizes feature grouping results produced by [clusterFeatures]. A
##' retention-time based feature group is displayed with its sub-groups based on
##' the feature intensity correlations either using a pair plot or graph.
##' Features with the same color indicate that they are in the same group.
##'
##' @param x A \linkS4class{SummarizedExperiment} object.
##' @param xin A string specifying which assay values to use. Choose the same
##'   value used in the feature grouping.
##' @param group A string specifying the label of retention time-based group to
##'   visualize.
##' @param type A string specifying which type of plots to visualize.
##' @param rtime_group_var A string specifying the names of variable containing
##'   the retention-time based grouping result in `rowData(x)`.
##' @param feature_group_var A string specifying the names of variable
##'   containing the final feature grouping result in `rowData(x)`.
##' @param cor_cut A numeric value specifying a cut-off for the visualizing
##'   correlations in a graph as edges. Ignored if type is "pairs".
##' @param cor_use A string specifying which method to compute correlations in
##'   the presence of missing values. Refer to \code{?cor} for details. Choose
##'   the same value used in the feature grouping. Ignored if type is "pairs".
##' @param cor_method A string specifying which correlation coefficient is to be
##'   computed. See \code{?cor} for details. Choose the same value used in the
##'   feature grouping. Ignored if type is "pairs".
##' @param log2 A logical specifying whether feature intensities needs to be
##'   log2-transformed before calculating a correlation matrix. Ignored if type
##'   is "pairs". Choose the same value used in the feature grouping.
##' @param A pair plot or graph.
##'
##' @seealso [clusterFeatures]
##' 
##' @export
plotRTgroup <- function(x, xin, group, type = c("graph", "pairs"),
                        rtime_group_var = "rtime_group",
                        feature_group_var = "feature_group",
                        cor_cut = 0.75,
                        cor_use = c("everything", "all.obs",
                                    "complete.obs", "na.or.complete",
                                    "pairwise.complete.obs"),
                        cor_method = c("pearson", "kendall", "spearman"),
                        log2 = FALSE) {
  ## Hard to show plots side by side as `pairs` internally reset mfrow
  if (!is(x, "SummarizedExperiment")) {
    stop("`x` must be a SummarizedExperiment object.")
  }
  if (!any(colnames(rowData(x)) == rtime_group_var)) {
    stop("Retention time group variable `", rtime_group_var,
         "` is not found in `rowData(x)`." )
  }
  if (!any(colnames(rowData(x)) == feature_group_var)) {
    stop("Feature group variable `", feature_group_var,
         "` is not found in `rowData(x)`." )
  }
  type <- match.arg(type)
  cor_use <- match.arg(cor_use)
  rg <- rowData(x)[, rtime_group_var]
  m <- assay(x, xin)
  idx <- which(rg == group)
  if (length(idx) == 0) {
    stop("Group `", group, "' is not found.")
  }
  if (length(idx) == 1) {
    stop("Group `", group, "' is a singleton cluster. ",
         "A plot cannot be created.")
  }
  m <- m[idx, , drop = FALSE]
  if (log2) {
    m <- log2(m)
  }
  fg <- rowData(x)[, feature_group_var]
  fg <- factor(fg[idx])
  cols <- scales::hue_pal()(length(unique(fg)))
  cols <- cols[fg]
  if (type == "graph") {
    co <- cor(t(m), use = cor_use, method = cor_method)
    g <- igraph::graph_from_adjacency_matrix(
                   co, mode = "lower", weighted = TRUE, diag = FALSE
                 )
    g <- igraph::delete_edges(g, which(igraph::E(g)$weight < cor_cut))
    plot(g, edge.label = prettyNum(igraph::E(g)$weight, digits = 2),
         vertex.color = cols)
  } else {
    ## This will fill background colors by group in combination with .pairs.diag
    .panel.diagcol <- function(x, i, ...) {   
      usr <- par("usr")
      on.exit(par(usr))
      rect(usr[1],usr[3],usr[2],usr[4], col = cols[i])
    }

    ## From pairs function examples
    .panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
      usr <- par("usr")
      on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      ## r <- abs(cor(x, y))
      r <- cor(x, y)
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
    }
    .pairs.diag(t(m), diag.panel = .panel.diagcol, upper.panel = .panel.cor,
                label.pos = 0.5)
  }
}


groupRT <- function(x, method = c("hclust", "closest", "consecutive"),
                    cut = 10) {
  method <- match.arg(method)
  if (anyNA(x)) {
    stop("`x` must have no missing values.")
  }
  switch(
    method,
    hclust = cutree(hclust(dist(x, "manhattan")), h = cut),
    closest = {
      .verify_package("MsFeatures")
      clus <- MsFeatures::groupClosest(x, maxDiff = cut)
      .relabel(clus)
    },
    consecutive = {
      .verify_package("MsFeatures")
      clus <- MsFeatures::groupConsecutive(x, maxDiff = cut)
      .relabel(clus)
    }
  )
}

groupCorr <- function(x, use = c("everything", "all.obs",
                                 "complete.obs", "na.or.complete",
                                 "pairwise.complete.obs"),
                      method = c("pearson", "kendall", "spearman"),
                      type = c("louvain", "AbundanceSimilar"),
                      cut = 0.75
                      ) {
  use <- match.arg(use)
  ## method <- match.arg(method) ## handled with cor function
  type <- match.arg(type)

  ## Calculate a correlation matrix
  m <- cor(t(x), use = use, method = method)
  if (anyNA(m)) {
    stop("A correlation matrix contains NA. ",
         "Please check `x` and consider imputation.")
  }

  switch(
    type,
    louvain = {
      g <- igraph::graph_from_adjacency_matrix(
                     m, mode = "lower", weighted = TRUE, diag = FALSE
                   )
      g <- igraph::delete_edges(g, which(igraph::E(g)$weight < cut))
      res <- igraph::cluster_louvain(g)
      igraph::membership(res)
    },
    AbundanceSimilar = {
      res <- MsFeatures::groupSimilarityMatrix(m, threshold = cut)
      .relabel(res)
    }
  )
}

.relabel <- function(x) {
  v <- rle(x)
  newc <- as.integer(factor(v$values, levels = unique(v$values)))
  rep(newc, times = v$lengths)
}

.format_padding <- function(x) {
  padding_width <- nchar(max(x))
  x <- formatC(x, width = padding_width, format = "d", flag = 0)
}

## One line modification to fill diagonal background colors by feature group 
.pairs.diag <- function (x, labels, panel = points, ..., horInd = 1:nc,
                         verInd = 1:nc, lower.panel = panel,
                         upper.panel = panel, diag.panel = NULL, 
                         text.panel = textPanel, label.pos = 0.5 + has.diag/3,
                         line.main = 3, cex.labels = NULL, font.labels = 1,
                         row1attop = TRUE, gap = 1,  log = "",
                         horOdd = !row1attop, verOdd = !row1attop) 
{
  if (doText <- missing(text.panel) || is.function(text.panel)) 
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) {
      text(x, y, txt, cex = cex, font = font)
    }
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, 
                        oma, ...) {
    xpd <- NA
    if (side%%2L == 1L && xl[j]) 
      xpd <- FALSE
    if (side%%2L == 0L && yl[i]) 
      xpd <- FALSE
    if (side%%2L == 1L) 
      Axis(x, side = side, xpd = xpd, ...)
    else Axis(y, side = side, xpd = xpd, ...)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) {
    plot(...)
  }
  localLowerPanel <- function(..., main, oma, font.main, cex.main) {
    lower.panel(...)
  }
  localUpperPanel <- function(..., main, oma, font.main, cex.main) {
    upper.panel(...)
  }
  localDiagPanel <- function(..., main, oma, font.main, cex.main) {
    diag.panel(...)
  }
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'pairs'")
    }
  }
  else if (!is.numeric(x)) 
    stop("non-numeric argument to 'pairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2L) 
    stop("only one column in the argument to 'pairs'")
  if (!all(1L <= horInd & horInd <= nc)) 
    stop("invalid argument 'horInd'")
  if (!all(1L <= verInd & verInd <= nc)) 
    stop("invalid argument 'verInd'")
  if (doText) {
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) 
        labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels)) 
      doText <- FALSE
  }
  oma <- if ("oma" %in% nmdots) 
           dots$oma
  main <- if ("main" %in% nmdots) 
            dots$main
  if (is.null(oma)) 
    oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
  opar <- par(mfcol = c(length(horInd), length(verInd)),
              mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  xl <- yl <- logical(nc)
  if (is.numeric(log)) 
    xl[log] <- yl[log] <- TRUE
  else {
    xl[] <- grepl("x", log)
    yl[] <- grepl("y", log)
  }
  ni <- length(iSet <- if (row1attop) horInd else rev(horInd))
  nj <- length(jSet <- verInd)
  for (j in jSet) {
    for (i in iSet) {
      l <- paste0(if (xl[j]) "x" else "",
                  if (yl[i]) "y" else "")
      localPlot(x[, j], x[, i], xlab = "", ylab = "",
                axes = FALSE, 
                type = "n", ..., log = l)
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box()
        j.odd <- (match(j, jSet) + horOdd)%%2L
        i.odd <- (match(i, iSet) + verOdd)%%2L
        if (i == iSet[1L] && (!j.odd || !has.upper || !has.lower)) 
          localAxis(3L, x[, j], x[, i], ...)
        if (i == iSet[ni] && (j.odd || !has.upper || !has.lower)) 
          localAxis(1L, x[, j], x[, i], ...)
        if (j == jSet[1L] && (!i.odd || !has.upper || !has.lower)) 
          localAxis(2L, x[, j], x[, i], ...)
        if (j == jSet[nj] && (i.odd || !has.upper || !has.lower)) 
          localAxis(4L, x[, j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) 
            ## Modified here
            localDiagPanel(as.vector(x[, i]), i, ...)
            ## localDiagPanel(as.vector(x[, i]), ...)
          if (doText) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            xlp <- if (xl[i]) 
                     10^0.5
                   else 0.5
            ylp <- if (yl[j]) 
                     10^label.pos
                   else label.pos
            text.panel(xlp, ylp, labels[i], cex = cex.labels, 
                       font = font.labels)
          }
        }
        else if (i < j) 
          localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        else localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        if (any(par("mfg") != mfg)) 
          stop("the 'panel' function made a new plot")
      }
      else par(new = FALSE)
    }
  }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
                   dots$font.main
                 else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
                  dots$cex.main
                else par("cex.main")
    mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, 
          font = font.main)
  }
  invisible(NULL)
}

