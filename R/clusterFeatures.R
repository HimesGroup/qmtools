##' Feature clustering
##'
##' Function to cluster LC-MS features according to their retention time and
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
##' The retention time-based grouping is performed using either a hierarchical
##' clustering via [hclust] or the methods available in the \pkg{MsFeatures}
##' package via [MsFeatures::groupClosest] and [MsFeatures::groupConsecutive].
##' For the \code{rt_grouping} = "hclust", by default, complete-linkage
##' clustering is conducted using the Manhattan distance (i.e., difference in
##' retention times) where the distance between two clusters is defined as the
##' difference in retention times between the farthest pair of elements in the
##' two clusters. Group memberships are assigned by specifying the cut height
##' for the distance metric. Other linkage methods can be specified with
##' \code{hclust_linkage}. Please refer to \code{?hclust} for details. For the
##' "closest" and "consecutive", please refer to
##' \code{?MsFeatures::groupClosest} and \code{?MsFeatures::groupConsecutive}
##' for the details of algorithms.
##'
##' For the correlation-based grouping, \code{cor_grouping} = "connected"
##' creates a undirected graph using feature correlations as an adjacency matrix
##' (i.e., correlations serve as edge weights). The edges whose weights are
##' below the cut-off specified by `cor_cut` will be removed from the graph,
##' separating features into several disconnected subgroups. Features in the
##' same subgroup will be assigned to the same feature cluster. For the
##' "louvain", the function further applies the Louvain algorithm to the graph
##' in order to identify densely connected features via
##' [igraph::cluster_louvain]. For the "SimilarityMatrix",
##' [MsFeatures::groupSimilarityMatrix] is used for feature grouping. Please
##' refer to \code{?MsFeatures::groupSimilarityMatrix} for the details of
##' algorithm.
##'
##' @param x A \linkS4class{SummarizedExperiment} object.
##' @param i A string or integer value specifying which assay values to use.
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
##' @param log2 A logical specifying whether feature intensities need to be
##'   log2-transformed before calculating a correlation matrix.
##' @param hclust_linkage A string specifying the linkage method to be used when
##'   \code{rt_grouping} is "hclust".
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
##' @seealso
##'
##' See [hclust], [cutree], [MsFeatures::groupClosest],
##' [MsFeatures::groupConsecutive], [MsFeatures::groupSimilarityMatrix], and
##' [igraph::cluster_louvain] for the underlying functions that do work.
##'
##' See [plotRTgroup] to visualize the grouping result.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' se <- clusterFeatures(faahko_se, i = "knn_vsn", rtime_var = "rtmed")
##' rowData(se)[, c("rtmed", "rtime_group", "feature_group")]
##'
##' @export
clusterFeatures <- function(x, i, rtime_var = "rtime",
                            rt_cut = 10, cor_cut = 0.7,
                            rt_grouping = c("hclust", "closest", "consecutive"),
                            cor_grouping = c("louvain", "SimilarityMatrix",
                                             "connected", "none"),
                            cor_use = c("everything", "all.obs",
                                        "complete.obs", "na.or.complete",
                                        "pairwise.complete.obs"),
                            cor_method = c("pearson", "kendall", "spearman"),
                            log2 = FALSE,
                            hclust_linkage = "complete") {
    if (!is(x, "SummarizedExperiment")) {
        stop("`x` must be a SummarizedExperiment object.")
    }
    if (!any(colnames(rowData(x)) == rtime_var)) {
        stop("Variable `", rtime_var, "` is not found in `rowData(x)`." )
    }
    rt_grouping <- match.arg(rt_grouping)
    cor_grouping <- match.arg(cor_grouping)
    rts <- rowData(x)[, rtime_var]
    rt_clus <- .groupRT(rts, method = rt_grouping, cut = rt_cut,
                        hclust_linkage = hclust_linkage)
    rt_clus <- .format_padding(rt_clus)
    rtime_group <- factor(paste0("FG.", rt_clus),
                          levels = unique(paste0("FG.", rt_clus)))
    rowData(x)$rtime_group <- rtime_group

    if (cor_grouping == "none") {
        x
    } else {
        m <- assay(x, i)
        if (log2) {
            m <- log2(m)
        }
        ml <- split.data.frame(m, rtime_group)
        res <- lapply(ml, function(x) {
            .groupCorr(x, use = cor_use, method = cor_method,
                       type = cor_grouping, cut = cor_cut)
        })
        cor_clus <- unsplit(res, f = rtime_group)
        cor_clus <- .format_padding(cor_clus)
        rowData(x)$feature_group <- paste(rtime_group, cor_clus, sep = ".")
        x
    }
}


.groupRT <- function(x, method = c("hclust", "closest", "consecutive"),
                     cut = 10, hclust_linkage = "complete") {
    method <- match.arg(method)
    if (anyNA(x)) {
        stop("`x` must have no missing values.")
    }
    switch(
        method,
        hclust = cutree(hclust(dist(x, "manhattan"), hclust_linkage), h = cut),
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

.groupCorr <- function(x, use = c("everything", "all.obs",
                                  "complete.obs", "na.or.complete",
                                  "pairwise.complete.obs"),
                       method = c("pearson", "kendall", "spearman"),
                       type = c("louvain", "connected", "SimilarityMatrix"),
                       cut = 0.7
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
        connected = {
            g <- igraph::graph_from_adjacency_matrix(
                             m, mode = "lower", weighted = TRUE, diag = FALSE
                         )
            g <- igraph::delete_edges(g, which(igraph::E(g)$weight < cut))
            igraph::components(g)$membership
        },
        louvain = {
            g <- igraph::graph_from_adjacency_matrix(
                             m, mode = "lower", weighted = TRUE, diag = FALSE
                         )
            g <- igraph::delete_edges(g, which(igraph::E(g)$weight < cut))
            res <- igraph::cluster_louvain(g)
            igraph::membership(res)
        },
        SimilarityMatrix = {
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
