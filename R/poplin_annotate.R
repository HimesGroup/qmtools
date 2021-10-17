##' @importFrom stats dist hclust cutree
##' @export
.poplin_annotate <- function(x, poplin_in, mz_var, rt_var, h, ref_samples = NULL,
                             show_dendro = FALSE,
                             cor_method = c("pearson", "spearman"),
                             cor_cutoff = 0.6, ...) {
  if (!requireNamespace("InterpretMSSpectrum", quietly = TRUE)) {
    stop("Package 'InterpretMSSpectrum' is required. ",
         "Please install and try again.")
  }
  ## Grouping peaks based on their retention time using hierarchical clustering
  rt <- rowData(x)[, rt_var]
  m <- dist(rt, method = "manhattan") # use manhattan to make interpretable
  fit <- hclust(m)
  if (show_dendro) {
    return(plot(fit))
  }
  rt_group <- cutree(fit, h = h)
  rowData(x)$rt_group <- rt_group
  if (missing(poplin_in)) {
    int_mat <- assay(x, "raw")
  } else {
    int_mat <- .verify_and_extract_input(x, poplin_in)
  }
  int_mat <- poplin_impute(int_mat, method = "halfmin") # temporary imputation

  if (is.null(ref_samples)) {
    ref_samples <- colnames(int_mat)
  }
  if (!(is.character(ref_samples) || is.numeric(ref_samples))) {
    stop ("'ref_samples' must be a vector of character or integer.")
  } else {
    if (is.character(ref_samples) &&
        !(all(ref_samples %in% colnames(x)))) {
      non_match <- setdiff(ref_samples, colnames(x))
      stop("Reference samples not found in colnames(x): ",
           non_match, call. = FALSE)
    } else if (is.numeric(ref_samples) &&
            !(all(ref_samples >= 1 & ref_samples <= ncol(x)))) {
      stop("Subscript out of bound. 'ref_samples' must be within [1, ncol(x)].")
    }
  }
  int_mat <- int_mat[, ref_samples, drop = FALSE]
  int_med <- apply(int_mat, 1, median, na.rm = TRUE)

  if (cor_cutoff < 1) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("Package 'igraph' is required. ",
           "Please install and try again.")
    }
    if (length(ref_samples) < 5) {
      stop("must have > 4 samples to calculate correlations of intensities ",
           "across samples.")
    }
    cor_method <- match.arg(cor_method)
    max_rt_group <- max(rt_group)
    graph_res <- list()
    for (i in seq_len(max_rt_group)) {
      row_idx <- which(rt_group == i)
      if (length(row_idx) > 1) {
        m <- int_mat[row_idx, ]
        graph_group <- .cor_igraph(m, cor_method = cor_method,
                                 cor_cutoff = cor_cutoff)
        d <- data.frame(id = names(graph_group), rt_group = i,
                        graph_group = graph_group)
      } else {
        d <- data.frame(id = rownames(x)[row_idx], rt_group = i, graph_group = 1,
                        row.names = rownames(x)[row_idx])
      }
      d$cor_group <- paste0(d$rt_group, "_", d$graph_group)
      graph_res[[i]] <- d
    }
    graph_res <- do.call(rbind, graph_res)
    graph_res <- with(graph_res, graph_res[order(rt_group, graph_group), ])
    group_rle <- rle(graph_res$cor_group)
    graph_res$feature_group <- rep(seq_along(group_rle$lengths), group_rle$lengths)
  }
  graph_res
  row_dat <- rowData(x)
  row_dat$tmp_key_x <- rownames(x)
  row_dat <- merge(row_dat, graph_res,
                   by.x = c("tmp_key_x", "rt_group"),
                   by.y = c("id", "rt_group"))
  row_dat$int_med <- int_med

  row_datlist <- split(row_dat, row_dat$feature_group)
  lapply(row_datlist, function(y) {
    y_sub <- y[, c(mz_var, "int_med")]
    colnames(y_sub) <- c("mz", "intensity")
    InterpretMSSpectrum::findMAIN(y_sub, ...)
  })
}

.cor_igraph <- function(m, cor_method, cor_cutoff) {
  cor_m <- stats::cor(t(m), method = cor_method)
  g <- igraph::graph.adjacency(cor_m, mod = "lower", weighted = TRUE,
                               diag = FALSE)
  g <- igraph::delete.edges(g, which(E(g)$weight < cor_cutoff))
  igraph::components(g)$membership
}

.do_findmain <- function(m, mz_var, ...) {
  m_sub <- m[, c(mz_var, "int_med")] ## must specify columns to return findmain score (don't know why...)
  colnames(m_sub) <- c("mz", "intensity")
  InterpretMSSpectrum::findMAIN(m_sub, ...)
}

