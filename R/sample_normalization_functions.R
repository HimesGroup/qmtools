.pqn <- function(x, ref_ids, ref_min_frac = 0, type = c("mean", "median")) {
  ## temporarily put assay(x)
  ## need to use poplinData with pre-specified name
  dat_m <- assay(x)
  if (missing(ref_ids)) {
    ref_m <- dat_m
  } else {
    if (!all(ref_ids %in% colnames(x))) {
      non_match <- setdiff(ref_ids, colnames(x))
      stop("samples not found in colnames(x): ",
           non_match, call. = FALSE)
    } else {
      idx <- which(colnames(x) %in% sample)
      ref_m <- dat_m[ , idx]
    }
  }
  if (ref_min_frac > 0) {
    ref_m <- .filter_by_missing(m, "feature", min_frac = ref_min_frac)
  }
  ref_summary <- .row_stats(ref_m, type = type)
  quotients <- apply(dat_m, 2, function(x) x / ref_summary)
  medians <- colMedians(quotients, na.rm = TRUE)
  sweep(dat_m, 2, medians, FUN = "/")
}

.filter_by_missing <- function(m, margin = c("sample", "feature"), min_frac) {
  margin <- match.arg(margin)
  if (margin == "sample") {
    missing_frac <- colSums(!is.na(m)) / nrow(m)
    idx_to_keep <- which(missing_frac >= min_frac)
    return(m[, idx_to_keep])
  } else {
    missing_frac <- rowSums(!is.na(m)) / ncol(m)
    idx_to_keep <- which(missing_frac >= min_frac)
    return(m[idx_to_keep, ])
  }
}

.row_stats <- function(m, type = c("mean", "median")) {
  type <- match.arg(type)
  switch(
    type,
    mean = rowMeans2(m, na.rm = TRUE),
    median = rowMedians(m, na.rm = TRUE)
  )
}

