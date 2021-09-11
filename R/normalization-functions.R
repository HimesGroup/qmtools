.pqn <- function(x, dat_in, dat_out, ref_ids = NULL,
                 ref_min_frac = 0, type = c("mean", "median")) {
  dat_m <- .verify_and_exract_input(x, dat_in)
  if (is.null(ref_ids)) {
    ref_m <- dat_m
  } else {
    if (!all(ref_ids %in% colnames(x))) {
      non_match <- setdiff(ref_ids, colnames(x))
      stop("Reference samples not found in colnames(x): ",
           non_match, call. = FALSE)
    } else {
      idx <- which(colnames(x) %in% ref_ids)
      ref_m <- dat_m[ , idx, drop = FALSE]
    }
  }
  idx_to_keep <- .idx_to_keep_by_missing(ref_m, "feature", ref_min_frac)
  ref_m_sub <- ref_m[idx_to_keep, , drop = FALSE]
  dat_m_sub <- dat_m[idx_to_keep, , drop = FALSE]
  ref_summary <- .mat_stats(ref_m_sub, margin = 1, type = type)
  quotients <- apply(dat_m_sub, 2, function(x) x / ref_summary)
  medians <- .mat_stats(quotients, margin = 2, type = "median")
  poplin_data(x, dat_out) <- sweep(dat_m, 2, medians, FUN = "/")
  x
}

.idx_to_keep_by_missing <- function(m, margin = c("sample", "feature"),
                                    min_frac) {
  margin <- match.arg(margin)
  if (margin == "sample") {
    non_missing_frac <- colSums(!is.na(m)) / nrow(m)
    which(non_missing_frac >= min_frac)
  } else {
    non_missing_frac <- rowSums(!is.na(m)) / ncol(m)
    which(non_missing_frac >= min_frac)
  }
}

.mat_stats <- function(m, margin, type = c("mean", "median")) {
  type <- match.arg(type)
  switch(
    type,
    mean = apply(m, margin, mean, na.rm = TRUE),
    median = apply(m, margin, median, na.rm = TRUE)
  )
}
