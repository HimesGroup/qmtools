## PQN normalization
## The reference suggests to apply integral normalization prior to PQN so
## consider to add that.
.pqn <- function(x, dat_in, dat_out, ref_ids = NULL,
                 min_frac = 0.5, type = c("mean", "median")) {
  dat_m <- .verify_and_exract_input(x, dat_in)
  ## unnecessary match.arg
  type <- match.arg(type)
  if (dat_out %in% assayNames(x)) {
    stop("'dat_out' must not be one of assayNames(x): ",
         assayNames(x))
  }
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
  idx_to_keep <- .idx_to_keep_by_missing(ref_m, "feature", min_frac)
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

## other spectral function normalization methods
##' @importFrom stats mad
.sample_norm <- function(x, normalizer = c("tic", "mean", "median",
                                           "mad", "euclidean"),
                         dat_in, dat_out, restrict = FALSE, rescale = FALSE) {
  dat_m <- .verify_and_exract_input(x, dat_in)
  if (dat_out %in% assayNames(x)) {
    stop("'dat_out' must not be one of assayNames(x): ",
         assayNames(x))
  }
  ## unnecessary match.arg
  normalizer <- match.arg(normalizer)
  if (restrict) {
    dat_m_sub <- na.omit(dat_m)
  } else {
    dat_m_sub <- dat_m
  }
  scale_factors <- switch(
    normalizer,
    tic = colSums(dat_m_sub, na.rm = TRUE),
    mean = colMeans(dat_m_sub, na.rm = TRUE),
    median = apply(dat_m_sub, 2, median, na.rm = TRUE),
    mad = apply(dat_m_sub, 2, mad, na.rm = TRUE),
    euclidean = apply(dat_m_sub, 2, function(x) sqrt(sum(x**2, na.rm = TRUE)))
    )
  if (rescale) {
    scale_factors <- scale_factors / median(scale_factors)
  }
  poplin_data(x, dat_out) <- sweep(dat_m, 2, scale_factors, FUN = "/")
  x
}
