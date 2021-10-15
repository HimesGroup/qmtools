##' @importFrom stats dist hclust cutree
##' @export
.poplin_annotate <- function(x, poplin_in, mzvar, rtvar, h, ref_samples,
                             show_dendro = FALSE, ...) {
  if (!requireNamespace("InterpretMSSpectrum", quietly = TRUE)) {
    stop("Package 'InterpretMSSpectrum' is required. ",
         "Please install and try again.")
  }
  ## Grouping peaks based on their retention time using hierarchical clustering
  rt <- rowData(x)[, rtvar]
  m <- dist(rt, method = "manhattan") # use manhattan to make interpretable
  fit <- hclust(m)
  if (show_dendro) {
    return(plot(fit))
  }
  clus_idx <- cutree(fit, h = h)
  rowData(x)$rtg <- clus_idx
  if (missing(poplin_in)) {
    int <- assay(x, "raw")
  } else {
    int <- .verify_and_extract_input(x, poplin_in)
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
    } else {
      ## int <- int[, ref_samples, drop = FALSE]
      ## int <- cbind(int, int_med = apply(int, 1, median, na.rm = TRUE))
      int <- apply(int[, ref_samples, drop = FALSE], 1, median, na.rm = TRUE)
    }
  }

  mz <- rowData(x)[, mzvar]
  d <- data.frame(mz = mz, intensity = int, rt = rt, rtg = clus_idx)
  dl <- split(d, d$rtg)
}

.poplin_findmain <- function(m, ...) {
  if (anyNA(m[, 2])) { ## second column is supposed to be 'intensity'
  } 
  m_sub <- m[, c("mz", "int")] ## must specify columns to return findmain score (don't know why...)
  res <- InterpretMSSpectrum::findMAIN(m, ...)
}
