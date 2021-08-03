## See SummarizedExperiment vignettes (enabling subsetting operations)
## .SummarizedExperiment.charbound <- function (idx, txt, fmt)
## {
##   orig <- idx
##   idx <- match(idx, txt)
##   if (any(bad <- is.na(idx))) {
##     msg <- paste(S4Vectors:::selectSome(orig[bad]), collapse = " ")
##     stop(sprintf(fmt, msg))
##   }
##   idx
## }
.get_subset_index <- function(subset, names) {
  if (is.character(subset)) {
    fmt <- paste0("<", class(x), ">[i,] index out of bounds: %s")
    subset <- SummarizedExperiment:::.SummarizedExperiment.charbound(
                                       subset, names, fmt
                                     )
  }
  as.vector(subset)
}

.subset_columns <- function(x, j, get_slot, element) {
  tmp <- get_slot(x)[[element]]
  nc <- ncol(tmp)
  for (k in seq_len(nc)) {
    tmp[[k]] <- tmp[[k]][, j]
  }
  tmp
}

.replace_columns <- function(x, j, get_slot, element, value, i) {
  left <- get_slot(x)[[element]]
  right <- get_slot(value)[[element]]
  nc <- ncol(left)
  if (missing(i)) {
    for (k in seq_len(nc)) {
      left[[k]][, j] <- right[[k]]
    }
  } else {
    for (k in seq_len(nc)) {
      left[[k]][i, j] <- right[[k]]
    }
  }
  left
}

