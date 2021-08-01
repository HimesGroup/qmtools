.replace_empty_names <- function(names, N, msg, name_pattern) {
  if (is.null(names) && N > 0) {
    warning("'", msg, "' is NULL, replacing with '", name_pattern, "'")
    names <- paste0(name_pattern, seq_len(N))
  } else if (any(empty <- names=="")) {
    warning("'", msg, "' contains empty strings, replacing with '",
            name_pattern, "'")
    names[empty] <- paste0(name_pattern, seq_len(sum(empty)))
    ## names[empty] <- paste0(name_pattern, seq_along(sum(empty)))
  }
  names
}

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
  for (i in seq_len(nc)) {
    tmp[[i]] <- tmp[[i]][, j]
  }
  tmp
}
