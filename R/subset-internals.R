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
    fmt <- "index out of bounds: %s"
    subset <- SummarizedExperiment:::.SummarizedExperiment.charbound(
                                       subset, names, fmt
                                     )
  }
  as.vector(subset)
}

.subset_columns <- function(x, j, get_slot) {
  tmp <- get_slot(x)
  tmp_subsets <- lapply(tmp, function(x) x[, j, drop = FALSE])
  do.call(
    DataFrame,
    c(lapply(tmp_subsets, I), list(row.names=NULL, check.names=FALSE))
  )
  ## nc <- ncol(tmp)
  ## for (k in seq_len(nc)) {
  ##   tmp[[k]] <- tmp[[k]][, j, drop = FALSE]
  ## }
  ## tmp
}

.replace_columns <- function(x, j, get_slot, value, i) {
  left <- get_slot(x)
  right <- get_slot(value)
  if (missing(i)) {
    tmp_replaced <- mapply(
      FUN = function(x, y) {
        x[, j] <- y
        x
      }, left, right, SIMPLIFY = FALSE
    )
  } else {
    tmp_replaced <- mapply(
      FUN = function(x, y) {
        x[i, j] <- y
        x
      }, left, right, SIMPLIFY = FALSE
    )
  }
  do.call(
    DataFrame,
    c(lapply(tmp_replaced, I), list(row.names=NULL, check.names=FALSE))
  )
  ## nc <- ncol(left)
  ## if (missing(i)) {
  ##   for (k in seq_len(nc)) {
  ##     left[[k]][, j] <- right[[k]]
  ##   }
  ## } else {
  ##   for (k in seq_len(nc)) {
  ##     left[[k]][i, j] <- right[[k]]
  ##   }
  ## }
  ## left
}
