##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop = TRUE) {
  ## x <- updateObject(x)
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    poplinData(x) <- poplinData(x)[ii, , drop = FALSE]
    if (length(reducedDataList(x)) != 0L) {
      message("'poplinReducedData' slot was reset.")
    }
    poplinReducedData(x) <- new("DFrame", nrows = ncol(x))
  }
  if (!missing(j)) {
    jj <- .get_subset_index(j, colnames(x))
    poplinData(x)[["imputedDataList"]] <- .subset_columns(
      x, jj, get_slot = poplinData, element = "imputedDataList"
    )
    poplinData(x)[["normalizedDataList"]] <- .subset_columns(
      x, jj, get_slot = poplinData, element = "normalizedDataList"
    )
    poplinReducedData(x) <- poplinReducedData(x)[jj, , drop = FALSE]
  }
  out <- callNextMethod()
  missingCount(out) <- .get_missing_count(assay(out))
  out
})
