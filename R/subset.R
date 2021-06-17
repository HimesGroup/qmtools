## ##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) {
  ## x <- updateObject(x)
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    ## poplinData(x) <- poplinData(x)[ii, , drop=FALSE]
  }
  if (!missing(j)) {
    jj <- .get_subset_index(j, colnames(x))
    ## poplinData(x) <- poplinData(x)[, jj, drop=FALSE]
  }
  callNextMethod()
})

