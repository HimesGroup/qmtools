##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop = TRUE) {
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    poplinData(x) <- poplinData(x)[ii, , drop = FALSE]
    ## if (length(reducedDataList(x)) != 0L) {
    ##   message("Row-subsetting operation: 'poplinReduced' slot was reset.")
    ## }
    ## poplinReduced(x) <- new("DFrame", nrows = ncol(x))
  }
  if (!missing(j)) {
    jj <- .get_subset_index(j, colnames(x))
    poplinData(x) <- .subset_columns(x, jj, get_slot = poplinData)
    poplinReduced(x) <- poplinReduced(x)[jj, , drop = FALSE]
  }
  callNextMethod()
})

##' @export
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
##' @importFrom SummarizedExperiment rowData colData
setReplaceMethod(
  "[", c("poplin", "ANY", "ANY", "poplin"), function(x, i, j, ..., value) {

    if (!missing(i)) {
      ii <- .get_subset_index(i, rownames(x))

      if (missing(j)) {
        poplinData_left <- poplinData(x)
        poplinData_right <- poplinData(value)

        tryCatch({
          poplinData_left[ii, ] <- poplinData_right
        }, error=function(err) {
          stop(
            "failed to replace 'poplinData' in '<", class(x), ">[i,] <- value'\n",
            conditionMessage(err))
        })
        poplinData(x) <- poplinData_left
      }
      ## message("Row-subsetting operation: 'poplinReduced' slot was reset.")
      ## poplinReduced(x) <- new("DFrame", nrows = ncol(x))
    }

    if (!missing(j)) {
      jj <- .get_subset_index(j, colnames(x))

      if (missing(i)) {
        tryCatch({
          poplinData_left <- .replace_columns(x, jj, poplinData, value)
        }, error=function(err) {
          stop(
            "failed to replace 'poplinData' in '<", class(x), ">[,j] <- value'\n",
            conditionMessage(err))
        })
      } else {
        tryCatch({
          poplinData_left <- .replace_columns(x, jj, poplinData, value, ii)
        }, error=function(err) {
          stop(
            "failed to replace 'poplinData' in '<", class(x), ">[,j] <- value'\n",
            conditionMessage(err))
        })
      }
      poplinReduced_left <- poplinReduced(x)
      poplinReduced_right <- poplinReduced(value)
      tryCatch({
        poplinReduced_left[jj, ] <- poplinReduced_right
      }, error=function(err) {
        stop(
          "failed to replace 'poplinReduced' in '<", class(x), ">[,j] <- value'\n",
          conditionMessage(err))
      })

      poplinData(x) <- poplinData_left
      poplinReduced(x) <- poplinReduced_left
    }
    callNextMethod()
})
