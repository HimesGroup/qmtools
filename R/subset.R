##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop = TRUE) {
  ## x <- updateObject(x)
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    poplinData(x) <- poplinData(x)[ii, , drop = FALSE]
    ## if (length(reducedDataList(x)) != 0L) {
    ##   message("Row-subsetting operation: 'poplinReducedData' slot was reset.")
    ## }
    ## poplinReducedData(x) <- new("DFrame", nrows = ncol(x))
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


##' @export
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
##' @importFrom SummarizedExperiment rowData colData
setReplaceMethod(
  "[", c("poplin", "ANY", "ANY", "poplin"), function(x, i, j, ..., value) {

    ## x <- updatedObject(x)
    ## value <- updateObject(value)

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
      ## message("Row-subsetting operation: 'poplinReducedData' slot was reset.")
      ## poplinReducedData(x) <- new("DFrame", nrows = ncol(x))
    }

    if (!missing(j)) {
      jj <- .get_subset_index(j, colnames(x))

      if (missing(i)) {
        tryCatch({
          imp_left <- .replace_columns(
            x, jj, get_slot = poplinData, element = "imputedDataList", value
          )
          norm_left <- .replace_columns(
            x, jj, get_slot = poplinData, element = "normalizedDataList", value
          )
        }, error=function(err) {
          stop(
            "failed to replace 'poplinData' in '<", class(x), ">[,j] <- value'\n",
            conditionMessage(err))
        })
      } else {
        tryCatch({
          imp_left <- .replace_columns(
            x, jj, get_slot = poplinData, element = "imputedDataList", value, ii
          )
          norm_left <- .replace_columns(
            x, jj, get_slot = poplinData, element = "normalizedDataList", value, ii
          )
        }, error=function(err) {
          stop(
            "failed to replace 'poplinData' in '<", class(x), ">[,j] <- value'\n",
            conditionMessage(err))
        })
      }
      poplinReducedData_left <- poplinReducedData(x)
      poplinReducedData_right <- poplinReducedData(value)
      tryCatch({
        poplinReducedData_left[jj, ] <- poplinReducedData_right
      }, error=function(err) {
        stop(
          "failed to replace 'poplinReducedData' in '<", class(x), ">[,j] <- value'\n",
          conditionMessage(err))
      })

      poplinData(x)[["imputedDataList"]] <- imp_left
      poplinData(x)[["normalizedDataList"]] <- norm_left
      poplinReducedData(x) <- poplinReducedData_left
    }

    out <- callNextMethod()
    missingCount(out) <- .get_missing_count(assay(out))
    out
}) 
