##' @export
##' @export
setMethod(
  "poplinReducedData",
  "poplin",
  function(x) {
    value <- .get_poplinData_datalist(x, get_slot = poplinData,
                                      element = "imputedDataList")
    for (i in seq_along(value)) {
      rownames(value[[i]]) <- rownames(x)
      colnames(value[[i]]) <- colnames(x)
    }
    value
  }
)


