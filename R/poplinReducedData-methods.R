##' @export
setMethod("poplinReducedData", "poplin", function(x) x@poplinReducedData)

##' @export
setReplaceMethod("poplinReducedData", "poplin", function(x, value) {
  x@poplinReducedData <- value
  x
})

##' @importClassesFrom S4Vectors SimpleList
##' @export
setMethod(
  "reducedDataList",
  "poplin",
  function(x) {
    value <- as(poplinReducedData(x), "SimpleList")
    for (i in seq_along(value)) {
      rownames(value[[i]]) <- colnames(x)
    }
    value
  }
)

##' @export
setReplaceMethod(
  "reducedDataList",
  "poplin",
  function(x, check_samplenames = TRUE, ..., value) {
    if (check_samplenames) {
      for (v in seq_along(value)) {
        value[[v]] <- .check_samplenames(x, value[[v]], fun ='reducedDataList')
      }
    }
    .set_poplinReducedData_datalist(
      x, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      name_pattern = "reduced")
  }
)


#' @export
setMethod(
  "reducedDataNames",
  "poplin",
  function(x) {
    .get_poplinReducedData_names(x, get_slot = poplinReducedData)
  }
)

#' @export
setReplaceMethod(
  "reducedDataNames",
  c("poplin", "character"),
  function(x, value) {
    .set_poplinReducedData_names(
      x, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      name_pattern = "reduced"
    )
  }
)

