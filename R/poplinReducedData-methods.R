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



#' @export
setMethod(
  "reducedData",
  c("poplin", "numeric"),
  function(x, type) {
    .get_poplinReducedData_data_integer(
      x, type,
      get_slot = poplinReducedData,
      funstr = "reducedData"
    )
  }
)

#' @export
setMethod(
  "reducedData",
  c("poplin", "character"),
  function(x, type) {
    .get_poplinReducedData_data_character(
      x, type,
      get_slot = poplinReducedData,
      funstr ="reducedData",
      namestr = "reducedDataNames"
    )
  }
)

#' @export
setMethod(
  "reducedData",
  c("poplin", "missing"),
  function(x, type) {
    .get_poplinReducedData_data_missing(
      x,
      base_fun = reducedData,
      name_fun = reducedDataNames,
      funstr = "reducedData"
    )
  }
)

#' @export
setReplaceMethod(
  "reducedData",
  c("poplin", "numeric"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    value <- .check_samplenames(x, value, check_samplenames)
    .set_poplinReducedData_data_integer(
      x, type, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      funstr = "reducedData"
    )
  }
)

#' @export
setReplaceMethod(
  "reducedData",
  c("poplin", "character"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    value <- .check_samplenames(x, value, check_samplenames)
    .set_poplinReducedData_data_character(
      x, type, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      funstr = "reducedData"
    )
  }
)

#' @export
setReplaceMethod(
  "reducedData",
  c("poplin", "missing"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    .set_poplinReducedData_data_missing(
      x, value,
      base_fun = `reducedData<-`,
      name_fun = reducedDataNames,
      name_pattern = "reduced"
    )
  }
)
