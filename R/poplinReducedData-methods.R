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
  "poplin_reduced_list",
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
  "poplin_reduced_list",
  "poplin",
  function(x, check_samplenames = TRUE, ..., value) {
    if (check_samplenames) {
      for (v in seq_along(value)) {
        value[[v]] <- .check_samplenames(x, value[[v]], fun ='poplin_reduced_list')
      }
    }
    value_names <- names(value)
    if (!is.null(value_names) && anyDuplicated(value_names[value_names != ""])) {
      stop("'names(value)' contains duplicates. ",
           "Use different names for incoming data.")
    }
    .set_poplinReducedData_datalist(
      x, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      funstr = "poplin_reduced_list",
      name_pattern = "reduced")
  }
)

##' @export
setMethod(
  "poplin_reduced_names",
  "poplin",
  function(x) {
    .get_poplinReducedData_names(x, get_slot = poplinReducedData)
  }
)

##' @export
setReplaceMethod(
  "poplin_reduced_names",
  c("poplin", "character"),
  function(x, value) {
    if (!is.null(value) && anyDuplicated(value[value != ""])) {
      stop("'value' contains duplicates. ",
           "Use unique names.")
    }
    .set_poplinReducedData_names(
      x, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      name_pattern = "reduced"
    )
  }
)

##' @export
setMethod(
  "poplin_reduced",
  c("poplin", "numeric"),
  function(x, type) {
    .get_poplinReducedData_data_integer(
      x, type,
      get_slot = poplinReducedData,
      funstr = "poplin_reduced"
    )
  }
)

##' @export
setMethod(
  "poplin_reduced",
  c("poplin", "character"),
  function(x, type) {
    .get_poplinReducedData_data_character(
      x, type,
      get_slot = poplinReducedData,
      funstr ="poplin_reduced",
      namestr = "poplin_reduced_names"
    )
  }
)

##' @export
setMethod(
  "poplin_reduced",
  c("poplin", "missing"),
  function(x, type) {
    .get_poplinReducedData_data_missing(
      x,
      base_fun = poplin_reduced,
      name_fun = poplin_reduced_names,
      funstr = "poplin_reduced"
    )
  }
)

##' @export
setReplaceMethod(
  "poplin_reduced",
  c("poplin", "numeric"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    value <- .check_samplenames(x, value, check_samplenames)
    .set_poplinReducedData_data_integer(
      x, type, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      funstr = "poplin_reduced"
    )
  }
)

##' @export
setReplaceMethod(
  "poplin_reduced",
  c("poplin", "character"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    if (type == "") {
      stop("Empty string is not allowed. ",
           "Use a different name for incoming data.")
    }
    value <- .check_samplenames(x, value, check_samplenames)
    .set_poplinReducedData_data_character(
      x, type, value,
      get_slot = poplinReducedData,
      set_element_fun = `poplinReducedData<-`,
      funstr = "poplin_reduced"
    )
  }
)

##' @export
setReplaceMethod(
  "poplin_reduced",
  c("poplin", "missing"),
  function(x, type, check_samplenames = TRUE, ..., value) {
    .set_poplinReducedData_data_missing(
      x, value,
      base_fun = `poplin_reduced<-`,
      name_fun = poplin_reduced_names,
      name_pattern = "reduced"
    )
  }
)
