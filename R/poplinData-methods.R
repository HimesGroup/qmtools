##' @export
setMethod("poplinData", "poplin", function(x) x@poplinData)

##' @export
setReplaceMethod("poplinData", "poplin", function(x, value) {
  x@poplinData <- value
  x
})


##' @importClassesFrom S4Vectors SimpleList
##' @export
setMethod(
  "poplin_data_list",
  "poplin",
  function(x) {
    value <- as(poplinData(x), "SimpleList")
    for (i in seq_along(value)) {
      rownames(value[[i]]) <- rownames(x)
      colnames(value[[i]]) <- colnames(x)
    }
    value
  }
)

##' @export
setReplaceMethod(
  "poplin_data_list",
  "poplin",
  function(x, check_dimnames = TRUE, ..., value) {
    if (check_dimnames) {
      for (v in seq_along(value)) {
        value[[v]] <- .check_dimnames(x, value[[v]], fun ='poplin_data_list')
      }
    }
    .set_poplinData_datalist(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      funstr = "poplin_data_list",
      name_pattern = "poplin")
  }
)

##' @export
setMethod(
  "poplin_data_names",
  "poplin",
  function(x) {
    .get_poplinData_names(x, get_slot = poplinData)
  }
)

##' @export
setReplaceMethod(
  "poplin_data_names",
  c("poplin", "character"),
  function(x, value) {
    .set_poplinData_names(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      name_pattern = "poplin"
    )
  }
)

##' @export
setMethod(
  "poplin_data",
  c("poplin", "numeric"),
  function(x, type) {
    .get_poplinData_data_integer(
      x, type,
      get_slot = poplinData,
      funstr = "poplin_data"
    )
  }
)

##' @export
setMethod(
  "poplin_data",
  c("poplin", "character"),
  function(x, type) {
    .get_poplinData_data_character(
      x, type,
      get_slot = poplinData,
      funstr ="poplin_data",
      namestr = "poplin_data_names"
    )
  }
)

##' @export
setMethod(
  "poplin_data",
  c("poplin", "missing"),
  function(x, type) {
    .get_poplinData_data_missing(
      x,
      base_fun = poplin_data,
      name_fun = poplin_data_names,
      funstr = "poplin_data"
    )
  }
)

##' @export
setReplaceMethod(
  "poplin_data",
  c("poplin", "numeric"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_integer(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      funstr = "poplin_data"
    )
  }
)


##' @export
setReplaceMethod(
  "poplin_data",
  c("poplin", "character"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_character(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      funstr = "poplin_data"
    )
  }
)

##' @export
setReplaceMethod(
  "poplin_data",
  c("poplin", "missing"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    .set_poplinData_data_missing(
      x, value,
      base_fun = `poplin_data<-`,
      name_fun = poplin_data_names,
      name_pattern = "poplin"
    )
  }
)
