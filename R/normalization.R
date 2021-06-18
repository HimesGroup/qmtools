##' @export
setMethod(
  "normalizedDataList",
  "poplin",
  function(x) {
  value <- .get_poplinData_datalist(x, get_slot = poplinData,
                                    element = "normalizedDataList")
  for (i in seq_along(value)) {
    rownames(value[[i]]) <- rownames(x)
    colnames(value[[i]]) <- colnames(x)
  }
  value
  }
)

##' @export
setReplaceMethod(
  "normalizedDataList",
  "poplin",
  function(x, check_dimnames = TRUE, ..., value) {
    if (check_dimnames) {
      for (v in seq_along(value)) {
        value[[v]] <- .check_dimnames(x, value[[v]], fun ='normalizedDataList')
      }
    }
    .set_poplinData_datalist(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "normalizedDataList",
      funstr = "normalizedDataList",
      name_pattern = "normalized"
    )
  }
)

#' @export
setMethod(
  "normalizedDataNames",
  "poplin",
  function(x) {
    .get_poplinData_names(x, get_slot = poplinData, element = "normalizedDataList")
  }
)

#' @export
setReplaceMethod(
  "normalizedDataNames",
  c("poplin", "character"),
  function(x, value) {
    .set_poplinData_names(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "normalizedDataList",
      name_pattern = "normalized"
    )
  }
)


#' @export
setMethod(
  "normalizedData",
  c("poplin", "numeric"),
  function(x, type) {
    .get_poplinData_data_integer(
      x, type,
      get_slot = poplinData,
      element = "normalizedDataList",
      funstr = "normalizedData"
    )
  }
)

#' @export
setMethod(
  "normalizedData",
  c("poplin", "character"),
  function(x, type) {
    .get_poplinData_data_character(
      x, type,
      get_slot = poplinData,
      element = "normalizedDataList",
      funstr ="normalizedData",
      namestr = "normalizedDataNames"
    )
  }
)

#' @export
setMethod(
  "normalizedData",
  c("poplin", "missing"),
  function(x, type) {
    .get_poplinData_data_missing(
      x,
      base_fun = normalizedData,
      name_fun = normalizedDataNames,
      funstr = "normalizedData"
    )
  }
)

#' @export
setReplaceMethod(
  "normalizedData",
  c("poplin", "numeric"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_integer(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "normalizedDataList",
      funstr = "normalizedData"
    )
  }
)

#' @export
setReplaceMethod(
  "normalizedData",
  c("poplin", "character"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_character(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "normalizedDataList",
      funstr = "normalizedData"
    )
  }
)

#' @export
setReplaceMethod(
  "normalizedData",
  c("poplin", "missing"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    .set_poplinData_data_missing(
      x, value,
      base_fun = `normalizedData<-`,
      name_fun = normalizedDataNames,
      name_pattern = "normalized"
    )
  }
)
