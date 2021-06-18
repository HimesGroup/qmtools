##' @export
setMethod(
  "imputedDataList",
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

##' @export
setReplaceMethod(
  "imputedDataList",
  "poplin",
  function(x, check_dimnames = TRUE, ..., value) {
    if (check_dimnames) {
      for (v in seq_along(value)) {
        value[[v]] <- .check_dimnames(x, value[[v]], fun ='imputedDataList')
      }
    }
    .set_poplinData_datalist(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "imputedDataList",
      funstr = "imputedDataList",
      name_pattern = "imputed"
    )
  }
)

#' @export
setMethod(
  "imputedDataNames",
  "poplin",
  function(x) {
    .get_poplinData_names(x, get_slot = poplinData, element = "imputedDataList")
  }
)

#' @export
setReplaceMethod(
  "imputedDataNames",
  c("poplin", "character"),
  function(x, value) {
    .set_poplinData_names(
      x, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "imputedDataList",
      name_pattern = "imputed"
    )
  }
)


#' @export
setMethod(
  "imputedData",
  c("poplin", "numeric"),
  function(x, type) {
    .get_poplinData_data_integer(
      x, type,
      get_slot = poplinData,
      element = "imputedDataList", 
      funstr = "imputedData"
    )
  }
)

#' @export
setMethod(
  "imputedData",
  c("poplin", "character"),
  function(x, type) {
    .get_poplinData_data_character(
      x, type,
      get_slot = poplinData,
      element = "imputedDataList", 
      funstr ="imputedData",
      namestr = "imputedDataNames"
    )
  }
)

#' @export
setMethod(
  "imputedData",
  c("poplin", "missing"),
  function(x, type) {
    .get_poplinData_data_missing(
      x,
      base_fun = imputedData, 
      name_fun = imputedDataNames, 
      funstr = "imputedData"
    )
  }
)

#' @export
setReplaceMethod(
  "imputedData",
  c("poplin", "numeric"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_integer(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "imputedDataList", 
      funstr = "imputedData"
    )
  }
)

#' @export
setReplaceMethod(
  "imputedData",
  c("poplin", "character"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    value <- .check_dimnames(x, value, check_dimnames)
    .set_poplinData_data_character(
      x, type, value,
      get_slot = poplinData,
      set_element_fun = `poplinData<-`,
      element = "imputedDataList", 
      funstr = "imputedData"
    )
  }
)

#' @export
setReplaceMethod(
  "imputedData",
  c("poplin", "missing"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    .set_poplinData_data_missing(
      x, value,
      base_fun = `imputedData<-`,
      name_fun = imputedDataNames,
      name_pattern = "imputed"
    )
  }
)
