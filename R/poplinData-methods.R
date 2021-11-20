##' Poplin data methods
##'
##' Methods to get or set data processing results in a \linkS4class{poplin}
##' object. These methods are intended to store and retrieve processed data
##' sets involved in normalization, imputation, and transformation.
##'
##' @section Getter methods:
##'
##' Let \code{x} is a \linkS4class{poplin} object.
##'
##' \describe{
##' \item{\code{poplin_data_names(x)}:}{
##' Return the names of all processed data sets stored in \code{x@poplinData}.
##' }
##' \item{\code{poplin_data_list(x, type)}:}{
##' Retrieves a named \linkS4class{List} of matrices containing one or more
##' data processing results. Each entry is a matrix with the same dimension of raw
##' feature data in \code{assay(x)}.
##' }
##' \item{\code{poplin_data(x, type)}:}{
##' Retrieves a matrix of data processing result. \code{type} is either a
##' string specifying the name of data set to retrieve or an integer specifying
##' the index of the desired data set, defaulting to the first entry if missing.
##' }
##' }
##'
##' @section Setter methods:
##'
##' \describe{
##' \item{\code{poplin_data_names(x) <- value}:}{
##' \code{value} is a character vector to be assigned for the names of
##' processed data sets.
##' }
##' \item{\code{poplin_data_list(x) <- value}:}{
##' \code{value} is expected to be a named \linkS4class{List} of matrices. If
##' the result already exists, it will be replaced. If \code{value} is
##' \code{NULL}, any existing result will be removed.
##' }
##' \item{\code{poplin_data(x, type) <- value}:}{
##' \code{value} is expected to be a matrix. \code{type} determines how the
##' result is assigned:
##' - integer: it must be within the range of existing results. \code{value}
##' will replace the result at that index.
##' - character: if the result exists with this name, it will be replaced with
##' \code{value}. Otherwise a new result with this name will be appended.
##' - missing: \code{value} will be assigned to the first entry.
##'
##' If \code{value} is \code{NULL}, the result corresponding to \code{type} will
##' be removed.
##' }
##' }
##'
##' @param x
##' @return

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
    .check_name_duplicates(assayNames(x), names(value),
                           msg1 = "names(value)",
                           msg2 = "Use different names for incoming data.")
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
    .check_name_duplicates(assayNames(x), value,
                           msg1 = "value",
                           msg2 = "Use unique names.")
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
    if (type == "") {
      stop("Empty string is not allowed. ",
           "Use a different name for incoming data.")
    }
    .check_name_duplicates(assayNames(x), type,
                           msg1 = "value",
                           msg2 = "Use a different name.")
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
