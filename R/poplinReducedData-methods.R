##' Dimension-reduced data methods
##'
##' Methods to get or set dimension reduction results in a \linkS4class{poplin}
##' object. These methods are intended to store and retrieve low-dimensional
##' representation (e.g., PCA, PLS-DA) of LC/MS data sets.
##'
##' @section Getter methods:
##'
##' In the code snippets below, \code{x} is a \linkS4class{poplin} object.
##'
##' \describe{
##' \item{\code{poplin_reduced_names(x)}:}{
##' Return the names of all dimension-reduced data sets stored in
##' \code{x@poplinReducedData}.
##' }
##' \item{\code{poplin_reduced_list(x)}:}{
##' Retrieves a named \linkS4class{List} of matrices containing one or more
##' dimension-reduced data. Each entry is a matrix with the same number of rows
##' as \code{ncol(x)}.
##' }
##' \item{\code{poplin_reduced(x, type)}:}{
##' Retrieves a matrix of low-dimensional representation. \code{type} is either
##' a string specifying the name of data set to retrieve or an integer
##' specifying the index of the desired data sets, defaulting to the first entry
##' if missing.
##' }
##' }
##'
##' @section Setter methods:
##'
##' \describe{
##' \item{\code{poplin_reduced_names(x) <- value}:}{
##' \code{value} must be a character vector to be assigned for the names of
##' dimension-reduced data sets.
##' }
##' \item{\code{poplin_reduced_list(x) <- value}:}{
##' \code{value} is expected to be a named \linkS4class{List} of matrices. If
##' the result already exists, it will be replaced. If \code{value} is
##' \code{NULL}, any existing result will be removed.
##' }
##' \item{\code{poplin_reduced(x, type) <- value}:}{
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
##' @name poplin_reduced
##' @docType methods
##' @seealso [poplin_raw], [poplin_data]
NULL

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
