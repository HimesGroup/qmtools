##' Raw data methods
##'
##' Methods to get or set raw intensity data. These methods are simply aliases
##' for [assayNames], [assays], and [assay] functions from the
##' \pkg{SummarizedExperiment} package.
##'
##' In the code snippets below, \code{x} is a \linkS4class{poplin} object.
##'
##' \describe{
##'
##' \item{\code{poplin_raw_names(x), poplin_raw_names(x) <- value}:}{ Get or set
##' the names of raw intensity data sets. \code{value} must be a character
##' vector to be assigned. This is an alias of
##' [SummarizedExperiment::assayNames]. }
##'
##' \item{\code{poplin_raw_list(x), poplin_raw_list(x) <- value}:}{ Get or set
##' the raw intensity data sets. \code{value} must be a named \linkS4class{List}
##' of matrices in which each element has the same dimension as \code{x}. This
##' is an alias of [SummarizedExperiment::assays]. }
##'
##' \item{\code{poplin_raw(x, i), poplin_raw(x, i) <- value}:}{ Get or set the
##' \code{i}-th element of raw intensity data sets. It is a convenient
##' alternative to \code{poplin_raw_list(x)[[i]]}. \code{value} must be a matrix
##' with the same dimension as \code{x}. If \code{i} is missing, the first entry
##' is retrieved or replaced. This is an alias of [SummarizedExperiment::assay].
##' }
##'
##' }
##' @name poplin_raw
##' @aliases
##' poplin_raw poplin_raw_list poplin_raw_names
##' poplin_raw,poplin,missing-method
##' poplin_raw,poplin,numeric-method
##' poplin_raw,poplin,character-method
##' poplin_raw_list,poplin-method
##' poplin_raw_names,poplin-method
##' poplin_raw<- poplin_raw_list<- poplin_raw_names<-
##' poplin_raw poplin_raw_list poplin_raw_names
##' poplin_raw<-,poplin,missing-method
##' poplin_raw<-,poplin,numeric-method
##' poplin_raw<-,poplin,character-method
##' poplin_raw_list<-,poplin-method
##' poplin_raw_names<-,poplin,character-method
##' @seealso [poplin_data], [poplin_reduced]
NULL

setMethod(
  "poplin_raw_list", "poplin",
  function(x, ...) {
    assays(x = x, ...)
})

setReplaceMethod(
  "poplin_raw_list", "poplin",
  function(x, check_dimnames = TRUE, ..., value) {
  if (check_dimnames) {
    for (v in seq_along(value)) {
      value[[v]] <- .check_dimnames(x, value[[v]], fun ='poplin_raw_list<-')
    }
  }
  names(value) <- .replace_empty_names(
    names(value), N = length(value), msg = "names(value)",
    name_pattern = "raw"
  )
  `assays<-`(x = x, ..., value = value)
})

setMethod(
  "poplin_raw_names", "poplin",
  function(x, ...) {
    assayNames(x = x, ...)
})

setReplaceMethod(
  "poplin_raw_names", c("poplin", "character"),
  function(x, ..., value) {
    N <- length(assayNames(x))
    value <- .replace_empty_names(value, N = N, msg = "value",
                                  name_pattern = "raw")
    `assayNames<-`(x = x, ..., value = value)
})

setMethod(
  "poplin_raw", c("poplin", "numeric"),
  function(x, type, ...) {
    assay(x, i = type)
  })

setMethod(
  "poplin_raw", c("poplin", "character"),
  function(x, type, ...) {
    assay(x, i = type)
  })

setMethod(
  "poplin_raw", c("poplin", "missing"),
  function(x, type, ...) {
    assay(x, i = 1L, ...)
  })

setReplaceMethod(
  "poplin_raw", c("poplin", "numeric"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    if (check_dimnames) {
      value <- .check_dimnames(x, value, fun = "poplin_raw<-")
    }
    `assay<-`(x, i = type, ..., value = value)
  })

setReplaceMethod(
  "poplin_raw", c("poplin", "character"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    if (type == "") {
      stop("Empty string is not allowed. ",
           "Use a different name for incoming data.")
    }
    if (check_dimnames) {
      value <- .check_dimnames(x, value, fun = "poplin_raw<-")
    }
    `assay<-`(x, i = type, ..., value = value)
  })

setReplaceMethod(
  "poplin_raw", c("poplin", "missing"),
  function(x, type, check_dimnames = TRUE, ..., value) {
    if (check_dimnames) {
      value <- .check_dimnames(x, value, fun = "poplin_raw<-")
    }
    if (length(poplin_raw_names(x))) {
      ## replace the first entries
      type <- 1L
    } else {
      ## if no data is available, set it to the first
      type <- paste0("raw", 1L)
    }
    `assay<-`(x, i = type, ..., value = value)
  })


## ##' @export
## poplin_raw_list <- function(...) assays(...)

## ##' @export
## `poplin_raw_list<-` <- function(x, ..., value) `assays<-`(x, ..., value = value)

## ##' @export
## poplin_raw_names <- function(...) assayNames(...)

## ##' @export
## `poplin_raw_names<-` <- function(x, ..., value) `assayNames<-`(x, ..., value = value)

## ##' @export
## poplin_raw <- function(...) assay(...)

## ##' @export
## `poplin_raw<-` <- function(x, ..., value) `assay<-`(x, ..., value = value)

