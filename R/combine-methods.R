##' Combining or subsetting poplin objects
##'
##' Methods to combine multiple \linkS4class{poplin} objects by row or column,
##' or to subset a \linkS4class{poplin} object by row or column.
##'
##' @section Combining:
##'
##' In the code snippets below, \code{...} represents multiple
##' \linkS4class{poplin} objects to be combined.
##'
##' \describe{
##' \item{\code{rbind(..., deparse.level = 1)}:}{
##'
##' Take a sequence of poplin objects in \code{...} and combine by rows. Note
##' that all objects in ... must have the exact same values for
##' \code{poplin_data_list}. See \code{?rbind} for the interpretation of
##' \code{deparse.level}.
##'
##' }
##' \item{\code{cbind(..., deparse.level = 1)}:}{
##'
##' Take a sequence of poplin objects in \code{...} and combine by columns. Note
##' that all objects in ... must have the same values of
##' \code{poplin_reduced_names}. Dimension reduction results with the same name
##' across the objects will be combined row-wise to create the corresponding
##' entry in the output object. See \code{?cbind} for the interpretation of
##' \code{deparse.level}.
##'
##' }
##' }
##'
##' Refer to \code{help("SummarizedExperiment-class", package =
##' "SummarizedExperiment")} for details on how \code{rowData}, \code{colData},
##' and \code{metadata} are combined in the output object.
##'
##' @section Subsetting:
##'
##' In the code snippets below, \code{x} is a \linkS4class{poplin}
##' object.
##'
##' \describe{
##'
##' \item{\code{x[i, j]}, \code{x[i, j] <- value}:}{\code{i} and \code{j} are
##' indices specifying the rows and columns to extract or replace. Indices can
##' be a logical, integer, character vector, or empty (missing). \code{value}
##' must be a poplin object with the dimension and assay elements consistent
##' with the subset \code{x[i, j]} being replaced. \code{value} is also expected
##' to have the same name and order of \code{poplin_data_names} (and
##' \code{poplin_reduced_names} as well if \code{j} is specified) as \code{x}.}
##'
##' }
##'
##' @importFrom BiocGenerics rbind cbind
##' @importFrom SummarizedExperiment Assays
##' @name poplin-combine/subset
##' @aliases
##' cbind,poplin-method
##' rbind,poplin-method
##' [,poplin,ANY,ANY,ANY-method
##' [<-,poplin,ANY,ANY,poplin-method
##' @docType methods
NULL

## Need to think about how to handle metadata of poplinData(x) and
## poplinReduced(x) when data are combined
## poplin class cbind and rbind
setMethod("rbind", "poplin", function(..., deparse.level = 1) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  out <- callNextMethod()
  args <- list(...)
  tryCatch({
    poplinData_all <- do.call(rbind, lapply(args, poplinData))
    ## How to handle metadata?
    ## Currently clean metadata; consistency with assay-via method
    metadata(poplinData_all) <- list() 
  })
  ## Utilize SE class for combining objects: see the SingleCellExperiment source
  poplinReduced_se <- lapply(args, function(x) .poplin_to_se_coldata(x))
  tryCatch({
    poplinReduced_all <- colData(do.call(rbind, poplinReduced_se))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinReduced' in 'rbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  ## Skip validity checks with check = FALSE for efficiency as modification
  ## cannot alter the validity of object
  BiocGenerics:::replaceSlots(out, poplinData = poplinData_all,
                              poplinReduced = poplinReduced_all,
                              check = FALSE)
})

setMethod("cbind", "poplin", function(..., deparse.level = 1) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  out <- callNextMethod()
  args <- list(...)
  poplinData_assays <- lapply(args, function(x) .poplin_to_assays(x))
  tryCatch({
    combined <- do.call(cbind, poplinData_assays)
    poplinData_all <- do.call(
      DataFrame, c(lapply(combined@data, I),
                   list(row.names = NULL, check.names = FALSE))
    )
    ## Make sure the returned DFrame has the correct # of rows
    ## Since combining empty poplinData would return nrow = 0
    poplinData_all <- BiocGenerics:::replaceSlots(poplinData_all,
                                                  nrows = nrow(args[[1]]))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinData' in 'cbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  tryCatch({
    poplinReduced_all <- do.call(rbind, lapply(args, poplinReduced))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinReduced' in 'cbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  ## Skip validity checks with check = FALSE for efficiency as modification
  ## cannot alter the validity of object
  BiocGenerics:::replaceSlots(out, poplinData = poplinData_all,
                              poplinReduced = poplinReduced_all,
                              check = FALSE)
})


