##' The poplin class
##'
##' The poplin class is designed to process LC/MS data. It is an extension of
##' the standard SummarizedExperiment class, and supports additional containers
##' for data processing results (e.g., normalization, imputation) via
##' [poplin_data] and dimension reduction results (PCA, PLS-DA) via
##' [poplin_reduced]. A poplin object also can be created by coercing from a
##' \linkS4class{SummarizedExperiment} object.
##'
##' @param intensity Peak intensity matrix where rows represent features and
##'   columns represent samples.
##' @param ... Arguments passed to the [SummarizedExperiment] constructor.
##' @return A poplin object
##' @author Jaehyun Joo
##' @name poplin-class
##' @aliases
##' poplin
##' poplin-class
##' coerce,SummarizedExperiment,poplin-method
##' @usage
##' ## Constructor
##' poplin(intensity, ...)
##' @examples
##' nsamp <- 20
##' nfeature <- 1000
##' m <- matrix(sample(1:1000000, size = 200000), nrow = nsamp, ncol = nfeature)
##' poplin(m)
##' ## Coercion from an SummarizedExperiment object
##'
NULL

##' Internal poplin fields
##'
##' Methods to get or set internal fields in a \linkS4class{poplin} object.
##' These methods are not intended to be used by end users of the \pkg{poplin}
##' package.
##'
##' In the snippets below, \code{x} is a \linkS4class{poplin} object.
##'
##' \describe{
##'
##' \item{\code{poplinData(x)}, \code{poplinData(x) <- value}:}{ Returns a
##' \linkS4class{DataFrame} of matrices containing one or more data processing
##' results. \code{value} must be a \linkS4class{DataFrame} with the dimension
##' equal to \code{dim(x)}. End users are supposed to interact with this field
##' via [poplin_data]. }
##'
##' \item{\code{poplinReduced(x)}, \code{poplinReduced(x) <- value}:}{
##' Returns a \linkS4class{DataFrame} of matrices containing one or more
##' dimension-reduced data. \code{value} must be a \linkS4class{DataFrame} with
##' the dimension equal to \code{ncol(x)}. End users are supposed to interact
##' with this field via [poplin_reduced]. }
##'
##' }
##'
##' @aliases
##' poplinData poplinReduced
##' poplinData,poplin-method
##' poplinReduced,poplin-method
##' poplinData<- poplinReduced<-
##' poplinData<-,poplin-method
##' poplinReduced<-,poplin-method
##' @seealso [poplin_data], [poplin_reduced]
##' @name poplin-internals
NULL


##' @export
##' @import methods
##' @importFrom SummarizedExperiment SummarizedExperiment
poplin <- function(intensity,  ...) {
  se <- SummarizedExperiment(assays = SimpleList(raw = intensity), ...)
  if (!is(se, "SummarizedExperiment")) {
    se <- as(se, "SummarizedExperiment")
  }
  .se_to_poplin(se)
}

##' @importFrom S4Vectors DataFrame SimpleList
##' @importClassesFrom S4Vectors DataFrame
##' @importFrom methods new
##' @importFrom BiocGenerics nrow ncol
##' @importMethodsFrom SummarizedExperiment assays assayNames assay assays<- assayNames<- assay<-
.se_to_poplin <- function(se) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    ## Temporarily disable validity check and restore original setting upon the
    ## exit of function
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  new(
    "poplin",
    se,
    poplinData = new("DFrame", nrows = nrow(se)),
    poplinReduced = new("DFrame", nrows = ncol(se))
  )
}

##' @exportMethod coerce
setAs("SummarizedExperiment", "poplin", function(from) {
  .se_to_poplin(from)
})

##' @importFrom S4Vectors coolcat
.poplin_show <- function(object) {
  ## callNextMethod()
  cat("class:", class(object), "\n")
  cat("dim:", dim(object), "\n")
  ## metadata()
  expt <- names(metadata(object))
  if (is.null(expt))
    expt <- character(length(metadata(object)))
  coolcat("metadata(%d): %s\n", expt)
  ## rownames()
  rownames <- rownames(object)
  if (!is.null(rownames)) coolcat("rownames(%d): %s\n", rownames)
  else cat("rownames: NULL\n")
  ## rowData`()
  coolcat("rowData names(%d): %s\n", names(rowData(object, use.names=FALSE)))
  ## colnames()
  colnames <- colnames(object)
  if (!is.null(colnames)) coolcat("colnames(%d): %s\n", colnames)
  else cat("colnames: NULL\n")
  ## colData()
  coolcat("colData names(%d): %s\n", names(colData(object)))
  ## assays() -> alias: poplin_raw()
  nms <- assayNames(object)
  if (is.null(nms))
    nms <- character(length(assays(object, withDimnames=FALSE)))
  coolcat("poplinRaw names(%d): %s\n", nms)
  ## poplin_data()
  coolcat("poplinData names(%d): %s\n", poplin_data_names(object))
  ## poplin_reduced()
  coolcat("poplinReduced names(%d): %s\n", poplin_reduced_names(object))
}

setMethod("show", "poplin", .poplin_show)


setMethod("poplinReduced", "poplin", function(x) x@poplinReduced)

setReplaceMethod("poplinReduced", "poplin", function(x, value) {
  x@poplinReduced <- value
  x
})

setMethod("poplinData", "poplin", function(x) x@poplinData)

setReplaceMethod("poplinData", "poplin", function(x, value) {
  x@poplinData <- value
  x
})

