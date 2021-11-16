##' The poplin class
##'
##' The poplin class is designed to process LC/MS data. It is an extension of
##' the standard SummarizedExperiment class, and supports additional containers
##' for data in process (e.g., normalization, imputation) via
##' \code{\link{poplinData}} and dimension reduction results (PCA, PLS-DA) via
##' \code{\link{poplinReduced}}. A poplin object also can be created by
##' coercing from a \linkS4class{SummarizedExperiment} object.
##' @param intensity Peak intensity matrix where rows represent features and
##'   columns represent samples.
##' @param ... Arguments passed to the
##'   \code{\link[SummarizedExperiment]{SummarizedExperiment}} constructor.
##' @return A poplin object
##' @author Jaehyun Joo
##' @examples
##' nsamp <- 20
##' nfeature <- 1000
##' m <- matrix(sample(1:100000, size = 200000), nrow = nsamp, ncol = nfeature)
##' poplin(m)
##' ## Coercion from an SummarizedExperiment object
##' as(se, "poplin")
##' 
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
##' @importMethodsFrom SummarizedExperiment assay
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
    poplinReducedData = new("DFrame", nrows = ncol(se))
  )
}

##' @exportMethod coerce
setAs("SummarizedExperiment", "poplin", function(from) {
  .se_to_poplin(from)
})

##' @importFrom S4Vectors coolcat
.poplin_show <- function(object) {
  callNextMethod()
  coolcat("poplinData names(%d): %s\n", poplin_data_names(object))
  coolcat("poplinReducedData names(%d): %s\n", poplin_reduced_names(object))
}

##' @export
setMethod("show", "poplin", .poplin_show)
