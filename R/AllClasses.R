##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
.poplin <- setClass(
  "poplin",
  slots = c(
    missing = "list"
    ## missing_flag = "matrix"
  ),
  contains = "SummarizedExperiment"
)

##' @export
##' @import methods
##' @importFrom SummarizedExperiment SummarizedExperiment
poplin <- function(intensity,  ...) {
  se <- SummarizedExperiment(list(raw = intensity), ...)
  if(!is(se, "SummarizedExperiment")) {
    se <- as(se, "SummarizedExperiment")
  }
  ## ints <- SummarizedExperiment::assay(se)
  ## missing_flag <- apply(ints, c(1, 2), function(a) ifelse(is.na(a), 1, 0))
  ## .poplin(se, missing_flag = missing_flag)
  .se_to_poplin(se)
}


##' @importFrom S4Vectors DataFrame SimpleList
##' @importClassesFrom S4Vectors DataFrame
##' @importFrom methods new
##' @importMethodsFrom SummarizedExperiment assay
.se_to_poplin <- function(se) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    ## Temporarily disable validity check and restore original setting upon the
    ## exit of function
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }

  out <- new("poplin", se)
  out@missing$per_feature <- apply(assay(se), 1, function(x) sum(is.na(x)))
  out@missing$per_sample <- apply(assay(se), 2, function(x) sum(is.na(x)))
  out
}


#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
setAs("SummarizedExperiment", "poplin", function(from) {
  .se_to_poplin(from)
})

#' @exportMethod coerce
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment SummarizedExperiment
setAs("SummarizedExperiment", "poplin", function(from) {
  .se_to_poplin(as(from, "SummarizedExperiment"))
})
