##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
.poplin <- setClass(
  "poplin",
  slots = c(
    missing = "list",
    imputation = "DataFrame"
    ## missing_flag = "matrix"
  ),
  contains = "SummarizedExperiment"
)

##' @export
##' @import methods
##' @importFrom SummarizedExperiment SummarizedExperiment
poplin <- function(intensity,  ..., poplin_impute = list()) {
  se <- SummarizedExperiment(list(raw = intensity), ...)
  if(!is(se, "SummarizedExperiment")) {
    se <- as(se, "SummarizedExperiment")
  }
  ## ints <- SummarizedExperiment::assay(se)
  ## missing_flag <- apply(ints, c(1, 2), function(a) ifelse(is.na(a), 1, 0))
  ## .poplin(se, missing_flag = missing_flag)
  .se_to_poplin(se, poplin_impute = poplin_impute)
}

.get_missing_count <- function(x) {
  list(
    per_sample = apply(x, 2, function(x) sum(is.na(x))),
    per_feature = apply(x, 1, function(x) sum(is.na(x)))
  )
}

##' @importFrom S4Vectors DataFrame SimpleList
##' @importClassesFrom S4Vectors DataFrame
##' @importFrom methods new
##' @importFrom BiocGenerics nrow ncol
##' @importMethodsFrom SummarizedExperiment assay
.se_to_poplin <- function(se, poplin_impute = list()) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    ## Temporarily disable validity check and restore original setting upon the
    ## exit of function
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  out <- new(
    "poplin",
    se,
    imputation = new("DFrame", nrows = nrow(se))
  )
  out@missing <- .get_missing_count(assay(se))
  ## imputation(out)@poplin_impute <- poplin_impute
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
