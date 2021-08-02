##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
.poplin <- setClass(
  "poplin",
  slots = c(
    missingCount = "list",
    poplinData = "DataFrame",
    poplinReducedData = "DataFrame"
  ),
  contains = "SummarizedExperiment"
)

##' @export
##' @import methods
##' @importFrom SummarizedExperiment SummarizedExperiment
poplin <- function(intensity,  ...,
                   imputedDataList = list(),
                   normalizedDataList = list()) {
  se <- SummarizedExperiment(list(raw = intensity), ...)
  if (!is(se, "SummarizedExperiment")) {
    se <- as(se, "SummarizedExperiment")
  }
  .se_to_poplin(
    se,
    imputedDataList = imputedDataList,
    normalizedDataList = normalizedDataList
  )
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
.se_to_poplin <- function(se,
                          imputedDataList = list(),
                          normalizedDataList = list()) {
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
    poplinData = new("DFrame", nrows = nrow(se)),
    poplinReducedData = new("DFrame", nrows = ncol(se))
  )
  imputedDataList(out) <- imputedDataList
  normalizedDataList(out) <- normalizedDataList
  missingCount(out) <- .get_missing_count(assay(out))
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
