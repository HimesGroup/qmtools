##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
setClass(
  "poplin",
  slots = c(
    poplinData = "DataFrame",
    poplinReduced = "DataFrame"
  ),
  contains = "SummarizedExperiment"
)
