##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
setClass(
  "poplin",
  slots = c(
    poplinData = "DataFrame",
    poplinReducedData = "DataFrame"
  ),
  contains = "SummarizedExperiment"
)
