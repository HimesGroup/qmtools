##' @importFrom SummarizedExperiment SummarizedExperiment
.poplin_to_se_coldata <- function(x) {
  SummarizedExperiment(colData = poplinReduced(x))
}

##' @importFrom SummarizedExperiment Assays
.poplin_to_assays <- function(x) {
  Assays(poplin_data_list(x))
}
