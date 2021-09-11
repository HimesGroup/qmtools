##' @export
##' @import methods
##' @importClassesFrom SummarizedExperiment SummarizedExperiment
setClass(
  "poplin",
  slots = c(
    missingCount = "list",
    poplinData = "DataFrame",
    poplinReducedData = "DataFrame"
  ),
  contains = "SummarizedExperiment"
)

## Superclass for function arguments
setClass("poplinArgs", contains = "VIRTUAL")

## Convenient union
setClassUnion("character_OR_NULL", c("character", "NULL"))

## Argument class for PQN normalization
setClass(
  "pqn_args",
  slots = c(
    dat_in = "character",
    dat_out = "character",
    ref_ids = "character_OR_NULL",
    min_frac = "numeric",
    type = "character"
  ),
  contains = "poplinArgs",
  prototype = prototype(
    dat_in = "raw",
    dat_out = "pqn",
    ref_ids = NULL,
    min_frac = 0.5,
    type = "mean"
  )
)
