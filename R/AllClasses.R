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
    dat_out = "normalized",
    ref_ids = NULL,
    min_frac = 0.5,
    type = "mean"
  )
)

## Argument class for other sample-based normalization
setClass(
  "sample_normalizer_args",
  slot = c(
    normalizer = "character",
    dat_in = "character",
    dat_out = "character",
    restrict = "logical",
    rescale = "logical"
  ),
  contain = "poplinArgs",
  prototype = prototype(
    normalizer = "tic",
    dat_in = "raw",
    dat_out = "normalized",
    restrict = TRUE,
    rescale = FALSE
  )
)
