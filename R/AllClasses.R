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
setClassUnion("numeric_OR_NULL", c("numeric", "NULL"))

## Argument class for PQN
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
    dat_out = "out",
    ref_ids = NULL,
    min_frac = 0.5,
    type = "mean"
  )
)

## argument class for cyclicloess normalization
setClass(
  "cyclicloess_args",
  slot = c(
    dat_in = "character",
    dat_out = "character",
    weights = "numeric_OR_NULL",
    span = "numeric",
    iterations = "integer",
    method = "character"
  ),
  contain = "poplinArgs",
  prototype = prototype(
    dat_in = "raw",
    dat_out = "out",
    weights = NULL,
    span = 0.7,
    iterations = 3L,
    method = "fast"
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
    normalizer = "sum",
    dat_in = "raw",
    dat_out = "out",
    restrict = TRUE,
    rescale = FALSE
  )
)

