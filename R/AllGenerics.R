## It is a bad practice to use {} in generic as it triggers a special case that
## is more expensive, and generally best avoided.

##'@export
setGeneric("normalizeIntensity", function(x, ...)
    standardGeneric("normalizeIntensity"))

##' @export
setGeneric("imputeIntensity", function(x, ...)
    standardGeneric("imputeIntensity"))

##' @export
setGeneric("reduceFeatures", function(x, ...)
    standardGeneric("reduceFeatures")
    )

##' @export
setGeneric("removeFeatures", function(x, ...)
    standardGeneric("removeFeatures")
    )

