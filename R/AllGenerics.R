## It is a bad practice to use {} in generic as it triggers a special case that
## is more expensive, and generally best avoided.

##' @export
setGeneric(
  "missingCount",
  function(x) standardGeneric("missingCount")
)

##' @export
setGeneric(
  "missingCount<-",
  function(x, value) standardGeneric("missingCount<-")
)

##' @export
setGeneric(
  "poplinData",
  function(x) standardGeneric("poplinData")
)

##' @export
setGeneric(
  "poplinData<-",
  function(x, value) standardGeneric("poplinData<-")
)

##' @export
setGeneric(
  "poplinReducedData",
  function(x) standardGeneric("poplinReducedData")
)

##' @export
setGeneric(
  "poplinReducedData<-",
  function(x, value) standardGeneric("poplinReducedData<-")
)


##' @export
setGeneric(
  "imputedDataList",
  function(x, ...) standardGeneric("imputedDataList")
)

##' @export
setGeneric(
  "imputedDataList<-",
  function(x, check_dimnames = TRUE, ..., value) standardGeneric("imputedDataList<-")
)

##' @export
setGeneric(
  "imputedDataNames",
  function(x) standardGeneric("imputedDataNames")
)

##' @export
setGeneric(
  "imputedDataNames<-",
  function(x, value) standardGeneric("imputedDataNames<-")
)

##' @export
setGeneric(
  "imputedData",
  function(x, type, ...) standardGeneric("imputedData")
)

##' @export
setGeneric(
  "imputedData<-",
  function(x, type, check_dimnames = TRUE, ..., value) standardGeneric("imputedData<-")
)


##' @export
setGeneric(
  "normalizedDataList",
  function(x, ...) standardGeneric("normalizedDataList")
)

##' @export
setGeneric(
  "normalizedDataList<-",
  function(x, check_dimnames = TRUE, ..., value) standardGeneric("normalizedDataList<-")
)

##' @export
setGeneric(
  "normalizedDataNames",
  function(x) standardGeneric("normalizedDataNames")
)

##' @export
setGeneric(
  "normalizedDataNames<-",
  function(x, value) standardGeneric("normalizedDataNames<-")
)

##' @export
setGeneric(
  "normalizedData",
  function(x, type, ...) standardGeneric("normalizedData")
)

##' @export
setGeneric(
  "normalizedData<-",
  function(x, type, check_dimnames = TRUE, ..., value) standardGeneric("normalizedData<-")
)


##' @export
setGeneric(
  "reducedDataList",
  function(x, ...) standardGeneric("reducedDataList")
)

##' @export
setGeneric(
  "reducedDataList<-",
  function(x, check_dimnames = TRUE, ..., value) standardGeneric("reducedDataList<-")
)

##' @export
setGeneric(
  "reducedDataNames",
  function(x) standardGeneric("reducedDataNames")
)

##' @export
setGeneric(
  "reducedDataNames<-",
  function(x, value) standardGeneric("reducedDataNames<-")
)

