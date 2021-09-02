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
  "poplin_data_list",
  function(x, ...) standardGeneric("poplin_data_list")
)

##' @export
setGeneric(
  "poplin_data_list<-",
  function(x, check_dimnames = TRUE, ..., value) standardGeneric("poplin_data_list<-")
)

##' @export
setGeneric(
  "poplin_data_names",
  function(x) standardGeneric("poplin_data_names")
)

##' @export
setGeneric(
  "poplin_data_names<-",
  function(x, value) standardGeneric("poplin_data_names<-")
)

##' @export
setGeneric(
  "poplin_data",
  function(x, type, ...) standardGeneric("poplin_data")
)

##' @export
setGeneric(
  "poplin_data<-",
  function(x, type, check_dimnames = TRUE, ..., value) standardGeneric("poplin_data<-")
)



## ##' @export
## setGeneric(
##   "reducedDataList",
##   function(x, ...) standardGeneric("reducedDataList")
## )

## ##' @export
## setGeneric(
##   "reducedDataList<-",
##   function(x, check_samplenames = TRUE, ..., value) standardGeneric("reducedDataList<-")
## )

## ##' @export
## setGeneric(
##   "reducedDataNames",
##   function(x) standardGeneric("reducedDataNames")
## )

## ##' @export
## setGeneric(
##   "reducedDataNames<-",
##   function(x, value) standardGeneric("reducedDataNames<-")
## )


## ##' @export
## setGeneric(
##   "reducedData",
##   function(x, type, ...) standardGeneric("reducedData")
## )

## ##' @export
## setGeneric(
##   "reducedData<-",
##   function(x, type, check_samplenames = TRUE, ..., value) standardGeneric("reducedData<-")
## )


