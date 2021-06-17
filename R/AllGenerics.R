##' @export
setGeneric("missingCount", function(x) standardGeneric("missingCount"))

##' @export
setGeneric("missingCount<-", function(x, value) standardGeneric("missingCount<-"))

##' @export
setGeneric("poplinData", function(x) standardGeneric("poplinData"))

##' @export
setGeneric("poplinData<-", function(x, value) standardGeneric("poplinData<-"))

##' @export
setGeneric("poplinReducedData", function(x) standardGeneric("poplinReducedData"))

##' @export
setGeneric("poplinReducedData<-", function(x, value) standardGeneric("poplinReducedData<-"))


##' @export
setGeneric("imputedData", function(x, type, ...) standardGeneric("imputedData"))

##' @export
setGeneric("imputedData<-", function(x, type, check_dimnames=TRUE, ..., value) standardGeneric("imputedData<-"))

##' @export
setGeneric("imputedDataNames", function(x) standardGeneric("imputedDataNames"))

##' @export
setGeneric("imputedDataNames<-", function(x, value) standardGeneric("imputedDataNames<-"))

##' @export
setGeneric("imputedData", function(x, ...) standardGeneric("imputedData"))

##' @export
setGeneric("imputedData<-", function(x, check_dimnames=TRUE, ..., value) standardGeneric("imputedData<-"))
