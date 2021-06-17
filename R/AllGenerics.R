##' @export
setGeneric("imputation", function(x) standardGeneric("imputation"))

##' @export
setGeneric("imputation<-", function(x, value) standardGeneric("imputation<-"))


##' @export
setGeneric("poplin_impute", function(x, type, ...) standardGeneric("poplin_impute"))

##' @export
setGeneric("poplin_impute<-", function(x, type, withDimnames=TRUE, ..., value) standardGeneric("poplin_impute<-"))

##' @export
setGeneric("poplin_impute_names", function(x) standardGeneric("poplin_impute_names"))

##' @export
setGeneric("poplin_impute_names<-", function(x, value) standardGeneric("poplin_impute_names<-"))

##' @export
setGeneric("poplin_impute", function(x, ...) standardGeneric("poplin_impute"))

##' @export
setGeneric("poplin_impute<-", function(x, withDimnames=TRUE, ..., value) standardGeneric("poplin_impute<-"))
