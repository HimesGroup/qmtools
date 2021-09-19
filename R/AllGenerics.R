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

#################################################################################
## poplinData and poplinReducedData
#################################################################################

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


##' @export
setGeneric(
  "poplin_reduced_list",
  function(x, ...) standardGeneric("poplin_reduced_list")
)

##' @export
setGeneric(
  "poplin_reduced_list<-",
  function(x, check_samplenames = TRUE, ..., value) standardGeneric("poplin_reduced_list<-")
)

##' @export
setGeneric(
  "poplin_reduced_names",
  function(x) standardGeneric("poplin_reduced_names")
)

##' @export
setGeneric(
  "poplin_reduced_names<-",
  function(x, value) standardGeneric("poplin_reduced_names<-")
)

##' @export
setGeneric(
  "poplin_reduced",
  function(x, type, ...) standardGeneric("poplin_reduced")
)

##' @export
setGeneric(
  "poplin_reduced<-",
  function(x, type, check_samplenames = TRUE, ..., value) standardGeneric("poplin_reduced<-")
)

#################################################################################
## Normalization Generics
#################################################################################
##'@export
setGeneric(
  "poplin_normalize",
  function(x, normalizer, ...) standardGeneric("poplin_normalize")
)

setGeneric(
  "poplin_normalize_pqn",
  function(x, ...) standardGeneric("poplin_normalize_pqn")
)

setGeneric(
  "poplin_normalize_sum",
  function(x, ...) standardGeneric("poplin_normalize_sum")
)

setGeneric(
  "poplin_normalize_mean",
  function(x, ...) standardGeneric("poplin_normalize_mean")
)

setGeneric(
  "poplin_normalize_median",
  function(x, ...) standardGeneric("poplin_normalize_median")
)

setGeneric(
  "poplin_normalize_mad",
  function(x, ...) standardGeneric("poplin_normalize_mad")
)

setGeneric(
  "poplin_normalize_euclidean",
  function(x, ...) standardGeneric("poplin_normalize_euclidean")
)

setGeneric(
  "poplin_normalize_cyclicloess",
  function(x, ...) standardGeneric("poplin_normalize_cyclicloess")
)

setGeneric(
  "poplin_normalize_auto",
  function(x, ...) standardGeneric("poplin_normalize_auto")
)

setGeneric(
  "poplin_normalize_range",
  function(x, ...) standardGeneric("poplin_normalize_range")
)

setGeneric(
  "poplin_normalize_pareto",
  function(x, ...) standardGeneric("poplin_normalize_pareto")
)

setGeneric(
  "poplin_normalize_vast",
  function(x, ...) standardGeneric("poplin_normalize_vast")
)

setGeneric(
  "poplin_normalize_level",
  function(x, ...) standardGeneric("poplin_normalize_level")
)

setGeneric(
  "poplin_normalize_vsn",
  function(x, ...) standardGeneric("poplin_normalize_vsn")
)

#################################################################################
## Imputation Generics
#################################################################################
setGeneric(
  "poplin_impute",
  function(x, method, ...) standardGeneric("poplin_impute")
)

setGeneric(
  "poplin_impute_knn",
  function(x, ...) standardGeneric("poplin_impute_knn")
)

setGeneric(
  "poplin_impute_halfmin",
  function(x, ...) standardGeneric("poplin_impute_halfmin")
)

setGeneric(
  "poplin_impute_median",
  function(x, ...) standardGeneric("poplin_impute_median")
)

setGeneric(
  "poplin_impute_mean",
  function(x, ...) standardGeneric("poplin_impute_mean")
)

setGeneric(
  "poplin_impute_pca",
  function(x, ...) standardGeneric("poplin_impute_pca")
)

setGeneric(
  "poplin_impute_randomforest",
  function(x, ...) standardGeneric("poplin_impute_randomforest")
)
