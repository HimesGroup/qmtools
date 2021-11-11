## It is a bad practice to use {} in generic as it triggers a special case that
## is more expensive, and generally best avoided.

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
  function(x, method, ...) standardGeneric("poplin_normalize")
)

##' @export
setGeneric(
  "poplin_normalize_pqn",
  function(x, ...) standardGeneric("poplin_normalize_pqn")
)

##' @export
setGeneric(
  "poplin_normalize_sum",
  function(x, ...) standardGeneric("poplin_normalize_sum")
)

##' @export
setGeneric(
  "poplin_normalize_mean",
  function(x, ...) standardGeneric("poplin_normalize_mean")
)

##' @export
setGeneric(
  "poplin_normalize_median",
  function(x, ...) standardGeneric("poplin_normalize_median")
)

##' @export
setGeneric(
  "poplin_normalize_mad",
  function(x, ...) standardGeneric("poplin_normalize_mad")
)

##' @export
setGeneric(
  "poplin_normalize_euclidean",
  function(x, ...) standardGeneric("poplin_normalize_euclidean")
)

##' @export
setGeneric(
  "poplin_normalize_cyclicloess",
  function(x, ...) standardGeneric("poplin_normalize_cyclicloess")
)

##' @export
setGeneric(
  "poplin_normalize_auto",
  function(x, ...) standardGeneric("poplin_normalize_auto")
)

##' @export
setGeneric(
  "poplin_normalize_range",
  function(x, ...) standardGeneric("poplin_normalize_range")
)

##' @export
setGeneric(
  "poplin_normalize_pareto",
  function(x, ...) standardGeneric("poplin_normalize_pareto")
)

##' @export
setGeneric(
  "poplin_normalize_vast",
  function(x, ...) standardGeneric("poplin_normalize_vast")
)

##' @export
setGeneric(
  "poplin_normalize_level",
  function(x, ...) standardGeneric("poplin_normalize_level")
)

##' @export
setGeneric(
  "poplin_normalize_vsn",
  function(x, ...) standardGeneric("poplin_normalize_vsn")
)

#################################################################################
## Imputation Generics
#################################################################################

##' @export
setGeneric(
  "poplin_impute",
  function(x, method, ...) standardGeneric("poplin_impute")
)

##' @export
setGeneric(
  "poplin_impute_knn",
  function(x, ...) standardGeneric("poplin_impute_knn")
)

##' @export
setGeneric(
  "poplin_impute_halfmin",
  function(x, ...) standardGeneric("poplin_impute_halfmin")
)

##' @export
setGeneric(
  "poplin_impute_median",
  function(x, ...) standardGeneric("poplin_impute_median")
)

##' @export
setGeneric(
  "poplin_impute_mean",
  function(x, ...) standardGeneric("poplin_impute_mean")
)

##' @export
setGeneric(
  "poplin_impute_pca",
  function(x, ...) standardGeneric("poplin_impute_pca")
)

##' @export
setGeneric(
  "poplin_impute_randomforest",
  function(x, ...) standardGeneric("poplin_impute_randomforest")
)

#################################################################################
## Reduction Generics
#################################################################################

##' @export
setGeneric(
  "poplin_reduce",
  function(x, method, ...) standardGeneric("poplin_reduce")
)

##' @export
setGeneric(
  "poplin_reduce_pca",
  function(x, ...) standardGeneric("poplin_reduce_pca")
)

##' @export
setGeneric(
  "poplin_reduce_tsne",
  function(x, ...) standardGeneric("poplin_reduce_tsne")
)

##' @export
setGeneric(
  "poplin_reduce_plsda",
  function(x, ...) standardGeneric("poplin_reduce_plsda")
)
