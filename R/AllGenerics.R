## It is a bad practice to use {} in generic as it triggers a special case that
## is more expensive, and generally best avoided.

#################################################################################
## poplinData
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

#################################################################################
## poplinReduced
#################################################################################

##' @export
setGeneric(
  "poplinReduced",
  function(x) standardGeneric("poplinReduced")
)

##' @export
setGeneric(
  "poplinReduced<-",
  function(x, value) standardGeneric("poplinReduced<-")
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
## poplinRaw
#################################################################################

##' @export
setGeneric(
  "poplin_raw_list",
  function(x, ...) standardGeneric("poplin_raw_list")
)

##' @export
setGeneric(
  "poplin_raw_list<-",
  function(x, check_dimnames = TRUE, ..., value) standardGeneric("poplin_raw_list<-")
)

##' @export
setGeneric(
  "poplin_raw_names",
  function(x, ...) standardGeneric("poplin_raw_names")
)

##' @export
setGeneric(
  "poplin_raw_names<-",
  function(x, ..., value) standardGeneric("poplin_raw_names<-")
)

##' @export
setGeneric(
  "poplin_raw",
  function(x, type, ...) standardGeneric("poplin_raw")
)

##' @export
setGeneric(
  "poplin_raw<-",
  function(x, type, check_dimnames = TRUE, ..., value) standardGeneric("poplin_raw<-")
)

#################################################################################
## Normalization Generics
#################################################################################
##'@export
setGeneric(
  "poplin_normalize",
  function(x, ...) standardGeneric("poplin_normalize")
)

##' @export
setGeneric(
  "normalize_pqn",
  function(x, ...) standardGeneric("normalize_pqn")
)

##' @export
setGeneric(
  "normalize_sum",
  function(x, ...) standardGeneric("normalize_sum")
)

##' @export
setGeneric(
  "normalize_mean",
  function(x, ...) standardGeneric("normalize_mean")
)

##' @export
setGeneric(
  "normalize_median",
  function(x, ...) standardGeneric("normalize_median")
)

##' @export
setGeneric(
  "normalize_mad",
  function(x, ...) standardGeneric("normalize_mad")
)

##' @export
setGeneric(
  "normalize_cyclicloess",
  function(x, ...) standardGeneric("normalize_cyclicloess")
)

##' @export
setGeneric(
  "normalize_vsn",
  function(x, ...) standardGeneric("normalize_vsn")
)

##' @export
setGeneric(
  "normalize_scale",
  function(x, ...) standardGeneric("normalize_scale")
)


#################################################################################
## Imputation Generics
#################################################################################

##' @export
setGeneric(
  "poplin_impute",
  function(x, ...) standardGeneric("poplin_impute")
)

##' @export
setGeneric(
  "impute_knn",
  function(x, ...) standardGeneric("impute_knn")
)

##' @export
setGeneric(
  "impute_univariate",
  function(x, ...) standardGeneric("impute_univariate")
)

##' @export
setGeneric(
  "impute_pca",
  function(x, ...) standardGeneric("impute_pca")
)

##' @export
setGeneric(
  "impute_randomforest",
  function(x, ...) standardGeneric("impute_randomforest")
)

#################################################################################
## Reduction Generics
#################################################################################

##' @export
setGeneric(
  "poplin_reduce",
  function(x, ...) standardGeneric("poplin_reduce")
)

##' @export
setGeneric(
  "reduce_pca",
  function(x, ...) standardGeneric("reduce_pca")
)

##' @export
setGeneric(
  "reduce_tsne",
  function(x, ...) standardGeneric("reduce_tsne")
)

##' @export
setGeneric(
  "reduce_plsda",
  function(x, ...) standardGeneric("reduce_plsda")
)
