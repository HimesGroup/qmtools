setMethod(
  "poplin_impute",
  "matrix",
  function(x, method, ...) {
    .poplin_impute(x, method = method, ...)
  }
)

setMethod(
  "poplin_impute",
  "poplin",
  function(x, method, poplin_in, poplin_out, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_data(x, poplin_out) <- .poplin_impute(m, method = method, ...)
    x
  }
)

setMethod(
  "poplin_impute_knn",
  "matrix",
  function(x, ...) {
    .poplin_impute_knn(x, ...)
  }
)

setMethod(
  "poplin_impute_knn",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_knn,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_impute_halfmin",
  "matrix",
  function(x, ...) {
    .poplin_impute_halfmin(x, ...)
  }
)

setMethod(
  "poplin_impute_halfmin",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_halfmin,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_impute_median",
  "matrix",
  function(x, ...) {
    .poplin_impute_median(x, ...)
  }
)

setMethod(
  "poplin_impute_median",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_median,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_impute_mean",
  "matrix",
  function(x, ...) {
    .poplin_impute_mean(x, ...)
  }
)

setMethod(
  "poplin_impute_mean",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_mean,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_impute_pca",
  "matrix",
  function(x, ...) {
    .poplin_impute_pca(x, ...)
  }
)

setMethod(
  "poplin_impute_pca",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_pca,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_impute_randomforest",
  "matrix",
  function(x, ...) {
    .poplin_impute_randomforest(x, ...)
  }
)

setMethod(
  "poplin_impute_randomforest",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_extract_and_assign(x, .poplin_impute_randomforest,
                               poplin_in, poplin_out, ...)
  }
)
