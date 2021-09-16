setMethod(
  "poplin_normalize",
  "matrix",
  function(x, normalizer, ...) {
    .poplin_normalize(x, normalizer = normalizer, ...)
  }
)

setMethod(
  "poplin_normalize",
  "poplin",
  function(x, normalizer, poplin_in, poplin_out, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_data(x, poplin_out) <- .poplin_normalize(m, normalizer = normalizer, ...)
    x
  }
)

.poplin_class_normlization <- function(x, fun, poplin_in, poplin_out, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_data(x, poplin_out) <- fun(m, ...)
  x
}

setMethod(
  "poplin_normalize_pqn",
  "matrix",
  function(x, ...) {
    .poplin_normalize_pqn(x, ...)
  }
)

setMethod(
  "poplin_normalize_pqn",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_pqn,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_sum",
  "matrix",
  function(x, ...) {
    .poplin_normalize_sum(x, ...)
  }
)

setMethod(
  "poplin_normalize_sum",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_sum,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_mean",
  "matrix",
  function(x, ...) {
    .poplin_normalize_mean(x, ...)
  }
)

setMethod(
  "poplin_normalize_mean",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_mean,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_median",
  "matrix",
  function(x, ...) {
    .poplin_normalize_median(x, ...)
  }
)

setMethod(
  "poplin_normalize_median",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_median,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_mad",
  "matrix",
  function(x, ...) {
    .poplin_normalize_mad(x, ...)
  }
)

setMethod(
  "poplin_normalize_mad",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_mad,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_euclidean",
  "matrix",
  function(x, ...) {
    .poplin_normalize_euclidean(x, ...)
  }
)

setMethod(
  "poplin_normalize_euclidean",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_euclidean,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_cyclicloess",
  "matrix",
  function(x, ...) {
    .poplin_normalize_cyclicloess(x, ...)
  }
)

setMethod(
  "poplin_normalize_cyclicloess",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_cyclicloess,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_auto",
  "matrix",
  function(x, ...) {
    .poplin_normalize_auto(x, ...)
  }
)

setMethod(
  "poplin_normalize_auto",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_auto,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_range",
  "matrix",
  function(x, ...) {
    .poplin_normalize_range(x, ...)
  }
)

setMethod(
  "poplin_normalize_range",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_range,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_pareto",
  "matrix",
  function(x, ...) {
    .poplin_normalize_pareto(x, ...)
  }
)

setMethod(
  "poplin_normalize_pareto",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_pareto,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_vast",
  "matrix",
  function(x, ...) {
    .poplin_normalize_vast(x, ...)
  }
)

setMethod(
  "poplin_normalize_vast",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_vast,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_level",
  "matrix",
  function(x, ...) {
    .poplin_normalize_level(x, ...)
  }
)

setMethod(
  "poplin_normalize_level",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_level,
                               poplin_in, poplin_out, ...)
  }
)

setMethod(
  "poplin_normalize_vsn",
  "matrix",
  function(x, ...) {
    .poplin_normalize_vsn(x, ...)
  }
)

setMethod(
  "poplin_normalize_vsn",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .poplin_class_normlization(x, .poplin_normalize_vsn,
                               poplin_in, poplin_out, ...)
  }
)
