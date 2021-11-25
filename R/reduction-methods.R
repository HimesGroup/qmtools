##' Dimension reduction methods
##'
##' @name poplin_reduce
setMethod(
  "poplin_reduce",
  "matrix",
  function(x, method, ...) {
    .poplin_reduce(x, method = method, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce",
  "poplin",
  function(x, method, poplin_in, poplin_out, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_reduced(x, poplin_out) <- .poplin_reduce(m, method = method, ...)
    x
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_pca",
  "matrix",
  function(x, ...) {
    .poplin_impute_pca(x, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_pca",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_pca,
                               poplin_in, poplin_out, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_tsne",
  "matrix",
  function(x, ...) {
    .poplin_impute_tsne(x, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_tsne",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_tsne,
                                poplin_in, poplin_out, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_plsda",
  "matrix",
  function(x, ...) {
    .poplin_impute_plsda(x, ...)
  }
)

##' @rdname poplin_reduce
setMethod(
  "poplin_reduce_plsda",
  "poplin",
  function(x, poplin_in, poplin_out, ...) {
    .reduced_extract_and_assign(x, .poplin_reduce_plsda,
                                poplin_in, poplin_out, ...)
  }
)




