.replace_empty_names <- function(names, N, msg, name_pattern) {
  if (is.null(names) && N > 0) {
    warning("'", msg, "' is NULL, replacing with '", name_pattern, "'")
    names <- paste0(name_pattern, seq_len(N))
  } else if (any(empty <- names=="")) {
    warning("'", msg, "' contains empty strings, replacing with '",
            name_pattern, "'")
    names[empty] <- paste0(name_pattern, seq_len(sum(empty)))
    ## names[empty] <- paste0(name_pattern, seq_along(sum(empty)))
  }
  names
}

.check_name_duplicates <- function(assay_names, value_names, msg1, msg2) {
  if (!is.null(value_names)) {
    ## Non-empty names must be unique
    if (anyDuplicated(value_names[value_names != ""])) {
      stop("'", msg1, "' contains duplicates. ", msg2)
    }
    if (any(value_names %in% assay_names)) {
      stop("'", msg1, "' must not overlap with assayNames(x). ", msg2)
    }
  }
}

##' @importFrom SummarizedExperiment assayNames
##' @export
.verify_and_extract_input <- function(x, name) {
  name_pool <- c(assayNames(x), poplin_data_names(x))
  if (!(name %in% name_pool)) {
    stop("data '", name, "' is not found in the poplin object.\n",
         "Input must be one of c(assayNames(x), poplin_data_names(x).")
  }
  tryCatch(
    assay(x, name),
    error = function(err) poplin_data(x, name)
  )
}

.poplin_extract_and_assign <- function(x, fun, poplin_in, poplin_out, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_data(x, poplin_out) <- fun(m, ...)
  x
}

.reduced_extract_and_assign <- function(x, fun, poplin_in, poplin_out, ...) {
  m <- .verify_and_extract_input(x, poplin_in)
  poplin_reduced(x, poplin_out) <- fun(m, ...)
  x
}

## `%||%` <- function(x, y) {
##   if (is.null(x)) y else x
## }
