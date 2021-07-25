.replace_empty_names <- function(names, N, msg, name_pattern) {
  if (is.null(names) && N > 0) {
    warning("'", msg, "' is NULL, replacing with '", name_pattern, "'")
    names <- paste0(name_pattern, seq_len(N))
  } else if (any(empty <- names=="")) {
    warning("'", msg, "' contains empty strings, replacing with '",
            name_pattern, "'")
    names[empty] <- paste0(name_pattern, seq_along(sum(empty)))
  }
  names
}

.check_dimnames <- function(reference, incoming, fun) {
  if (!is.null(incoming)) {
    rownames_incoming <- rownames(incoming)
    colnames_incoming <- colnames(incoming)
    rownames_reference <- rownames(reference)
    colnames_reference <- colnames(reference)
    if (!is.null(rownames_incoming)) {
      if (!identical(rownames_incoming, rownames_reference)) {
        stop(
          "non-NULL 'rownames(value)' should be the same as 'rownames(x)' for '",
          fun
        )
      }
    } else {
      tryCatch({
        rownames(incoming) <- rownames_reference
      }, error = function(e) {
        stop(
          "'value' should have number of rows equal to 'nrow(x)'",
          call. = FALSE
        )
      })
    }
    if (!is.null(colnames_incoming)) {
      if (!identical(colnames_incoming, colnames_reference)) {
        stop(
          "non-NULL 'colnames(value)' should be the same as 'colnames(x)' for '",
          fun
        )
      }
    } else {
      tryCatch({
        colnames(incoming) <- colnames_reference
      }, error = function(e) {
        stop(
          "'value' should have number of columns equal to 'ncol(x)'",
          call. = FALSE
        )
      })
    }
  }
  incoming
}

## See SummarizedExperiment vignettes (enabling subsetting operations)
.get_subset_index <- function(subset, names) {
  if (is.character(subset)) {
    fmt <- paste0("<", class(x), ">[i,] index out of bounds: %s")
    subset <- SummarizedExperiment:::.SummarizedExperiment.charbound(
                                       subset, names, fmt
                                     )
  }
  as.vector(subset)
}

