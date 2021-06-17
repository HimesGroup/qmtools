##' @importClassesFrom S4Vectors SimpleList
##' @export
.get_poplin_element <- function(x, get_slot, element) {
  ## x <- updateObject(x) # internal update for obsolete class but not necessary
  as(get_slot(x)[[element]], "SimpleList")
}

.get_poplin_names <- function(x, get_slot, element) {
  colnames(get_slot(x)[[element]])
}

.set_poplin_names <- function(x, value, get_slot, set_element_fun,
                              element, name_pattern) {
  poplin_slot <- get_slot(x)
  N <- ncol(poplin_slot[[element]])
  value <- .replace_empty_names(value, N, msg = "value",
                                name_pattern = name_pattern)
  colnames(poplin_slot[[element]]) <- value
  set_element_fun(x, poplin_slot)
}


##' @export
##' @importFrom methods as
##' @importFrom S4Vectors DataFrame I mcols mcols<- metadata metadata<-
.set_poplin_element <- function(x, value, get_slot, set_element_fun, element,
                                funstr, name_pattern
                                ) {
  ## x <- updateObject(x)

  if (length(value) == 0L) {
    collected <- get_slot(x)[, 0] # DataFrame with 0 column
  } else {
    original <- value

    N_row <- vapply(value, nrow, 0L) # ensure integer of length 1
    N_col <- vapply(value, ncol, 0L) # ensure integer of length 1
    if (!all(N_row == nrow(x))) {
      stop(
        "invalid 'value' in '", funstr, "(<", class(x), ">) <- value'\n",
        "each element of 'value' should have number of rows equal to 'nrow(x)'"
      )
    }

    if (!all(N_col == ncol(x))) {
      stop(
        "invalid 'value' in '", funstr, "(<", class(x), ">) <- value'\n",
        "each element of 'value' should have number of columns equal to 'ncol(x)'"
      )
    }

    names(value) <- .replace_empty_names(
      names(value), N = length(value), msg = "names(value)",
      name_pattern = name_pattern
    )
    collected <- do.call(
      DataFrame,
      c(lapply(value, I), list(row.names=NULL, check.names=FALSE))
    )

    ## Transfer metadata
    if (is(original, "Annotated")) {
      metadata(collected) <- metadata(original)
    }
    if (is(original, "Vector")) {
      mcols(collected) <- mcols(original)
    }
  }

  tmp <- get_slot(x)
  tmp[[element]] <- collected
  set_element_fun(x, tmp)
}


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
    }
    if (!is.null(colnames_incoming)) {
      if (!identical(colnames_incoming, colnames_reference)) {
        stop(
          "non-NULL 'colnames(value)' should be the same as 'colnames(x)' for '",
          fun
        )
      }
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

