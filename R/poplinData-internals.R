##' @importClassesFrom S4Vectors SimpleList
##' @export
## .get_poplinData_datalist <- function(x, get_slot, element) {
##   ## x <- updateObject(x) # internal update for obsolete class but not necessary
##   as(get_slot(x)[[element]], "SimpleList")
## }

.get_poplinData_names <- function(x, get_slot) {
  colnames(get_slot(x))
}

.set_poplinData_names <- function(x, value, get_slot, set_element_fun,
                                  name_pattern) {
  poplin_slot <- get_slot(x)
  N <- ncol(poplin_slot)
  value <- .replace_empty_names(value, N, msg = "value",
                                name_pattern = name_pattern)
  colnames(poplin_slot) <- value
  set_element_fun(x, poplin_slot)
}

##' @export
.get_poplinData_data_integer <- function(x, index, get_slot, funstr) {
  ## x <- updateObject(x)
  tmp <- get_slot(x)

  tryCatch({
    tmp[, index]
  }, error = function(e) {
    stop("invalid subscript 'type' in '", funstr,
         "(<", class(x), ">, type=\"numeric\", ...)':\n  ",
         conditionMessage(e))
  })

}

##' @export
.get_poplinData_data_character <- function(x, index, get_slot,
                                           funstr, namestr) {

  ## x <- updateObject(x)
  tmp <- get_slot(x)

  tryCatch({
    tmp[, index]
  }, error = function(e) {
    stop("invalid subscript 'type' in '", funstr,
         "(<", class(x), ">, type=\"character\", ...)':\n  ",
         "'", index, "' not in '", namestr, "(<", class(x), ">)'")
  })

}

##' @export
.get_poplinData_data_missing <- function(x, base_fun, name_fun, funstr, ...) {
  if (identical(length(name_fun(x)), 0L)) {
    stop("no available entries for '", funstr, "(<", class(x), ">, ...)'")
  }
  base_fun(x, 1L, ...) # fallback to numeric type; retrieve the first data
}

##' @export
.set_poplinData_data_integer <- function(x, type, value, get_slot,
                                         set_element_fun, funstr) {
  ## x <- updateObject(x)

  if (length(type) != 1L) {
    stop("attempt to replace more than one element")
  }

  if (!is.null(value)) {
    ## This dim assertion may be redundant as we pre-check dimnames
    if (!identical(nrow(value), nrow(x))) {
      stop("invalid 'value' in '",
           funstr, "(<", class(x), ">, type=\"numeric\") <- value':\n  ",
           "'value' should have number of rows equal to 'nrow(x)'")
    }
    if (!identical(ncol(value), ncol(x))) {
      stop("invalid 'value' in '",
           funstr, "(<", class(x), ">, type=\"numeric\") <- value':\n  ",
           "'value' should have number of columns equal to 'ncol(x)'")
    }
  }

  tmp <- get_slot(x)
  if (type > ncol(tmp)) {
    stop("'type' out of bounds in '", funstr,
         "(<", class(x), ">, type='numeric')")
  }

  tmp[[type]] <- value
  set_element_fun(x, tmp)

}


##' @export
.set_poplinData_data_character <- function(x, type, value, get_slot,
                                           set_element_fun, funstr) {
  ## x <- updateObject(x)

  if (length(type) != 1L) {
    stop("attempt to replace more than one element")
  }

  if (!is.null(value)) {
    ## This dim assertion may be redundant as we pre-check dimnames
    if (!identical(nrow(value), nrow(x))) {
      stop("invalid 'value' in '",
           funstr, "(<", class(x), ">, type=\"character\") <- value':\n  ",
           "'value' should have number of rows equal to 'nrow(x)'")
    }
    if (!identical(ncol(value), ncol(x))) {
      stop("invalid 'value' in '",
           funstr, "(<", class(x), ">, type=\"character\") <- value':\n  ",
           "'value' should have number of columns equal to 'ncol(x)'")
    }
  }

  tmp <- get_slot(x)
  tmp[[type]] <- value
  set_element_fun(x, tmp)

}


##' @export
.set_poplinData_data_missing <- function(x, value, ..., base_fun, name_fun,
                                         name_pattern) {
  if (length(name_fun(x))) {
    ## replace the first entries
    type <- 1L
  } else {
    ## if no data is available, set it to the first
    type <- paste0(name_pattern, 1L)
  }
  base_fun(x, type, ..., value = value)
}


##' @export
##' @importFrom methods as
##' @importFrom S4Vectors DataFrame I mcols mcols<- metadata metadata<-
.set_poplinData_datalist <- function(x, value, get_slot, set_element_fun,
                                     funstr, name_pattern) {
  ## x <- updateObject(x)

  if (identical(length(value), 0L)) {
    collected <- get_slot(x)[, 0] # DataFrame with 0 column
  } else {
    original <- value

    N_row <- vapply(value, nrow, 0L) # ensure integer of length 1
    N_col <- vapply(value, ncol, 0L) # ensure integer of length 1

    ## The following assertion may not be necessary as we pre-check dimension
    ## via .check_dimnames
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

  ## tmp <- get_slot(x)
  ## tmp[[element]] <- collected
  set_element_fun(x, collected)
}

.check_dimnames <- function(reference, incoming, fun) {
  if (!is.null(incoming)) {
    if (!(identical(dim(reference), dim(incoming)))) {
      stop("'value' should have the dimension same as 'dim(x)'")
    }
    ## if (!(identical(nrow(reference), nrow(incoming)))) {
    ##   stop("'value' should have number of rows equal to 'nrow(x)'")
    ## }
    ## if (!(identical(ncol(reference), ncol(incoming)))) {
    ##   stop("'value' should have number of columns equal to 'ncol(x)'")
    ## }
    rownames_incoming <- rownames(incoming)
    colnames_incoming <- colnames(incoming)
    rownames_reference <- rownames(reference)
    colnames_reference <- colnames(reference)
  ##   if (!is.null(rownames_incoming) &&
  ##       !identical(rownames_incoming, rownames_reference)) {
  ##     stop(
  ##       "non-NULL 'rownames(value)' should be the same as 'rownames(x)' for '",
  ##       fun
  ##     )
  ##   }
  ##   if (!is.null(colnames_incoming) &&
  ##       !identical(colnames_incoming, colnames_reference)) {
  ##     stop(
  ##       "non-NULL 'colnames(value)' should be the same as 'colnames(x)' for '",
  ##       fun
  ##     )
  ##   }
  ## }
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
        stop("'value' should have number of rows equal to 'nrow(x)'")
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
        stop("'value' should have number of columns equal to 'ncol(x)'")
      })
    }
  }
  incoming
}

