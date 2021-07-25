##' @importClassesFrom S4Vectors SimpleList
##' @export
.get_poplinData_datalist <- function(x, get_slot, element) {
  ## x <- updateObject(x) # internal update for obsolete class but not necessary
  as(get_slot(x)[[element]], "SimpleList")
}

.get_poplinData_names <- function(x, get_slot, element) {
  colnames(get_slot(x)[[element]])
}

.set_poplinData_names <- function(x, value, get_slot, set_element_fun,
                              element, name_pattern) {
  poplin_slot <- get_slot(x)
  N <- ncol(poplin_slot[[element]])
  value <- .replace_empty_names(value, N, msg = "value",
                                name_pattern = name_pattern)
  colnames(poplin_slot[[element]]) <- value
  set_element_fun(x, poplin_slot)
}

##' @export
.get_poplinData_data_integer <- function(x, index, get_slot, element, funstr) {
  ## x <- updateObject(x)
  tmp <- get_slot(x)[[element]]

  tryCatch({
    tmp[, index]
  }, error = function(e) {
    stop("invalid subscript 'type' in '", funstr,
         "(<", class(x), ">, type=\"numeric\", ...)':\n  ",
         conditionMessage(e))
  })

}

##' @export
.get_poplinData_data_character <- function(x, index, get_slot, element,
                                           funstr, namestr) {
  ## x <- updateObject(x)
  tmp <- get_slot(x)[[element]]

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
.set_poplinData_data_integer <- function(x, type, value, get_slot, set_element_fun,
                                element, funstr) {
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
  if (type > ncol(tmp[[element]])) {
    stop("'type' out of bounds in '", funstr,
         "(<", class(x), ">, type='numeric')")
  }

  tmp[[element]][[type]] <- value
  set_element_fun(x, tmp)

}


##' @export
.set_poplinData_data_character <- function(x, type, value, get_slot,
                                           set_element_fun, element, funstr) {
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
  tmp[[element]][[type]] <- value
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
                                     element, funstr, name_pattern) {
  ## x <- updateObject(x)

  if (identical(length(value), 0L)) {
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

