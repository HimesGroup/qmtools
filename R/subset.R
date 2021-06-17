## ##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) {
  x <- updateObject(x)
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    imputation(x) <- imputation(x)[ii, , drop=FALSE]
  }
  if (!missing(j)) {
    jj <- .get_subset_index(j, colnames(x))
    imputation(x) <- imputation(x)[, jj, drop=FALSE]
  }
  callNextMethod()
})



#' @importFrom methods as
#' @importFrom S4Vectors DataFrame I mcols mcols<- metadata metadata<- 
.set_internal_all <- function(x, value, getfun, setfun, key, convertfun, xdimfun, vdimfun, funstr, xdimstr, vdimstr) {
    x <- updateObject(x)

    if (length(value) == 0L) {
        collected <- getfun(x)[, 0]
    } else {
        original <- value

        if (!is.null(convertfun)) {
            value <- lapply(value, convertfun)
        }

        N <- vapply(value, vdimfun, 0L)
        if (!all(N == xdimfun(x))) {
            stop("invalid 'value' in '", funstr, "(<", class(x), ">) <- value'\n",
                "each element of 'value' should have number of ", vdimstr, " equal to '", xdimstr, "(x)'")
        }

        names(value) <- .clean_internal_names(names(value), N=length(value), msg="names(value)")
        collected <- do.call(DataFrame, c(lapply(value, I), list(row.names=NULL, check.names=FALSE)))

        if (is(original, "Annotated")) {
            metadata(collected) <- metadata(original)
        }
        if (is(original, "Vector")) {
            mcols(collected) <- mcols(original)
        }
    }

    tmp <- getfun(x)
    tmp[[key]] <- collected
    setfun(x, tmp)
}

.clean_internal_names <- function(names, N, msg) {
    if (is.null(names) && N > 0) {
        warning("'", msg, "' is NULL, replacing with 'unnamed'")
        names <- paste0(.unnamed, seq_len(N))
    } else if (any(empty <- names=="")) {
        warning("'", msg, "' contains empty strings, replacing with 'unnamed'")
        names[empty] <- paste0(.unnamed, seq_along(sum(empty)))
    }
    names
}


#' @export
setMethod("reducedDims", "SingleCellExperiment", function(x, withDimnames=TRUE) {
  value <- .get_internal_all(x, 
                             getfun=int_colData, 
                             key=.red_key)

  if (withDimnames) {
    for (i in seq_along(value)) {
      rownames(value[[i]]) <- colnames(x)
    }
  }
  value
})

#' @export
setReplaceMethod("reducedDims", "SingleCellExperiment", function(x, withDimnames=TRUE, ..., value) {
  if (withDimnames) {
    for (v in seq_along(value)) {
      .check_reddim_names(x, value[[v]], withDimnames=TRUE, 
                          vname=sprintf("value[[%s]]", v), fun='reducedDims')
    }
  }

  .set_internal_all(x, value, 
                    getfun=int_colData,
                    setfun=`int_colData<-`,
                    key=.red_key,
                    convertfun=NULL,
                    xdimfun=ncol,
                    vdimfun=nrow,
                    funstr="reducedDims",
                    xdimstr="ncol",
                    vdimstr="rows")
})
