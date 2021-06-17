## ##' @export
setMethod("[", c("poplin", "ANY", "ANY"), function(x, i, j, ..., drop=TRUE) {
  x <- updateObject(x)
  if (!missing(i)) {
    ii <- .get_subset_index(i, rownames(x))
    ## poplinData(x) <- poplinData(x)[ii, , drop=FALSE]
  }
  if (!missing(j)) {
    jj <- .get_subset_index(j, colnames(x))
    ## poplinData(x) <- poplinData(x)[, jj, drop=FALSE]
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
