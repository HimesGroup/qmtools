#################################################################################
## Taken from SingleCellExperiment package (09/18/2021)
## Just rename it to poplin.class and extend it for print functions
#################################################################################

poplin.matrix <- function(x, tag, ...) {
  class(x) <- c(tag, "poplin.matrix", "matrix")
  mostattributes(x) <- c(attributes(x), list(...))
  x
}

## Register as S4 class; not necessary?
##' @rawNamespace exportClasses(poplin.matrix)
setOldClass(c("poplin.matrix", "matrix"))

##' @export
`[.poplin.matrix` <- function(x, i, j, ..., drop=FALSE) {
  at <- attributes(x)
  out <- NextMethod()
  ## Keep matrix attributes when subset
  if (!is.null(dim(out))) {
    at <- at[setdiff(names(at), c("dim", "dimnames"))]
    mostattributes(out) <- c(attributes(out), at)
  }
  out
}

.check_reddim_attributes <- function(available) {
  all.attr <- lapply(available, attributes)

  ## Extract all attributes
  ## Ignore dim and dimnames
  for (i in seq_along(all.attr)) {
    current <- all.attr[[i]]
    all.attr[[i]] <- current[setdiff(names(current), c("dim", "dimnames"))]
  }

  ## Make sure custom attributes are the same; otherwise they won't be combined
  u.attr <- unique(all.attr)
  if (length(u.attr) > 1) {
    warning("mismatched custom attributes when combining 'poplin.matrix' objects")
    NULL
  } else {
    ## Singular
    u.attr[[1]]
  }
}


##' @export
##' @method rbind poplin.matrix
rbind.poplin.matrix <- function(..., deparse.level = 1) {
  available <- list(...)
  u.attr <- .check_reddim_attributes(available)

  available <- lapply(available, unclass)
  out <- do.call(rbind, available)

  mostattributes(out) <- c(attributes(out), u.attr)
  out
}

##' @export
##' @method cbind poplin.matrix
cbind.poplin.matrix <- function(..., deparse.level = 1) {
  available <- list(...)
  u.attr <- .check_reddim_attributes(available)

  available <- lapply(available, unclass)
  out <- do.call(cbind, available)

  mostattributes(out) <- c(attributes(out), u.attr)
  out
}

## poplin.matrix.pca <- function(x, ...) {
##   x <- poplin.matrix(x)
##   class(x) <- c("poplin.matrix.pca", "poplin.matrix", "matrix")
##   x
## }

##' @export
summary.poplin.pca <- function(x, ...) {
  cat("Call:", deparse(attr(x, "call")), "\n")
  cat("Reduction method:", attr(x, "method"), "\n")
  cat("Input dimension: [",
      attr(x, "origD")[1], ", ",
      attr(x, "origD")[2], "]\n", sep = "")
  cat("Input centered before PCA:", attr(x, "centered"), "\n")
  cat("Input scaled before PCA:", attr(x, "scaled"), "\n")
  cat("Number of PCs calculated:", attr(x, "ncomp"), "\n")
  cat("Importance of PC(s):\n")
  imp <- rbind(attr(x, "R2"), attr(x, "R2cum"))
  rownames(imp) <- c("Proportion of Variance",
                     "Cumulative Proportion")
  colnames(imp) <- paste0("PC", 1:attr(x, "ncomp"))
  print(imp, digits = 4)
  invisible(imp)
}

##' @export
print.poplin.matrix <- function(x, ...) {
  at <- attributes(x)
  custom_at <- setdiff(names(at), c("dim", "dimnames"))
  for (i in custom_at) {
    attr(x, i) <- NULL
  }
  print.default(x)
}

## ##' @export
## poplin.tsne <- function(x, ...) {
##   x <- poplin.matrix(x)
##   class(x) <- c("poplin.matrix.tsne", "poplin.matrix", "matrix")
##   x
## }


##' @export
summary.poplin.tsne <- function(x, ...) {
  cat("Call:", deparse(attr(x, "call")), "\n")
  cat("Reduction method:", attr(x, "method"), "\n")
  cat("Input dimension: [",
      attr(x, "origD")[1], ", ",
      attr(x, "origD")[2], "]\n", sep = "")
  cat("Input normalized before t-SNE:", attr(x, "normalized"), "\n")
  cat("Dimension of the embedded spcae:", attr(x, "ncomp"), "\n")
  cat("Perplexity:", attr(x, "perplexity"), "\n")
  cat("Theta:", attr(x, "theta"), "\n")
  cat("Eta:", attr(x, "eta"), "\n")
  invisible(x)
}

