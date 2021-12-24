#################################################################################
## Taken from SingleCellExperiment package (09/18/2021)
## Just rename it to poplin.class and extend it for print functions
#################################################################################
##' The poplin.matrix class
##'
##' A matrix class that retains its attributes upon being subsetted or combined.
##' It is a simply renamed version of \code{reduced.dim.matrix} in the
##' \pkg{SingleCellExperiment}. This is useful for storing metadata about a
##' dimensionality reduction result alongside the matrix, and for ensuring that
##' the metadata persists when the matrix is stored inside
##' [poplin_reduced_list].
##'
##' @references
##'
##' Amezquita, R.A., Lun, A.T.L., Becht, E. et al. Orchestrating single-cell
##' analysis with Bioconductor. Nat Methods 17, 137–145 (2020).
##' https://doi.org/10.1038/s41592-019-0654-x
##'
##' @name poplin.matrix
##' @aliases
##' poplin.matrix
##' poplin.matrix-class
##' [.poplin.matrix
##' rbind.poplin.matrix
##' cbind.poplin.matrix
##' @keywords internal
NULL


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
summary.poplin.pca <- function(object, ...) {
  cat("Call:", deparse(attr(object, "call")), "\n")
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("Input dimension: [",
      attr(object, "origD")[1], ", ",
      attr(object, "origD")[2], "]\n", sep = "")
  cat("Input centered before PCA:", attr(object, "centered"), "\n")
  cat("Input scaled before PCA:", attr(object, "scaled"), "\n")
  cat("Number of PCs calculated:", attr(object, "ncomp"), "\n")
  cat("Importance of PC(s):\n")
  imp <- rbind(attr(object, "R2"), attr(object, "R2cum"))
  rownames(imp) <- c("Proportion of Variance",
                     "Cumulative Proportion")
  colnames(imp) <- paste0("PC", 1:attr(object, "ncomp"))
  print(imp, digits = 4)
  invisible(object)
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
summary.poplin.tsne <- function(object, ...) {
  cat("Call:", deparse(attr(object, "call")), "\n")
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("Input dimension: [",
      attr(object, "origD")[1], ", ",
      attr(object, "origD")[2], "]\n", sep = "")
  cat("Input normalized before t-SNE:", attr(object, "normalized"), "\n")
  cat("Dimension of the embedded spcae:", attr(object, "ncomp"), "\n")
  cat("Perplexity:", attr(object, "perplexity"), "\n")
  cat("Theta:", attr(object, "theta"), "\n")
  cat("Eta:", attr(object, "eta"), "\n")
  invisible(object)
}

##' @export
summary.poplin.plsda <- function(object, ...) {
  cat("Call:", deparse(attr(object, "call")), "\n")
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("X dimension: [",
    attr(object, "origD")[1], ", ",
    attr(object, "origD")[2], "]\n",
    sep = ""
  )
  cat("Y responses:", attr(object, "responses"), "\n")
  cat("X, Y matrices centered before PLS-DA:", attr(object, "centered"), "\n")
  cat("X scaled before PLS-DA:", attr(object, "scaled"), "\n")
  cat("Number of components considered:", attr(object, "ncomp"), "\n")
  cat("Amount of X variance explained by each component:", "\n")
  imp <- rbind(attr(object, "explvar"), cumsum(attr(object, "explvar")))
  rownames(imp) <- c("Explained %", "Cumulative %")
  print(imp, digits = 4)
  invisible(imp)
}
