##' @export
summary.reduced.pca <- function(object, ...) {
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("Input centered before PCA:", attr(object, "centered"), "\n")
  cat("Input scaled before PCA:", attr(object, "scaled"), "\n")
  cat("Number of PCs calculated:", attr(object, "ncomp"), "\n")
  cat("Importance of PC(s):\n")
  imp <- rbind(attr(object, "R2"), attr(object, "R2cum"))
  rownames(imp) <- c("Proportion of Variance",
                     "Cumulative Proportion")
  colnames(imp) <- paste0("PC", seq_len(attr(object, "ncomp")))
  print(imp, digits = 4)
  invisible(object)
}

##' @export
summary.reduced.tsne <- function(object, ...) {
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("Input normalized before t-SNE:", attr(object, "normalized"), "\n")
  cat("Dimension of the embedded spcae:", attr(object, "ncomp"), "\n")
  cat("Perplexity:", attr(object, "perplexity"), "\n")
  cat("Theta:", attr(object, "theta"), "\n")
  invisible(object)
}

##' @export
summary.reduced.plsda <- function(object, ...) {
  cat("Reduction method:", attr(object, "method"), "\n")
  cat("Y responses:", attr(object, "responses"), "\n")
  cat("Input centered before PLS-DA:",
      attr(object, "centered"), "\n")
  cat("Input scaled before PLS-DA:", attr(object, "scaled"), "\n")
  cat("Number of components considered:", attr(object, "ncomp"), "\n")
  cat("Amount of X variance explained by each component:", "\n")
  imp <- rbind(attr(object, "explvar"), cumsum(attr(object, "explvar")))
  rownames(imp) <- c("Explained %", "Cumulative %")
  print(imp, digits = 4)
  invisible(object)
}

##' @export
print.reduced.pca <- function(x, ...) {
  .hide_attributes(x)
}

##' @export
print.reduced.tsne <- function(x, ...) {
  .hide_attributes(x)
}

##' @export
print.reduced.plsda <- function(x, ...) {
  .hide_attributes(x)
}

.hide_attributes <- function(x, ...) {
  at <- attributes(x)
  custom_at <- setdiff(names(at), c("dim", "dimnames"))
  for (i in custom_at) {
    attr(x, i) <- NULL
  }
  print.default(x, ...)
}

