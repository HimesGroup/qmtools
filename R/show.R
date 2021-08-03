##' @importFrom S4Vectors coolcat
.poplin_show <- function(object) {
  callNextMethod()
  coolcat("imputedDataNames(%d): %s\n", imputedDataNames(object))
  coolcat("normalizedDataNames(%d): %s\n", normalizedDataNames(object))
  coolcat("reducedDataNames(%d): %s\n", reducedDataNames(object))
}

##' @export
setMethod("show", "poplin", .poplin_show)
