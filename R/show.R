##' @importFrom S4Vectors coolcat
.poplin_show <- function(object) {
  callNextMethod()
  coolcat("poplinData names(%d): %s\n", poplin_data_names(object))
  coolcat("poplinReducedData names(%d): %s\n", poplin_reduced_names(object))
}

##' @export
setMethod("show", "poplin", .poplin_show)
