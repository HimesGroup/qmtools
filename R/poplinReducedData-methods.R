##' @export
setMethod("poplinReducedData", "poplin", function(x) x@poplinReducedData)

##' @export
setReplaceMethod("poplinReducedData", "poplin", function(x, value) {
  x@poplinReducedData <- value
  x
})


