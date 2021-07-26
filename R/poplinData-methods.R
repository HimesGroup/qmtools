##' @export
setMethod("poplinData", "poplin", function(x) x@poplinData)

##' @export
setReplaceMethod("poplinData", "poplin", function(x, value) {
  x@poplinData <- value
  x
})

