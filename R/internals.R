##' @export
setMethod("missingCount", "poplin", function(x) x@missingCount)

##' @export
setReplaceMethod("missingCount", "poplin", function(x, value) {
  x@missing <- value
  x
})

##' @export
setMethod("poplinData", "poplin", function(x) x@poplinData)

##' @export
setReplaceMethod("poplinData", "poplin", function(x, value) {
  x@poplinData <- value
  x
})

