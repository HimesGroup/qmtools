##' @export
setMethod("missingCount", "poplin", function(x) x@missingCount)

##' @export
setReplaceMethod("missingCount", "poplin", function(x, value) {
  x@missingCount <- value
  x
})

