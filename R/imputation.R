##' @export
setMethod("imputation", "poplin", function(x) x@imputation)

##' @export
setReplaceMethod("imputation", "poplin", function(x, value) {
  x@imputation <- value
  x
})

##' @export
setMethod("poplin_impute", "poplin", function(x) {
  value <- .get_poplin_element(x, get_slot=imputation, element = "poplin_impute")
  for (i in seq_along(value)) {
    rownames(value[[i]]) <- rownames(x)
    colnames(value[[i]]) <- colnames(x)
  }
  value
})

##' @export
setReplaceMethod("poplin_impute", "poplin",
                 function(x, check_dimnames = TRUE, ..., value) {
  if (check_dimnames) {
    for (v in seq_along(value)) {
      .check_dimnames(x, value[[v]], fun='poplin_impute')
    }
  }

  .set_poplin_element(
    x, value,
    get_slot = imputation,
    set_element_fun = `imputation<-`,
    element = "poplin_impute",
    funstr = "poplin_impute",
    name_pattern = "imputed"
  )

})
