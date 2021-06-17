##' @export
setMethod("imputedData", "poplin", function(x) {
  value <- .get_poplin_element(x, get_slot=poplinData, element = "imputedData")
  for (i in seq_along(value)) {
    rownames(value[[i]]) <- rownames(x)
    colnames(value[[i]]) <- colnames(x)
  }
  value
})

##' @export
setReplaceMethod("imputedData", "poplin",
                 function(x, check_dimnames = TRUE, ..., value) {
  if (check_dimnames) {
    for (v in seq_along(value)) {
      .check_dimnames(x, value[[v]], fun='imputedData')
    }
  }

  .set_poplin_element(
    x, value,
    get_slot = poplinData,
    set_element_fun = `poplinData<-`,
    element = "imputedData",
    funstr = "imputedData",
    name_pattern = "imputed"
  )

})
