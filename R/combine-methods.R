## Need to think about how to handle metadata of poplinData(x) and
## poplinReducedData(x) when data are combined

## poplin class cbind and rbind
##' @export
##' @importFrom BiocGenerics rbind cbind
##' @importFrom SummarizedExperiment Assays
setMethod("rbind", "poplin", function(..., deparse.level = 1) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  out <- callNextMethod()
  args <- list(...)
  ## Utilize Assay class for combining objects
  ## poplinData_assays <- lapply(args, function(x) .poplin_to_assays(x))
  ## tryCatch({
  ##   combined <- do.call(rbind, poplinData_assays)
  ##   poplinData_all <- do.call(
  ##     DataFrame, c(lapply(combined@data, I),
  ##                  list(row.names = NULL, check.names = FALSE))
  ##   )
  ## },
  ## error = function(err) {
  ##   stop(
  ##     "failed to combine 'poplinData' in 'rbind(<",
  ##     class(args[[1]]), ">)':\n  ", conditionMessage(err)
  ##   )
  ## })
  tryCatch({
    poplinData_all <- do.call(rbind, lapply(args, poplinData))
    ## how to handle metadata?
    metadata(poplinData_all) <- list() # clean metadata; consistency with assay-via method
  })
  ## Utilize SE class for combining objects: see the SingleCellExperiment source
  poplinReducedData_se <- lapply(args, function(x) .poplin_to_se_coldata(x))
  tryCatch({
    poplinReducedData_all <- colData(do.call(rbind, poplinReducedData_se))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinReducedData' in 'rbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  ## Skip validity checks with check = FALSE for efficiency as modification
  ## cannot alter the validity of object
  BiocGenerics:::replaceSlots(out, poplinData = poplinData_all,
                              poplinReducedData = poplinReducedData_all,
                              check = FALSE)
})

setMethod("cbind", "poplin", function(..., deparse.level = 1) {
  old_validity <- S4Vectors:::disableValidity()
  if (!isTRUE(old_validity)) {
    S4Vectors:::disableValidity(TRUE)
    on.exit(S4Vectors:::disableValidity(old_validity))
  }
  out <- callNextMethod()
  args <- list(...)
  poplinData_assays <- lapply(args, function(x) .poplin_to_assays(x))
  tryCatch({
    combined <- do.call(cbind, poplinData_assays)
    poplinData_all <- do.call(
      DataFrame, c(lapply(combined@data, I),
                   list(row.names = NULL, check.names = FALSE))
    )
    ## how to hand metadata?
    ## metadata(poplinData_all) <- metadata(poplinData(args[[1]]))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinData' in 'cbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  tryCatch({
    poplinReducedData_all <- do.call(rbind, lapply(args, poplinReducedData))
  },
  error = function(err) {
    stop(
      "failed to combine 'poplinReducedData' in 'cbind(<",
      class(args[[1]]), ">)':\n  ", conditionMessage(err)
    )
  })
  ## Skip validity checks with check = FALSE for efficiency as modification
  ## cannot alter the validity of object
  BiocGenerics:::replaceSlots(out, poplinData = poplinData_all,
                              poplinReducedData = poplinReducedData_all,
                              check = FALSE)
})


