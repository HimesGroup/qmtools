setClass("poplinArgs", contains = "VIRTUAL")

setMethod("as.list", "poplinArgs", function(x, ...) {
  snames <- slotNames(x)
  internals <- grep(snames, pattern = "^\\.")
  if (length(internals)) {
    ## Remove hidden slots
    snames <- snames[-internals]
  }
  args_list <- vector("list", length(snames)) # empty list
  names(args_list) <- snames
  for (i in names(arg_list)) {
    args_list[[i]] <- slot(x, name = i)
  }
  args_list
})

#' @exportMethod coerce
setAs("poplinArgs", "list", function(from) {
  as.list(from)
})

.poplinArgs_show <- function(object) {
  cat("Class: '", class(x), "'\n")
  cat(" Arguments:\n")
  poplin_args <- as.list(x)
  for (i in seq_along(poplin_args)) {
    cat(" - ", names(poplin_args)[i], ": ", deparse(poplin_args[[i]]), "\n")
  }
}

setMethod("show", "poplinArgs", .poplinArgs_show)

