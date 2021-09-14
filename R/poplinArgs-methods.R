## List coercion method
##' @export
as.list.poplinArgs <- function(x) {
  snames <- slotNames(x)
  internals <- grep(snames, pattern = "^\\.")
  if (length(internals)) {
    ## Exclude hidden slots
    snames <- snames[-internals]
  }
  ## args_list <- vector("list", length(snames)) # empty list
  ## names(args_list) <- snames
  ## for (i in names(args_list)) {
  ##   args_list[[i]] <- slot(x, name = i)
  ## }
  args_list <- lapply(snames, function(name) slot(x, name))
  names(args_list) <- snames
  args_list
}

setMethod("as.list", "poplinArgs", as.list.poplinArgs)

##' @exportMethod coerce
setAs("poplinArgs", "list", function(from) {
  as.list(from)
})

## Show method
.poplinArgs_show <- function(object) {
  cat("Class: ", class(object), "\n", sep = "")
  cat(" Arguments:\n")
  poplin_args <- as.list(object)
  for (i in seq_along(poplin_args)) {
    cat(" - ", names(poplin_args)[i], ": ", deparse(poplin_args[[i]]), "\n",
        sep = "")
  }
}

setMethod("show", "poplinArgs", .poplinArgs_show)

.slot_check <- function(object) {
  snames <- slotNames(object)
  slot_check <- sapply(slotNames(object), function(x) !.hasSlot(object, x))
  if (sum(slot_check) == 0) {
    character(0)
  } else {
    paste0("Slots not found: ", snames[slot_check])
  }
}

.pqn_args_validity <- function(object) {
  msg <- .slot_check(object)
  if (length(object@dat_in) != 1) {
    msg <- c(msg, "'dat_in' must be a character of length 1.")
  }
  if (length(object@dat_out) != 1) {
    msg <- c(msg, "'dat_out' must be a character of length 1.")
  }
  if (length(object@min_frac) != 1 || object@min_frac < 0 ||
      object@min_frac > 1) {
    msg <- c(msg, "'min_frac' must be a numeric value between 0 and 1.")
  }
  if (length(object@type) != 1 || !(object@type %in% c("mean", "median"))) {
    msg <- c(msg, "'type' must be either \"mean\" or \"median\".")
  }
  if (length(msg)) {
    msg
  } else TRUE
}

setValidity("pqn_args", .pqn_args_validity)

.sample_normalizer_args_validity <- function(object) {
  msg <- .slot_check(object)
  normalizer_allowed <- c("sum", "mean", "median", "mad", "euclidean")
  if (length(object@normalizer) != 1 ||
      !(object@normalizer %in% normalizer_allowed)) {
    msg <- c(
      msg,
      paste0("'normalizer' must be one of ",
             paste0("'", normalizer_allowed, "'", collapse = ", "), ".")
    )
  }
  if (length(object@dat_in) != 1) {
    msg <- c(msg, "'dat_in' must be a character of length 1.")
  }
  if (length(object@dat_out) != 1) {
    msg <- c(msg, "'dat_out' must be a character of length 1.")
  }
  if (length(object@restrict) != 1) {
    msg <- c(msg, "'restrict' must be a logical of length 1.")
  }
  if (length(object@rescale) != 1) {
    msg <- c(msg, "'rescale' must be a logical of length 1.")
  }
  if (length(msg)) {
    msg
  } else TRUE
}

setValidity("sample_normalizer_args", .sample_normalizer_args_validity)

.cyclicloess_args_validity <- function(object) {
  msg <- .slot_check(object)
  ## if (length(object@normalizer) != 1 || object@normalizer != "cyclicloess") {
  ##   msg <- c(msg, "'normalizer' must be cyclicloess.")
  ## }
  if (length(object@dat_in) != 1) {
    msg <- c(msg, "'dat_in' must be a character of length 1.")
  }
  if (length(object@dat_out) != 1) {
    msg <- c(msg, "'dat_out' must be a character of length 1.")
  }
  if (length(object@span) != 1 || object@span < 0 || object@span > 1) {
    msg <- c(msg, "'span' must be a numeric value between 0 and 1.")
  }
  if (length(object@iterations) != 1 || object@iterations <= 0) {
    msg <- c(msg, "'iterations' must be a positive integer.")
  }
  method_allowed <- c("pairs", "fast", "affy")
  if (length(object@method) != 1 || !(object@method %in% method_allowed)) {
    msg <- c(
      msg,
      paste0("'method' must be one of ",
             paste0("'", method_allowed, "'", collapse = ", "), ".")
    )
  }
}

setValidity("cyclicloess_args", .cyclicloess_args_validity)
