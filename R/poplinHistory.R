setClassUnion("Args_or_NULL", c("poplinArgs", "NULL"))

setClass(
  "poplinHistory",
  slots = c(
    process = "character",
    date = "character",
    ## args = "Args_or_NULL"
    args = "list"
    )
)

##' @export
.poplin_history <- function(process, date, args) {
  new("poplinHistory", process = process, date = date, args = args)
}
