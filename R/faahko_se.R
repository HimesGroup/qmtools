##' FAAH knockout LC/MS data SummarizedExperiment
##'
##' A \linkS4class{SummarizedExperiment} object containing FAAH knockout LC/MS
##' feature data from the \pkg{faahKO} created with the following code:
##' \code{xcms::quantify(faahko3, filled = FALSE)}.
##'
##' @references
##'
##' Colin A. Smith (2021). faahKO: Saghatelian et al. (2004) FAAH knockout LC/MS
##' data. R package version 1.32.0. http://dx.doi.org/10.1021/bi0480335
##'
##' @examples
##' faahko_se
"faahko_se"

##' FAAH knockout LC/MS data poplin
##'
##' A \linkS4class{poplin} object containing FAAH knockout LC/MS feature data
##' from the \pkg{faahKO} (\code{faahko3} data) created by coercing the
##' [faahko_se] object.
##'
##' @examples
##' faahko_poplin
"faahko_poplin"
