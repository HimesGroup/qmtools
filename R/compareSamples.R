##' Sample comparison
##'
##' Function to make a comparisons between two groups in study samples with a
##' \linkS4class{SummarizedExperiment}.
##'
##' This function provides a simplified interface of fitting a linear model to
##' make a comparison of interest using the [limma::lmFit], [limma::eBayes], and
##' [limma::topTable] functions. For more flexible model specification (e.g.,
##' interaction model, multi-level model), please use a standard workflow
##' outlined in the `limma` package manual.
##'
##' @param x A \linkS4class{SummarizedExperiment} object.
##' @param i A string or integer value specifying which assay values to use. The
##'   assay is expected to contain log-transformed intensities.
##' @param group A string specifying the name of variable containing a class
##'   label of each sample in `colData(x)`.
##' @param class1,class2 A string specifying the class label of samples to be
##'   compared. Must be one of `group` levels. No need to be specified if
##'   `group` has only two levels. This function evaluates the contrast: class2
##'   - class1.
##' @param covariates A vector indicating the names of variables to be included
##'   in the model as covariates. The covariates must be found in `colData(x)`.
##' @param confint A logical specifying whether 95% confidence intervals of
##'   log-fold-change need to be reported. Alternatively, a numeric value
##'   between zero and one specifying the confidence level required.
##' @param number The maximum number of metabolic features to list.
##' @param adjust.method A string specifying which p-value adjustment method to
##'   use. Options, in increasing conservatism, include "none", "BH", "BY" and
##'   "holm". See [p.adjust] for the complete list of options. A NULL value will
##'   result in the default adjustment method, which is "BH".
##' @param sort.by A string specifying which statistic to rank the metabolic
##'   features by. Possible values for topTable are "logFC", "AveExpr", "t",
##'   "P", "p", "B" or "none" (Permitted synonyms are "M" for "logFC", "A" or
##'   "Amean" for "AveExpr", "T" for "t" and "p" for "P").
##' @param resort.by A string specifying statistic to sort the selected
##'   metabolic features by in the output. Possibilities are the same as for
##'   sort.by.
##' @param p.value A numeric value specifying a cut-off for adjusted p-values.
##'   Only metabolic features with lower p-values are listed.
##' @param fc A numeric value specifying a minimum fold-change to be required.
##'   If specified, the function output only includes metabolic features with
##'   absolute fold-change greater than `fc`.
##' @param lfc A numeric value specifying a minimum log2-fold-change required,
##'   equal to log2(fc). `fc` and `lfc` are alternative ways to specify a
##'   fold-change cut-off and, if both are specified, then `fc` take precedence.
##' @param ... Additional arguments passed to [limma::eBayes].
##' @return A data.frame with a row for the metabolic features and the following
##'   columns:
##' * logFC: an estimate of log-fold-change corresponding to the contrast tested
##' * CI.L: a left limit of confidence interval for `logFC` (if `confint` is
##' enabled)
##' * CI.R: a right limit of confidence interval for `logFC` (if `confint` is
##' enabled)
##' * AveExpr: an average log-expression/abundance of metabolic features
##' * t: a moderated t-statistic
##' * P.Value: a raw p-value
##' * adj.P.Value: an adjusted p-value
##' * B: a log-odds that the metabolic feature is differentially expressed
##'
##' @references
##'
##' Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, Smyth GK. limma powers
##' differential expression analyses for RNA-sequencing and microarray studies.
##' Nucleic Acids Res. 2015 Apr 20;43(7):e47. doi: 10.1093/nar/gkv007. Epub 2015
##' Jan 20. PMID: 25605792; PMCID: PMC4402510.
##'
##' @seealso
##'
##' See [limma::lmFit], [limma::eBayes], and [limma::topTable] for underlying
##' functions that do work.
##'
##' @examples
##'
##' data(faahko_se)
##'
##' compareSamples(faahko_se, i = "knn_vsn", group = "sample_group", number = 5)
##'
##' @export
compareSamples <- function(x, i, group,
                           class1, class2, covariates = NULL, confint = TRUE,
                           number = nrow(x), adjust.method = "BH",
                           sort.by = "B", resort.by = NULL,
                           p.value = 1, fc = NULL, lfc = NULL, ...) {
  if (!is(x, "SummarizedExperiment")) {
    stop("`x` must be a SummarizedExperiment object.")
  }
  ## Meta data describing the study samples
  cdat <- colData(x)
  ## Assert `group` is a column of colData(x)
  if (!any(colnames(colData(x)) == group)) {
    stop("Variable `", group, "` is not found in `colData(x)`." )
  }
  ## Convert `group` into a factor variable
  if (!is.factor(cdat[[group]])) {
    cdat[[group]] <- as.factor(cdat[[group]])
  }
  ## Two-level group does not have to specify class levels; instead, infer a
  ## contrast to test.
  if (any(missing(class1), missing(class2))) {
    if (length(levels(cdat[[group]])) > 2) {
      stop("`", group, "` has more than two levels.",
           " Please specify class1 and class2 arguments.")
    } else if (missing(class1) && !missing(class2)) {
      class1 <- setdiff(levels(cdat[[group]]), class2)
    } else if (!missing(class1) && missing(class2)) {
      class2 <- setdiff(levels(cdat[[group]]), class1)
    } else {
      class1 <- levels(cdat[[group]])[1]
      class2 <- levels(cdat[[group]])[2]
    }
  }
  if (!all(c(class1, class2) %in% levels(cdat[[group]]))) {
    stop(setdiff(c(class1, class2), levels(cdat[[group]])),
         " is not the levels of '", group, "'")
  }
  message(paste0("Contrast: ", class2, " - ", class1))
  ## Check covariates are found in cdat
  if (!all(covariates %in% colnames(cdat))) {
    stop("`colData(x)` does not have a column(s): ",
         paste(setdiff(covariates, colnames(cdat)), collapse = ", "))
  }
  ## Model formula
  fm <- as.formula(paste(c("~ 0", group, covariates), collapse = " + "))
  ## Design matrix (group mean specification)
  dm <- model.matrix(fm, data = cdat)
  colnames(dm) <- sub(group, "", colnames(dm))
  contrast <- limma::makeContrasts(contrasts = paste(class2, class1, sep = " - "), levels = dm)
  ## Fit model
  fit <- limma::lmFit(assay(x, i), design = dm)
  fit <- limma::contrasts.fit(fit, contrasts = contrast)
  fit <- limma::eBayes(fit, ...)
  ## Output
  limma::topTable(
           fit, number = number, adjust.method = adjust.method,
           sort.by = sort.by, resort.by = resort.by, p.value = p.value,
           fc = fc, lfc = lfc, confint = confint
         )
}
