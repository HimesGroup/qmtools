##' Normalization methods
##'
##' Performs a few data-driven normalization methods on a matrix-like object or
##' \linkS4class{SummarizedExperiment} object. The methods include probabilistic
##' quotient normalization (PQN), cyclic loess normalization, feature-based
##' scaling, and many others from the [MsCoreUtils::normalize_matrix]. See the
##' details below.
##'
##' The method argument can be one of "pqn", "cyclicloess", "vsn",
##' "feature.scale", "div.sum", "div.mean", "div.median", "div.mad",
##' "center.mean", "center.median", "diff.median", "quantiles", and
##' "quantiles.robust".
##'
##' * "pqn" performs probabilistic quotient normalization, as described in
##' Dieterle et al. (2006). See [normalizePQN] for details.
##'
##' * "cyclicloess" performs cyclic LOESS normalization using the
##' [limma::normalizeCyclicLoess]. The input \code{x} is expected to contain
##' log-transformed intensities. See Bolstad et al. (2003) and Ballman et al.
##' (2004) for details. Please use `type` if you want to specify a cyclic loess
##' method due to a name conflict with the existing argument in this function.
##'
##' * "vsn" performs variance stabilizing normalization (VSN), as described in
##' Huber et al. (2002). It produces normalized intensities based on a glog
##' (generalized logarithm) scale. See the [vsn::vsn2] for details.
##'
##' * "feature.scale" performs feature-based scaling (applied along the rows) as
##' described in van den Berg et al. (2006). See [scaleRows] for details.
##' 
##' * For "div.sum", "div.mean", "div.median", and "div.mad", the respective
##' sample intensities are divided by the column sums, means, medians, or median
##' absolute deviations. See [scaleCols] for details.
##'
##' * "center.mean" and "center.median" center the intensities by subtracting
##' the column means or medians, respectively.
##' 
##' * "diff.median" centers all samples so that they all match the grand median
##' by subtracting the respective columns medians differences to the grand
##' median.
##'
##' * "quantiles" and "quantiles.robust" perform quantiles normalization, as
##' described in Bolstad et al. (2003). See the
##' [preprocessCore::normalize.quantiles] and
##' [preprocessCore::normalize.quantiles.robust] for details.
##'
##' @param x A matrix-like object or \linkS4class{SummarizedExperiment} object.
##' @param method A string specifying which normalization method to use.
##' @param i A string or integer value specifying which assay values to use
##'   when \code{x} is a SummarizedExperiment object.
##' @param name A string specifying the name to be used to store the normalized
##'   intensities in \code{x} when \code{x} is a SummarizedExperiment object. If
##'   not specified, a matrix containing the normalized intensities is returned.
##' @param ... Arguments passed to a specific normalization method.
##' @return A matrix or \linkS4class{SummarizedExperiment} object of the same
##'   dimension as \code{x} containing the normalized intensities.
##'
##' @references
##'
##' Laurent Gatto, Johannes Rainer and Sebastian Gibb (2021).
##' MsCoreUtils: Core Utils for Mass Spectrometry Data. R package version
##' 1.4.0. https://github.com/RforMassSpectrometry/MsCoreUtils
##'
##' Dieterle F, Ross A, Schlotterbeck G, Senn H. Probabilistic quotient
##' normalization as robust method to account for dilution of complex biological
##' mixtures. Application in 1H NMR metabonomics. Anal Chem. 2006 Jul
##' 1;78(13):4281-90. doi: 10.1021/ac051632c. PMID: 16808434.
##'
##' Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, Smyth GK. limma powers
##' differential expression analyses for RNA-sequencing and microarray studies.
##' Nucleic Acids Res. 2015 Apr 20;43(7):e47. doi: 10.1093/nar/gkv007. Epub 2015
##' Jan 20. PMID: 25605792; PMCID: PMC4402510.
##'
##' Bolstad BM, Irizarry RA, Astrand M, Speed TP. A comparison of normalization
##' methods for high density oligonucleotide array data based on variance and
##' bias. Bioinformatics. 2003 Jan 22;19(2):185-93. doi:
##' 10.1093/bioinformatics/19.2.185. PMID: 12538238.
##'
##' Ballman KV, Grill DE, Oberg AL, Therneau TM. Faster cyclic loess:
##' normalizing RNA arrays via linear models. Bioinformatics. 2004 Nov
##' 1;20(16):2778-86. doi: 10.1093/bioinformatics/bth327. Epub 2004 May 27.
##'
##' Huber W, von Heydebreck A, Sültmann H, Poustka A, Vingron M. Variance
##' stabilization applied to microarray data calibration and to the
##' quantification of differential expression. Bioinformatics. 2002;18 Suppl
##' 1:S96-104. doi: 10.1093/bioinformatics/18.suppl_1.s96. PMID: 12169536.
##'
##' van den Berg RA, Hoefsloot HC, Westerhuis JA, Smilde AK, van der Werf MJ.
##' Centering, scaling, and transformations: improving the biological
##' information content of metabolomics data. BMC Genomics. 2006 Jun 8;7:142.
##' doi: 10.1186/1471-2164-7-142. PMID: 16762068; PMCID: PMC1534033.
##'
##' @name normalizeIntensity
##'
##' @seealso See [normalizePQN], [scaleRows], [scaleCols],
##'   [limma::normalizeCyclicLoess], and [MsCoreUtils::normalize_matrix] for the
##'   underlying functions that do the work.
##'
##' @examples
##'
##' ## SummarizedExperiment object
##' se <- normalizeIntensity(faahko_se, i = "knn", name = "knn_pqn",
##'                          method = "pqn")
##' assayNames(se)
##'                    
##' ## Matrix
##' m <- assay(faahko_se, "knn")
##' normalizeIntensity(m, method = "feature.scale", type = "pareto")
##' 
NULL

##' @rdname normalizeIntensity
setMethod(
  "normalizeIntensity", "ANY",
  function(x, method, ...) {
    .normalizeIntensity(x, method = method, ...)
  }
)

##' @rdname normalizeIntensity
setMethod(
  "normalizeIntensity", "SummarizedExperiment",
  function(x, method, i, name, ...) {
    if (missing(name)) {
      .normalizeIntensity(assay(x, i), method = method, ...)
    } else {
      assay(x, name) <- .normalizeIntensity(assay(x, i), method = method, ...)
      x
    }
  }
)

.normalizeIntensity <- function(x,
                                method = c("pqn", "div.sum", "div.mean",
                                           "div.median", "div.mad",
                                           "center.mean", "center.median",
                                           "diff.median",
                                           "cyclicloess", "vsn",
                                           "quantiles", "quantiles.robust",
                                           "feature.scale"),
                              ...) {
  method <- match.arg(method)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  switch(
    method,
    pqn = normalizePQN(x = x, ...),
    div.sum = scaleCols(x = x, type = "div.sum", ...),
    div.mean = scaleCols(x = x, type = "div.mean", ...),
    div.median = scaleCols(x = x, type = "div.median", ...),
    div.mad = scaleCols(x = x, type = "div.mad", ...),
    center.mean = MsCoreUtils::normalize_matrix(x = x, method = "center.mean"),
    center.median = MsCoreUtils::normalize_matrix(x = x,
                                                  method = "center.median"),
    diff.median = MsCoreUtils::normalize_matrix(x = x, method = "diff.median"),
    feature.scale = scaleRows(x = x, ...),
    cyclicloess = .normalize_cyclicloess(x = x, ...),
    vsn = {
      .verify_package("vsn")
      MsCoreUtils::normalize_matrix(x, method = "vsn", ...)
    },
    quantiles = {
      .verify_package("preprocessCore")
      MsCoreUtils::normalize_matrix(x, method = "quantiles", ...)
    },
    quantiles.robust = {
      .verify_package("preprocessCore")
      MsCoreUtils::normalize_matrix(x, method = "quantiles.robust", ...)
    }

  )
}

