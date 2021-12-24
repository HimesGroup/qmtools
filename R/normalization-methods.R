##' Normalization methods
##'
##' Data normalization aims to reduce unwanted variations. The \pkg{poplin}
##' package provides a few data-driven normalization methods.
##' [poplin_normalize] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{normalize_sum}}:}{
##' sum normalization (TIC normalization)
##' }
##' \item{\code{\link{normalize_mean}}:}{
##' mean normalization
##' }
##' \item{\code{\link{normalize_median}}:}{
##' median normalization
##' }
##' \item{\code{\link{normalize_mad}}:}{
##' MAD (median absolute deviation) normalization
##' }
##' \item{\code{\link{normalize_cyclicloess}}:}{
##' cyclic LOESS normalization
##' }
##' \item{\code{\link{normalize_pqn}}:}{
##' PQN (probabilistic quotient normalization)
##' }
##' \item{\code{\link{normalize_vsn}}:}{
##' VSN (variance stabilizing normalization)
##' }
##' \item{\code{\link{normalize_scale}}:}{
##' feature-based scaling (e.g., auto, range, pareto, vast, level)
##' }
##' }
##' @param x A matrix or \linkS4class{poplin} object.
##' @param method A normalization method. Default is 'pqn'.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ... Argument passed to a specific normalization method.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name poplin_normalize
##' @aliases
##' poplin_normalize
##' poplin_normalize,matrix-method
##' poplin_normalize,poplin-method
##' @family normalization methods
setMethod(
  "poplin_normalize",
  "matrix",
  function(x, method = c("pqn",  "sum", "mean", "median",
                         "mad", "euclidean", "cyclicloess",
                         "vsn", "scale"), ...) {
    .poplin_normalize(x, method = method, ...)
  }
)

##' @rdname poplin_normalize
setMethod(
  "poplin_normalize",
  "poplin",
  function(x, method = c("pqn",  "sum", "mean", "median",
                         "mad", "euclidean", "cyclicloess",
                         "vsn", "scale"), poplin_in, poplin_out, ...) {
    m <- .verify_and_extract_input(x, poplin_in)
    poplin_data(x, poplin_out) <- .poplin_normalize(m, method = method, ...)
    x
  }
)

##' Probabilistic quotient normalization (PQN)
##'
##' Apply probabilistic quotient normalization (PQN) to a matrix or
##' \linkS4class{poplin} object. For the calculation of quotients, a reference
##' spectrum needs to be obtained from a mean or median spectrum based on all
##' spectra of the study or a subset of the study. Feature intensities are
##' normalized by the median of quotients. See Dieterle et al. (2006) for
##' details.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param ref_samples A vector of sample names or indices to identify reference
##'   samples for the calculation of quotients. Must be a subset of
##'   \code{colnames(x)} if it is a character vector. If \code{NULL}, all
##'   samples are used.
##' @param min_frac A minimum proportion of reference samples for features to
##'   be considered in the calculation of a reference spectrum.
##' @param type A method to compute a reference spectrum
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @references
##' Dieterle F, Ross A, Schlotterbeck G, Senn H. Probabilistic quotient
##' normalization as robust method to account for dilution of complex biological
##' mixtures. Application in 1H NMR metabonomics. Anal Chem. 2006 Jul
##' 1;78(13):4281-90. doi: 10.1021/ac051632c. PMID: 16808434.
##' @name normalize_pqn
##' @aliases
##' normalize_pqn
##' normalize_pqn,matrix-method
##' normalize_pqn,poplin-method
##' @family normalization methods
setMethod(
  "normalize_pqn",
  "matrix",
  function(x, ref_samples = colnames(x), min_frac = 0.5,
           type = c("mean", "median")) {
    .normalize_pqn(x, ref_samples = ref_samples, min_frac = min_frac,
                          type = type)
  }
)

##' @rdname normalize_pqn
setMethod(
  "normalize_pqn",
  "poplin",
  function(x, poplin_in, poplin_out, ref_samples = colnames(x), min_frac = 0.5,
           type = c("mean", "median")) {
    .poplin_extract_and_assign(x, .normalize_pqn,
                               poplin_in, poplin_out, ref_samples = ref_samples,
                               min_frac = min_frac, type = type)
  }
)

##' Sum normalization (TIC normalization)
##'
##' Apply sum normalization to a matrix or \linkS4class{poplin} object. For each
##' sample, feature intensities are divided by its Total Ion Current (TIC),
##' i.e., every feature is divided by the sum of all intensity values.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param restrict If \code{TRUE}, any feature with missing values is excluded
##'   from the calculation of normalization factors.
##' @param rescale If \code{TRUE}, normalized intensities are multiplied by the
##'   median of normalization factors to make look similar to their original
##'   scales.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_sum
##' @aliases
##' normalize_sum
##' normalize_sum,matrix-method
##' normalize_sum,poplin-method
##' @family normalization methods
setMethod(
  "normalize_sum",
  "matrix",
  function(x, restrict = FALSE, rescale = FALSE) {
    .normalize_sum(x, restrict = restrict, rescale = rescale)
  }
)

##' @rdname normalize_sum
setMethod(
  "normalize_sum",
  "poplin",
  function(x, poplin_in, poplin_out, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_sum,
                               poplin_in, poplin_out,
                               restrict = restrict, rescale = rescale)
  }
)

##' Mean normalization
##'
##' Apply mean normalization to a matrix or \linkS4class{poplin} object. For
##' each sample, feature intensities are divided by its mean. The mean of
##' intensity values for individual samples will be one as a result.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param restrict If \code{TRUE}, any feature with missing values is excluded
##'   from the calculation of normalization factors.
##' @param rescale If \code{TRUE}, normalized intensities are multiplied by the
##'   median of normalization factors to make look similar to their original
##'   scales.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_mean
##' @aliases
##' normalize_mean
##' normalize_mean,matrix-method
##' normalize_mean,poplin-method
##' @family normalization methods
setMethod(
  "normalize_mean",
  "matrix",
  function(x, restrict = FALSE, rescale = FALSE) {
    .normalize_mean(x, restrict = restrict, rescale = rescale)
  }
)

##' @rdname normalize_mean
setMethod(
  "normalize_mean",
  "poplin",
  function(x, poplin_in, poplin_out, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_mean,
                               poplin_in, poplin_out,
                               restrict = restrict, rescale = rescale)
  }
)

##' Median normalization
##'
##' Apply median normalization to a matrix or \linkS4class{poplin} object. For
##' each sample, feature intensities are divided by its median. The median of
##' intensity values for individual samples will be one as a result.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param restrict If \code{TRUE}, any feature with missing values is excluded
##'   from the calculation of normalization factors.
##' @param rescale If \code{TRUE}, normalized intensities are multiplied by the
##'   median of normalization factors to make look similar to their original
##'   scales.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_median
##' @aliases
##' normalize_median
##' normalize_median,matrix-method
##' normalize_median,poplin-method
##' @family normalization methods
setMethod(
  "normalize_median",
  "matrix",
  function(x, restrict = FALSE, rescale = FALSE) {
    .normalize_median(x, restrict = restrict, rescale = rescale)
  }
)

##' @rdname normalize_median
setMethod(
  "normalize_median",
  "poplin",
  function(x, poplin_in, poplin_out, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_median,
                               poplin_in, poplin_out,
                               restrict = restrict, rescale = rescale)
  }
)

##' Median absolute deviation (MAD) normalization
##'
##' Apply median absolute deviation (MAD) normalization to a matrix or
##' \linkS4class{poplin} object. For each sample, feature intensities are scaled
##' by its MAD. The MAD of intensity values for individual samples will be one
##' as a result.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param restrict If \code{TRUE}, any feature with missing values is excluded
##'   from the calculation of normalization factors.
##' @param rescale If \code{TRUE}, normalized intensities are multiplied by the
##'   median of normalization factors to make look similar to their original
##'   scales.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_mad
##' @aliases
##' normalize_mad
##' normalize_mad,matrix-method
##' normalize_mad,poplin-method
##' @family normalization methods
setMethod(
  "normalize_mad",
  "matrix",
  function(x, restrict = FALSE, rescale = FALSE) {
    .normalize_mad(x, restrict = restrict, rescale = rescale)
  }
)

##' @rdname normalize_mad
setMethod(
  "normalize_mad",
  "poplin",
  function(x, poplin_in, poplin_out, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_mad,
                               poplin_in, poplin_out,
                               restrict = restrict, rescale = rescale)
  }
)

## setMethod(
##   "normalize_euclidean",
##   "matrix",
##   function(x, restrict = FALSE, rescale = FALSE) {
##     .normalize_euclidean(x, restrict = restrict, rescale = rescale)
##   }
## )

## setMethod(
##   "normalize_euclidean",
##   "poplin",
##   function(x, poplin_in, poplin_out, restrict = FALSE, rescale = FALSE) {
##     .poplin_extract_and_assign(x, .normalize_euclidean,
##                                poplin_in, poplin_out,
##                                restrict = restrict, rescale = rescale)
##   }
## )


##' Cyclic LOESS normalization
##'
##' Apply Cyclic LOESS normalization to a matrix or \linkS4class{poplin} object.
##' This is an interface to the [limma::normalizeCyclicLoess] from the
##' \pkg{limma} package. The input \code{x} is expected to contain
##' log-transformed raw intensities. See Bolstad et al. (2003) and Ballman et
##' al. (2004) for details.
##'
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param pre_log2 If \code{TRUE}, feature intensities are log2-transformed
##'   before normalization.
##' @param type Character string specifying which variant of the cyclic LOESS
##'   method to use.
##' @param span Span of LOESS smoothing window, between 0 and 1.
##' @param iterations Number of times to cycle through all pairs of columns.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @references
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
##' PMID: 15166021.
##' @name normalize_cyclicloess
##' @aliases
##' normalize_cyclicloess
##' normalize_cyclicloess,matrix-method
##' normalize_cyclicloess,poplin-method
##' @family normalization methods
setMethod(
  "normalize_cyclicloess",
  "matrix",
  function(x, pre_log2 = TRUE, type = c("fast", "affy", "pairs"),
           span = 0.7, iterations = 3) {
    .normalize_cyclicloess(x, pre_log2 = pre_log2, type = type,
                                  span = span, iterations = iterations,
                                  weights = NULL)
  }
)

##' @rdname normalize_cyclicloess
setMethod(
  "normalize_cyclicloess",
  "poplin",
  function(x, poplin_in, poplin_out, pre_log2 = TRUE,
           type = c("fast", "affy", "pairs"), span = 0.7, iterations = 3) {
    .poplin_extract_and_assign(x, .normalize_cyclicloess,
                               poplin_in, poplin_out,
                               pre_log2 = pre_log2, type = type,
                               span = span, iterations = iterations,
                               weights = NULL)
  }
)

##' Variance stabilizing normalization (VSN)
##'
##' Apply variance stabilizing normalization (VSN) to a matrix or
##' \linkS4class{poplin} object. This is an interface to the [vsn::vsnMatrix]
##' from the \pkg{vsn} package (see [vsn::vsn2] for help). The vsn produces
##' normalized intensities based on a glog (generalized logarithm) scale to base
##' 2. See Huber et al. (2002) for details.
##'
##' @references
##' Huber W, von Heydebreck A, Sültmann H, Poustka A, Vingron M. Variance
##' stabilization applied to microarray data calibration and to the
##' quantification of differential expression. Bioinformatics. 2002;18 Suppl
##' 1:S96-104. doi: 10.1093/bioinformatics/18.suppl_1.s96. PMID: 12169536.
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param meanSdPlot If \code{TRUE}, the function shows [vsn::meanSdPlot] to
##'   visually verify whether there is a dependence of the standard deviation on
##'   the mean.
##' @param ... Additional arguments passed to [vsn::vsnMatrix].
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_vsn
##' @aliases
##' normalize_vsn
##' normalize_vsn,matrix-method
##' normalize_vsn,poplin-method
##' @family normalization methods
setMethod(
  "normalize_vsn",
  "matrix",
  function(x, meanSdPlot = FALSE, ...) {
    .normalize_vsn(x, meanSdPlot = meanSdPlot, ...)
  }
)

##' @rdname normalize_vsn
setMethod(
  "normalize_vsn",
  "poplin",
  function(x, poplin_in, poplin_out, meanSdPlot = FALSE, ...) {
    .poplin_extract_and_assign(x, .normalize_vsn,
                               poplin_in, poplin_out,
                               meanSdPlot = meanSdPlot, ...)
  }
)

##' Feature-based scaling
##'
##' Apply feature-based scaling to a matrix or \linkS4class{poplin} object. The
##' supported methods include
##' \itemize{
##' \item Auto scaling (unit variance scaling): each feature is scaled by its
##' standard deviation.
##' \item Range scaling: each feature is scaled by its range.
##' \item Pareto scaling: each feature is scaled by the square root of its standard
##' deviation.
##' \item Vast scaling (variance stability scaling): it is an extension of auto
##' scaling, using the product of standard deviation and coefficient of
##' variation as a scaling factor.
##' \item Level scaling: each feature is scaled by its mean.
##' }
##' Note that each feature is mean-centered prior to division. See van den Berg
##' et al. (2006) for details.
##' @references
##' van den Berg RA, Hoefsloot HC, Westerhuis JA, Smilde AK, van der Werf MJ.
##' Centering, scaling, and transformations: improving the biological
##' information content of metabolomics data. BMC Genomics. 2006 Jun 8;7:142.
##' doi: 10.1186/1471-2164-7-142. PMID: 16762068; PMCID: PMC1534033.
##' @param x A matrix or \linkS4class{poplin} object.
##' @param poplin_in Name of a data matrix to retrieve.
##' @param poplin_out Name of a data matrix to store.
##' @param type A scaling method to be applied.
##' @return A matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_scale
##' @aliases
##' normalize_scale
##' normalize_scale,matrix-method
##' normalize_scale,poplin-method
##' @family normalization methods
setMethod(
  "normalize_scale",
  "matrix",
  function(x, type = c("auto", "range", "pareto", "vast", "level")) {
    .normalize_scale(x, type = type)
  }
)

##' @rdname normalize_scale
setMethod(
  "normalize_scale",
  "poplin",
  function(x, poplin_in, poplin_out,
           type = c("auto", "range", "pareto", "vast", "level")) {
    .poplin_extract_and_assign(x, .normalize_scale,
                               poplin_in, poplin_out, type = type)
  }
)

## setMethod(
##   "normalize_auto",
##   "matrix",
##   function(x, ...) {
##     .normalize_auto(x, ...)
##   }
## )

## setMethod(
##   "normalize_auto",
##   "poplin",
##   function(x, poplin_in, poplin_out, ...) {
##     .poplin_extract_and_assign(x, .normalize_auto,
##                                poplin_in, poplin_out, ...)
##   }
## )

## setMethod(
##   "normalize_range",
##   "matrix",
##   function(x, ...) {
##     .normalize_range(x, ...)
##   }
## )

## setMethod(
##   "normalize_range",
##   "poplin",
##   function(x, poplin_in, poplin_out, ...) {
##     .poplin_extract_and_assign(x, .normalize_range,
##                                poplin_in, poplin_out, ...)
##   }
## )

## setMethod(
##   "normalize_pareto",
##   "matrix",
##   function(x, ...) {
##     .normalize_pareto(x, ...)
##   }
## )

## setMethod(
##   "normalize_pareto",
##   "poplin",
##   function(x, poplin_in, poplin_out, ...) {
##     .poplin_extract_and_assign(x, .normalize_pareto,
##                                poplin_in, poplin_out, ...)
##   }
## )

## setMethod(
##   "normalize_vast",
##   "matrix",
##   function(x, ...) {
##     .normalize_vast(x, ...)
##   }
## )

## setMethod(
##   "normalize_vast",
##   "poplin",
##   function(x, poplin_in, poplin_out, ...) {
##     .poplin_extract_and_assign(x, .normalize_vast,
##                                poplin_in, poplin_out, ...)
##   }
## )

## setMethod(
##   "normalize_level",
##   "matrix",
##   function(x, ...) {
##     .normalize_level(x, ...)
##   }
## )

## setMethod(
##   "normalize_level",
##   "poplin",
##   function(x, poplin_in, poplin_out, ...) {
##     .poplin_extract_and_assign(x, .normalize_level,
##                                poplin_in, poplin_out, ...)
##   }
## )

