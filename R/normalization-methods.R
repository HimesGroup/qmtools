##' Normalization methods
##'
##' Data normalization aims to reduce unwanted variations. The \pkg{poplin}
##' package provides a few data-driven normalization methods.
##' [poplin_normalize] is a wrapper for the following set of functions:
##' \describe{
##' \item{\code{\link{normalize_pqn}}:}{
##' PQN (probabilistic quotient normalization)
##' }
##' \item{\code{\link{normalize_sum}}:}{
##' sum normalization
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
##' \item{\code{\link{normalize_vsn}}:}{
##' VSN (variance stabilizing normalization)
##' }
##' \item{\code{\link{normalize_scale}}:}{
##' feature-based scaling (e.g., auto, range, pareto, vast, level)
##' }
##' }
##' @param x a matrix or \linkS4class{poplin} object.
##' @param method the normalization method to be used, defaulting to "pqn".
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ... arguments passed to a specific normalization method.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name poplin_normalize
##' @aliases
##' poplin_normalize
##' poplin_normalize,matrix-method
##' poplin_normalize,poplin-method
##' @family normalization methods
##' @examples
##' 
##' ## poplin object
##' poplin_normalize(faahko_poplin, method = "pqn", xin = "knn", xout = "knn_pqn")
##' 
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' poplin_normalize(m)
setMethod(
  "poplin_normalize",
  "matrix",
  function(x, method = c("pqn",  "sum", "mean", "median",
                         "mad", "cyclicloess", "vsn", "scale"), ...) {
    .poplin_normalize(x, method = method, ...)
  }
)

##' @rdname poplin_normalize
setMethod(
  "poplin_normalize",
  "poplin",
  function(x, method = c("pqn",  "sum", "mean", "median",
                         "mad", "cyclicloess", "vsn", "scale"),
           xin, xout, ...) {
    m <- .verify_and_extract_input(x, xin)
    poplin_data(x, xout) <- .poplin_normalize(m, method = method, ...)
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
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param ref_samples a vector of sample names or indices to specify reference
##'   samples for the calculation of quotients. Must be a subset of
##'   \code{colnames(x)} if it is a character vector. If \code{NULL}, all
##'   samples are used.
##' @param min_frac a minimum proportion of reference samples for features to be
##'   considered in the calculation of a reference spectrum, between 0 and 1.
##'   For \code{min_frac = 1}, all reference samples should have non-missing
##'   values for the features to be included.
##' @param type a method to compute a reference spectrum. Either "mean" or
##'   "median".
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
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
##' @examples
##' 
##' ## poplin object
##' normalize_pqn(faahko_poplin, xin = "knn", xout = "knn_pqn")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_pqn(m)
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
  function(x, xin, xout, ref_samples = colnames(x), min_frac = 0.5,
           type = c("mean", "median")) {
    .poplin_extract_and_assign(x, .normalize_pqn,
                               xin, xout, ref_samples = ref_samples,
                               min_frac = min_frac, type = type)
  }
)

##' Sum normalization
##'
##' Apply sum normalization to a matrix or \linkS4class{poplin} object. For each
##' sample, feature intensities are divided by the sum of all intensity values.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param restrict logical controlling whether any feature with missing values
##'   is excluded from the calculation of normalization factors.
##' @param rescale logical controlling whether the normalized intensities are
##'   multiplied by the median of normalization factors to make look similar to
##'   their original scales.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_sum
##' @aliases
##' normalize_sum
##' normalize_sum,matrix-method
##' normalize_sum,poplin-method
##' @family normalization methods
##' @examples
##' 
##' ## poplin object
##' normalize_sum(faahko_poplin, xin = "knn", xout = "knn_sum")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_sum(m, rescale = TRUE)
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
  function(x, xin, xout, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_sum,
                               xin, xout,
                               restrict = restrict, rescale = rescale)
  }
)

##' Mean normalization
##'
##' Apply mean normalization to a matrix or \linkS4class{poplin} object. For
##' each sample, feature intensities are divided by its mean. The mean of
##' intensity values for individual samples will be one as a result.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param restrict logical controlling whether any feature with missing values
##'   is excluded from the calculation of normalization factors.
##' @param rescale logical controlling whether the normalized intensities are
##'   multiplied by the median of normalization factors to make look similar to
##'   their original scales.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_mean
##' @aliases
##' normalize_mean
##' normalize_mean,matrix-method
##' normalize_mean,poplin-method
##' @family normalization methods
##' @examples
##' ## poplin object
##' normalize_mean(faahko_poplin, xin = "knn", xout = "knn_mean")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_mean(m, rescale = TRUE)
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
  function(x, xin, xout, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_mean,
                               xin, xout,
                               restrict = restrict, rescale = rescale)
  }
)

##' Median normalization
##'
##' Apply median normalization to a matrix or \linkS4class{poplin} object. For
##' each sample, feature intensities are divided by its median. The median of
##' intensity values for individual samples will be one as a result.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param restrict logical controlling whether any feature with missing values
##'   is excluded from the calculation of normalization factors.
##' @param rescale logical controlling whether the normalized intensities are
##'   multiplied by the median of normalization factors to make look similar to
##'   their original scales.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_median
##' @aliases
##' normalize_median
##' normalize_median,matrix-method
##' normalize_median,poplin-method
##' @family normalization methods
##' @examples
##' ## poplin object
##' normalize_median(faahko_poplin, xin = "knn", xout = "knn_med")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_median(m, rescale = TRUE)
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
  function(x, xin, xout, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_median,
                               xin, xout,
                               restrict = restrict, rescale = rescale)
  }
)

##' Median absolute deviation (MAD) normalization
##'
##' Apply median absolute deviation (MAD) normalization to a matrix or
##' \linkS4class{poplin} object. For each sample, feature intensities are
##' divided by its MAD.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param restrict logical controlling whether any feature with missing values
##'   is excluded from the calculation of normalization factors.
##' @param rescale logical controlling whether the normalized intensities are
##'   multiplied by the median of normalization factors to make look similar to
##'   their original scales.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_mad
##' @aliases
##' normalize_mad
##' normalize_mad,matrix-method
##' normalize_mad,poplin-method
##' @family normalization methods
##' @examples
##' ## poplin object
##' normalize_mad(faahko_poplin, xin = "knn", xout = "knn_mad")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_mad(m, rescale = TRUE)
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
  function(x, xin, xout, restrict = FALSE, rescale = FALSE) {
    .poplin_extract_and_assign(x, .normalize_mad,
                               xin, xout,
                               restrict = restrict, rescale = rescale)
  }
)

##' Cyclic LOESS normalization
##'
##' Apply Cyclic LOESS normalization to a matrix or \linkS4class{poplin} object.
##' This is an interface to the [limma::normalizeCyclicLoess] from the
##' \pkg{limma} package. The input \code{x} is expected to contain
##' log-transformed raw intensities. See Bolstad et al. (2003) and Ballman et
##' al. (2004) for details.
##'
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param pre_log2 logical controlling whether feature intensities are
##'   log2-transformed before normalization.
##' @param type character specifying which variant of the cyclic LOESS
##'   method to use.
##' @param span span of LOESS smoothing window, between 0 and 1.
##' @param iterations number of times to cycle through all pairs of columns.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
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
##' @examples
##'
##' if (requireNamespace("limma", quietly = TRUE)) {
##'   ## poplin object
##'   normalize_cyclicloess(faahko_poplin, xin = "knn", xout = "knn_cyclic",
##'                        pre_log2 = TRUE)
##'   ## matrix
##'   m <- poplin_data(faahko_poplin, "knn")
##'   normalize_cyclicloess(m, pre_log2 = TRUE)
##' }
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
  function(x, xin, xout, pre_log2 = TRUE,
           type = c("fast", "affy", "pairs"), span = 0.7, iterations = 3) {
    .poplin_extract_and_assign(x, .normalize_cyclicloess,
                               xin, xout,
                               pre_log2 = pre_log2, type = type,
                               span = span, iterations = iterations,
                               weights = NULL)
  }
)

##' Variance stabilizing normalization (VSN)
##'
##' Apply variance stabilizing normalization (VSN) to a matrix or
##' \linkS4class{poplin} object. This is an interface to the
##' \link[vsn]{vsnMatrix} function from the \pkg{vsn} package (see [vsn::vsn2]
##' for help). The vsn produces normalized intensities based on a glog
##' (generalized logarithm) scale to base 2. See Huber et al. (2002) for
##' details.
##'
##' @references
##' Huber W, von Heydebreck A, Sültmann H, Poustka A, Vingron M. Variance
##' stabilization applied to microarray data calibration and to the
##' quantification of differential expression. Bioinformatics. 2002;18 Suppl
##' 1:S96-104. doi: 10.1093/bioinformatics/18.suppl_1.s96. PMID: 12169536.
##' 
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param meanSdPlot Logical controlling whether the function displays
##'   \link[vsn]{meanSdPlot} from the \pkg{vsn} package to visually check a
##'   dependence of the standard deviation on the mean.
##' @param ... additional arguments passed to \link[vsn]{vsnMatrix}.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
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
  function(x, xin, xout, meanSdPlot = FALSE, ...) {
    .poplin_extract_and_assign(x, .normalize_vsn,
                               xin, xout,
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
##' 
##' @references
##' van den Berg RA, Hoefsloot HC, Westerhuis JA, Smilde AK, van der Werf MJ.
##' Centering, scaling, and transformations: improving the biological
##' information content of metabolomics data. BMC Genomics. 2006 Jun 8;7:142.
##' doi: 10.1186/1471-2164-7-142. PMID: 16762068; PMCID: PMC1534033.
##' 
##' @param x a matrix or \linkS4class{poplin} object.
##' @param xin character specifying the name of data to retrieve from \code{x}
##'   when \code{x} is a poplin object.
##' @param xout character specifying the name of data to store in \code{x} when
##'   \code{x} is a poplin object.
##' @param type the scaling method to be applied.
##' @return a matrix or \linkS4class{poplin} object of the same dimension as
##'   \code{x} containing the normalized intensities.
##' @name normalize_scale
##' @aliases
##' normalize_scale
##' normalize_scale,matrix-method
##' normalize_scale,poplin-method
##' @family normalization methods
##' @examples
##' 
##' ## poplin object
##' normalize_scale(faahko_poplin, xin = "knn", xout = "knn_auto", type = "auto")
##'
##' ## matrix
##' m <- poplin_data(faahko_poplin, "knn")
##' normalize_scale(m, "pareto")
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
  function(x, xin, xout,
           type = c("auto", "range", "pareto", "vast", "level")) {
    .poplin_extract_and_assign(x, .normalize_scale,
                               xin, xout, type = type)
  }
)
