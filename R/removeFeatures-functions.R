##' Feature filtering based on proportions of missing values
##'
##' Removes Features based on proportions of missing values in the matrix where
##' rows represent features and columns represent samples. Features can be
##' removed based on missing values within a specific group or multiple groups.
##' A feature will be retained, if there is at least one group with a proportion
##' of non-missing values above a cut-off.
##'
##' @param x A matrix-like object.
##' @param group A character vector for the information about each sample's
##'     group.
##' @param levels A string or character vector specifying one or more groups for
##'     filter filtering based on missing values. If \code{NULL}, all group
##'     levels in `group` will be used.
##' @param cut A numeric value between 0 and 1 specifying a minimum proportion
##'     of non-missing values to retain a feature.
##' @return A matrix containing the filtered features.
##'
##' @seealso See [removeFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' @examples
##'
##' data(faahko_se)
##' m <- assay(faahko_se, "raw")
##' g <- colData(faahko_se)$sample_group
##' table(g)
##'
##' ## Filter based on missing values in "KO" and "WT" groups
##' removeMiss(m, group = g, cut = 0.9)
##'
##' ## Consider only "KO" group (can be useful for QC-based filtering)
##' removeMiss(m, group = g, levels = "KO", cut = 0.9)
##'
##' @export
removeMiss <- function(x, group, levels = NULL, cut = 0.7) {
    idx_to_keep <- .removeMiss(x = x, group = group, levels = levels, cut = cut)
    x[idx_to_keep, , drop = FALSE]
}

.removeMiss <- function(x, group, levels = NULL, cut = 0.7) {
    if (is.null(levels)) {
        levels <- unique(group)
    }
    if (!all(levels %in% group)) {
        stop("All elements of `levels` should be a part of `group`")
    }
    idx_to_keep <- integer(0)
    for (level in levels) {
        m <- x[, level == group, drop = FALSE]
        non_missing_frac <- rowSums(!is.na(m)) / ncol(m)
        idx_to_keep <- union(idx_to_keep, which(non_missing_frac >= cut))
    }
    sort(idx_to_keep)
}

##' Feature Filtering based on RSD
##'
##' Removes Features with low reproducibility based on a relative standard
##' deviation (also known as coefficient of variation) of QC samples using the
##' data matrix where rows represent features and columns represent samples.
##' Features with a RSD above a cut-off will be removed from the data.
##'
##' @param x A matrix-like object.
##' @param qc_samples A vector of sample names or column indices specifying QC
##'     samples for the calculation of RSD. Must be a subset of
##'     \code{colnames(x)} if it is a character vector.
##' @param cut A numeric value between specifying a RSD cut-off to retain a
##'     feature.
##' @return A matrix containing the filtered features.
##'
##' @seealso See [removeFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' @examples
##'
##' set.seed(1e7)
##'
##' m_bio <- matrix(rlnorm(800, sdlog = 1), ncol = 20)
##' m_qc <- matrix(rlnorm(400, sdlog = 0.25), ncol = 10)
##' m <- cbind(m_bio, m_qc)
##' colnames(m) <- c(paste0("S", seq_len(20)), paste0("Q", seq_len(10)))
##'
##' removeRSD(m, qc_samples = paste0("Q", seq_len(10)))
##'
##' @export
removeRSD <- function(x, qc_samples, cut = 0.3) {
    idx_to_keep <- .removeRSD(x = x, qc_samples = qc_samples, cut = cut)
    x[idx_to_keep, , drop = FALSE]
}

.removeRSD <- function(x, qc_samples, cut = 0.3) {
    if (missing(qc_samples)) {
        stop("Please specify QC samples.")
    }
    m <- x[, qc_samples, drop = FALSE]
    rsd <- apply(m, 1, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
    which(rsd <= cut) # This drops rsd = NA
}

##' Feature Filtering based on ICC
##'
##' Removes Features based on a intraclass correlation coefficient (ICC) using
##' the data matrix where rows represent features and columns represent samples.
##' For each feature, ICC will be calculated using both biological and QC
##' samples to identify how much of the total variation is explained by
##' biological variability, as described in Schiffman, C et al (2019).
##' Informative features are expected to have relatively high variability across
##' the biological samples, compared to QC replicates. Features with an ICC
##' below a cut-off will be removed.
##'
##' @param x A matrix-like object.
##' @param qc_samples A vector of sample names or column indices specifying QC
##'     samples for the calculation of ICC. Must be a subset of
##'     \code{colnames(x)} if it is a character vector.
##' @param bio_samples A vector of sample names or column indices specifying
##'     biological samples for the calculation of ICC. Must be a subset of
##'     \code{colnames(x)} if it is a character vector.
##' @param cut A numeric value between 0 and 1 specifying a ICC cut-off to
##'     retain a feature.
##' @return A matrix containing the filtered features.
##'
##'
##' @references
##' Schiffman, C., Petrick, L., Perttula, K. et al. Filtering procedures for
##' untargeted LC-MS metabolomics data. BMC Bioinformatics 20, 334 (2019).
##' https://doi.org/10.1186/s12859-019-2871-9
##'
##' @seealso See [removeFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' @examples
##'
##' set.seed(1e7)
##'
##' m_bio_1 <- matrix(rlnorm(600, sdlog = 1), ncol = 20)
##' m_bio_2 <- matrix(rlnorm(200, sdlog = 0.3), ncol = 20)
##' m_bio <- rbind(m_bio_1, m_bio_2)
##' m_qc <- matrix(rlnorm(400, sdlog = 0.25), ncol = 10)
##' m <- cbind(m_bio, m_qc)
##' colnames(m) <- c(paste0("S", seq_len(20)), paste0("Q", seq_len(10)))
##'
##' removeICC(m, qc_samples = paste0("Q", seq_len(10)),
##'           bio_samples = paste0("S", seq_len(20)))
##' @export
removeICC <- function(x, qc_samples,
                      bio_samples = setdiff(colnames(x), qc_samples),
                      cut = 0.4) {
    idx_to_keep <- .removeICC(x = x, qc_samples = qc_samples,
                              bio_samples = bio_samples, cut = cut)
    x[idx_to_keep, , drop = FALSE]
}

.removeICC <- function(x, qc_samples, bio_samples, cut = 0.4) {
    .verify_package("nlme")
    if (missing(qc_samples)) {
        stop("Please specify QC samples.")
    }
    if (missing(bio_samples)) {
        stop("Please specify biological samples.")
    }
    m <- x[, c(bio_samples, qc_samples)]
    f <- factor(c(seq_len(length(bio_samples)), rep("Q", length(qc_samples))))
    icc_vec <- numeric(0)
    pb <- utils::txtProgressBar(1, nrow(x), style = 3)
    message("Calculating ICC...")
    for (i in seq_len(nrow(x))) {
        d <- data.frame(y = m[i, ], f = f)
        icc <- tryCatch({
            fit <- nlme::lme(y ~ 1,  random = ~ 1 | f, data = d,
                             na.action = na.omit)
            vv <- as.numeric(nlme::VarCorr(fit)[c(1, 2)])
            vv[1] / sum(vv)
        },
        error = function(e) NA_real_
        )
        icc_vec <- append(icc_vec, icc)
        utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    which(icc_vec >= cut) # This drops icc = NA
}

##' Feature Filtering based on QC/blank ratio
##'
##' Removes Features with based on QC/blank ratios using the data matrix where
##' rows represent features and columns represent samples. A feature will be
##' retained if there are not enough blank samples to calculate an intensity
##' ratio for a feature (or completely absent in blank samples). Use
##' [removeMiss] to remove features based on a proportion of missing values.
##' Features with a QC/blank ratio below a cut-off will be discarded.
##'
##' @param x A matrix-like object.
##' @param blank_samples A vector of sample names or column indices specifying
##'     blank samples for the calculation of ratio. Must be a subset of
##'     \code{colnames(x)} if it is a character vector.
##' @param qc_samples A vector of sample names or column indices specifying QC
##'     samples for the calculation of ratio. Must be a subset of
##'     \code{colnames(x)} if it is a character vector.
##' @param cut A numeric value greater than 1 specifying a QC/blank ratio
##'     cut-off to retain a feature.
##' @param type A method to compute a QC/blank ratio. Either "median" or
##'     "mean".
##' @param blank_min_n An integer value specifying the minimum number of blank
##'     samples to calculate a ratio.
##' @return A matrix containing the filtered features.
##'
##' @seealso See [removeFeatures] that provides a
##'   \linkS4class{SummarizedExperiment}-friendly wrapper for this function.
##'
##' @examples
##' set.seed(1e7)
##'
##' m_blank <- matrix(rlnorm(200), ncol = 5)
##' m_qc <- matrix(rlnorm(400, 1), ncol = 10)
##' m <- cbind(m_blank, m_qc)
##' colnames(m) <- c(paste0("B", seq_len(5)), paste0("Q", seq_len(10)))
##'
##' removeBlankRatio(m, blank_samples = paste0("B", seq_len(5)),
##'                  qc_samples = paste0("Q", seq_len(10)))
##'
##' @export
removeBlankRatio <- function(x, blank_samples, qc_samples, cut = 2,
                             type = c("median", "mean"), blank_min_n = 3) {
    idx_to_keep <- .removeBlankRatio(x = x, blank_samples = blank_samples,
                                     qc_samples = qc_samples, cut = cut,
                                     type = type, blank_min_n = blank_min_n)
    x[idx_to_keep, , drop = FALSE]
}

.removeBlankRatio <- function(x, blank_samples, qc_samples, cut = 2,
                              type = c("median", "mean"), blank_min_n = 3) {
    type <- match.arg(type)
    if (missing(qc_samples)) {
        stop("Please specify QC samples.")
    }
    if (missing(blank_samples)) {
        stop("Please specify Blank samples.")
    }
    m_qc <- x[, qc_samples, drop = FALSE]
    m_blank <- x[, blank_samples, drop = FALSE]
    if (type == "median") {
        qc_summary <- apply(m_qc, 1, median, na.rm = TRUE)
        blank_summary <- apply(m_blank, 1, median, na.rm = TRUE)
    } else {
        qc_summary <- rowMeans(m_qc, na.rm = TRUE)
        mean_summary <- rowMeans(m_blank, na.rm = TRUE)
    }
    ## Get indices to drop according to QC/blank ratio
    ## Trying to keep ratio = NA as we are uncertain about those features
    idx_to_drop <- which(qc_summary / blank_summary < cut)
    if (blank_min_n > 1) {
        ## The number of non-missing blanks is smaller than the predefined
        ## threshold: try not to remove those features as the calculated ratios
        ## would be unreliable
        blank_non_missing <- apply(m_blank, 1, function(x) sum(!is.na(x)))
        idx_to_drop <- setdiff(
            idx_to_drop, which(blank_non_missing < blank_min_n)
        )
    }
    setdiff(seq_len(nrow(x)), idx_to_drop)
}


