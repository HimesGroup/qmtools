##' @export
pqn_args <- function(dat_in, dat_out, ref_ids = NULL, min_frac = 0.5,
                     type = "mean") {
  new("pqn_args", dat_in = dat_in, dat_out = dat_out,
      ref_ids = ref_ids, min_frac = min_frac)
}

sample_normalizer_args <- function(...) {
  new("sample_normalizer_args", ...)
}

sum_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "sum",
      dat_in = dat_in, dat_out = dat_out,
      restrict = restrict, rescale = rescale)
}

mean_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "mean",
      dat_in = dat_in, dat_out = dat_out,
      restrict = restrict, rescale = rescale)
}

median_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "median",
      dat_in = dat_in, dat_out = dat_out,
      restrict = restrict, rescale = rescale)
}

mad_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "mad",
      dat_in = dat_in, dat_out = dat_out,
      restrict = restrict, rescale = rescale)
}

euclidean_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "euclidean",
      dat_in = dat_in, dat_out = dat_out,
      restrict = restrict, rescale = rescale)
}

cyclicloess_args <- function(dat_in, dat_out, weights = NULL, span = 0.7,
                             iterations = 3, method = "fast") {
  new("cyclicloess_args", 
      dat_in = dat_in, dat_out = dat_out,
      weights = weights, span = span, iterations = as.integer(iterations),
      method = method)
}
