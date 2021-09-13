##' @export
pqn_args <- function(...) {
  new("pqn_args", ...)
}

sample_normalizer_args <- function(...) {
  new("sample_normalizer_args", ...)
}

tic_args <- function(dat_in, dat_out, restrict = TRUE, rescale = FALSE) {
  new("sample_normalizer_args", normalizer = "tic",
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
