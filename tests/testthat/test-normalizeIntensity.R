test_that("normalizeIntensity works.", {

  for (method in normalization_methods) {
    se <- normalizeIntensity(faahko_sub, i = "knn", name = "norm",
                             method = method)
    expect_s4_class(se, "SummarizedExperiment")

    ## No name argument returns a matrix
    out1 <- normalizeIntensity(faahko_sub, i = "knn", method = method)
    expect_true(is.matrix(out1))

    ## Matrix input returns a matrix
    m <- assay(faahko_sub, "knn")
    out2 <- normalizeIntensity(m, method = method)
    expect_true(is.matrix(out2))

    expect_equal(assay(se, "norm"), out1)
    expect_equal(out1, out2)
  }
})


test_that("normalizePQN works.", {
  ## No missing
  m <- assay(faahko_sub, "knn")
  out1 <- normalizePQN(m, ref_samples = NULL)
  out2 <- normalizePQN(m, ref_samples = colnames(m))
  expect_identical(out1, out2)

  ## Missing
  m <- assay(faahko_sub, "raw")
  out <- normalizePQN(m, min_frac = 1)
  out2 <- normalizePQN(m, min_frac = 0)
  expect_false(identical(out1, out2))
})

test_that("scaleCols works.", {
  m <- assay(faahko_sub, "knn")
  out <- scaleCols(m, "div.sum")
  expect_equal(var(apply(out, 2, sum)), 0)

  out <- scaleCols(m, "div.mean")
  out2 <- scaleCols(m, "div.mean", rescale = TRUE)
  expect_equal(var(apply(out, 2, mean)), 0)
  expect_false(identical(out, out2))
  expect_equal(var(as.vector(out / out2)), 0)

  ## With missing
  m <- assay(faahko_sub, "raw")
  out <- scaleCols(m, "div.median")
  expect_equal(var(apply(out, 2, median, na.rm = TRUE)), 0)

  out <- scaleCols(m, "div.mad")
  out2 <- scaleCols(m, "div.mad", restrict = TRUE)
  expect_equal(var(apply(out, 2, mad, na.rm = TRUE)), 0)
  expect_false(identical(out, out2))
})

test_that("scaleRows works.", {
  m <- assay(faahko_sub, "knn")
  out  <- scaleRows(m, "auto")
  out2 <- t(apply(m, 1, function(x) (x - mean(x)) / sd(x)))
  expect_equal(out, out2)

  ## With missing
  m <- assay(faahko_sub, "raw")
  out  <- scaleRows(m, "max")
  out2 <- t(apply(m, 1, function(x) x / max(x, na.rm = TRUE)))
  expect_equal(out, out2, ignore_attr = TRUE)
})
