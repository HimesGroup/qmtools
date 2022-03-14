test_that("imputeIntensity works for SummarizedExperiment.", {

  for (method in setdiff(imputation_methods, c("mixed", "with", "MLE", 
                                               "none", "nbavg"))) {
    set.seed(1e7)
    se <- imputeIntensity(faahko_sub, i = 1, name = "imp", method = method)
    expect_s4_class(se, "SummarizedExperiment")
    expect_false(anyNA(assay(se, "imp")))

    ## No name argument returns a matrix
    set.seed(1e7)
    out1 <- imputeIntensity(faahko_sub, i = 1, method = method)
    expect_true(is.matrix(out1))
    expect_false(anyNA(out1))

    ## Matrix input returns a matrix
    m <- assay(faahko_sub, 1)
    set.seed(1e7)
    out2 <- imputeIntensity(m, method = method)
    expect_true(is.matrix(out2))
    expect_false(anyNA(out2))

    expect_equal(assay(se, "imp"), out1)
    expect_equal(out1, out2)
  }

  ## mixed
  randna <- apply(assay(faahko_sub, 1), 1, anyNA)
  expect_error(
    imputeIntensity(faahko_sub, i = 1, method = "mixed",
                    randna = randna, mnar = "min"),
    regexp = "mar"
  )
  expect_error(
    imputeIntensity(faahko_sub, i = 1, method = "mixed",
                    randna = randna, mar = "knn"),
    regexp = "mnar"
  )
  expect_error(
    imputeIntensity(faahko_sub, i = 1, method = "mixed",
                    mnar = "min", mar = "knn"),
    regexp = "randna"
  )
  expect_error(
    imputeIntensity(faahko_sub, i = 1, method = "mixed", randna = TRUE,
                    mnar = "min", mar = "knn"),
    regexp = "randna"
  )
  out <- imputeIntensity(faahko_sub, i = 1, method = "mixed", randna = randna,
                         mnar = "min", mar = "knn")
  expect_false(anyNA(out))

  ## none
  out <- imputeIntensity(faahko_sub, i = 1, method = "none")
  expect_identical(out, assay(faahko_sub, 1))
})

test_that("imputeKNN works.", {
  m <- assay(faahko_sub, 1)

  ## Gower
  out_g <- imputeKNN(m, by = "sample", type = "gower")
  expect_false(anyNA(out_g))
  
  ## Euclidean
  out_e <- imputeKNN(m, type = "euclidean", scale = TRUE)
  expect_false(anyNA(out_e))
  expect_error(
    imputeKNN(m, type = "zzz")
  )
})

