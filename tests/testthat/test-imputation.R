test_that("poplin_impute works.", {
  pp <- empty
  pp <- poplin_impute(pp, method = "univariate",
                      xin = "raw", xout = "halfmin", type = "halfmin")
  expect_identical(poplin_data_names(pp), "halfmin")
  m <- poplin_raw(pp, "raw")
  out <- poplin_impute(m, method = "univariate", type = "halfmin")
  expect_identical(poplin_data(pp, "halfmin"), out)

  skip_if_not_installed("VIM")
  pp <- poplin_impute(pp, method = "knn",
                      xin = "raw", xout = "knn")
  expect_identical(poplin_data_names(pp), c("halfmin", "knn"))
  m <- poplin_raw(pp, "raw")
  out <- poplin_impute(m, method = "knn")
  expect_identical(poplin_data(pp, "knn"), out)
})

test_that("impute_knn works.", {
  skip_if_not_installed("VIM")
  pp <- empty

  set.seed(100)
  pp <- impute_knn(pp, xin = "raw", xout = "knn")
  expect_identical(poplin_data_names(pp), "knn")
  expect_false(anyNA(poplin_data(pp, "knn")))
  m <- poplin_raw(pp, "raw")
  out <- impute_knn(m)
  expect_identical(poplin_data(pp, "knn"), out)

  set.seed(1e7)
  out2 <-  impute_knn(m)
  expect_identical(poplin_data(faahko_poplin, "knn"), out)
})

test_that("impute_randomforest works.", {
  skip_if_not_installed("missForest")
  pp <- empty
  ## prevent warnings by filtering too many missing for test
  raw_int <- poplin_raw(pp, "raw")
  idx <- which(apply(raw_int, 1, function(x) sum(is.na(x))) < 5)
  pp2 <- pp[idx, ]

  set.seed(100)
  pp2 <- impute_randomforest(pp2, xin = "raw", xout = "rf")
  expect_identical(poplin_data_names(pp2), "rf")
  expect_false(anyNA(poplin_data(pp2, "rf")))

  set.seed(100)
  m <- poplin_raw(pp2, "raw")
  out <- impute_randomforest(m)
  expect_identical(poplin_data(pp2, "rf"), out)

  set.seed(1000)
  out2 <- impute_randomforest(m)
  expect_false(isTRUE(all.equal(poplin_data(pp2, "rf"), out2)))
})

test_that("impute_pca works.", {
  skip_if_not_installed("pcaMethods")
  pp <- empty

  ## NIPALS
  pp <- impute_pca(pp, xin = "raw", xout = "nipals")
  expect_identical(poplin_data_names(pp), "nipals")
  expect_false(anyNA(poplin_data(pp, "nipals")))

  m <- poplin_raw(pp, "raw")
  out <- impute_pca(m)
  expect_identical(poplin_data(pp, "nipals"), out)

  ## BPCA
  pp <- impute_pca(pp, xin = "raw", xout = "bpca", type = "bpca")
  expect_identical(poplin_data_names(pp), c("nipals", "bpca"))
  expect_false(anyNA(poplin_data(pp, c("bpca"))))

  m <- poplin_raw(pp, "raw")
  out <- impute_pca(m, type = "bpca")
  expect_identical(poplin_data(pp, "bpca"), out)

  ## PPCA
  pp <- impute_pca(pp, xin = "raw", xout = "ppca", type = "ppca", seed = 100)
  expect_identical(poplin_data_names(pp), c("nipals", "bpca", "ppca"))
  expect_false(anyNA(poplin_data(pp, c("ppca"))))

  m <- poplin_raw(pp, "raw")
  out <- impute_pca(m, type = "ppca", seed = 100)
  expect_identical(poplin_data(pp, "ppca"), out)

  out2 <- impute_pca(m, type = "ppca", seed = 1000)
  expect_false(isTRUE(all.equal(poplin_data(pp, "ppca"), out2)))

  ## SVD
  pp <- impute_pca(pp, xin = "raw", xout = "svd", type = "svd")
  expect_identical(poplin_data_names(pp), c("nipals", "bpca", "ppca", "svd"))
  expect_false(anyNA(poplin_data(pp, c("svd"))))

  m <- poplin_raw(pp, "raw")
  out <- impute_pca(m, type = "svd")
  expect_identical(poplin_data(pp, "svd"), out)
})

test_that("impute_univariate works.", {
  pp <- empty
  
  ## Half-min
  pp <- impute_univariate(pp, xin = "raw", xout = "halfmin")
  expect_identical(poplin_data_names(pp), "halfmin")
  expect_false(anyNA(poplin_data(pp, "halfmin")))

  m <- poplin_raw(pp, "raw")
  out <- impute_univariate(m)
  expect_identical(poplin_data(pp, "halfmin"), out)

  ## Median
  pp <- impute_univariate(pp, xin = "raw", xout = "median", type = "median")
  expect_identical(poplin_data_names(pp), c("halfmin", "median"))
  expect_false(anyNA(poplin_data(pp, "median")))

  m <- poplin_raw(pp, "raw")
  out <- impute_univariate(m, type = "median")
  expect_identical(poplin_data(pp, "median"), out)

  ## Mean
  pp <- impute_univariate(pp, xin = "raw", xout = "mean", type = "mean")
  expect_identical(poplin_data_names(pp), c("halfmin", "median", "mean"))
  expect_false(anyNA(poplin_data(pp, "mean")))

  m <- poplin_raw(pp, "raw")
  out <- impute_univariate(m, type = "mean")
  expect_identical(poplin_data(pp, "mean"), out)
})
