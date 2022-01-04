pp <- empty
poplin_data(pp, "knn") <- poplin_data(faahko_poplin, "knn")

test_that("poplin_normalize works.", {
  pp2 <- pp

  pp2 <- poplin_normalize(pp2, xin = "knn", xout = "pqn")
  expect_identical(poplin_data_names(pp2), c("knn", "pqn"))
  m <- poplin_data(pp2, "knn")
  out <- poplin_normalize(m)
  expect_identical(poplin_data(pp2, "pqn"), out)

  skip_if_not_installed("limma")
  pp2 <- poplin_normalize(pp2, xin = "knn", xout = "cyclic",
                          method = "cyclicloess", pre_log2 = TRUE)
  expect_identical(poplin_data_names(pp2), c("knn", "pqn", "cyclic"))
  m <- poplin_data(pp2, "knn")
  out <- poplin_normalize(m, method = "cyclicloess", pre_log2 = TRUE)
  expect_identical(poplin_data(pp2, "cyclic"), out)
})

test_that("normalize_pqn works.", {
  pp2 <- pp
  pp2 <- normalize_pqn(pp2, xin = "knn", xout = "pqn")
  expect_identical(poplin_data_names(pp2), c("knn", "pqn"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_pqn(m)

  expect_identical(poplin_data(pp2, "pqn"), out)
})

test_that("normalize_pqn works.", {
  pp2 <- pp
  pp2 <- normalize_pqn(pp2, xin = "knn", xout = "pqn")
  expect_identical(poplin_data_names(pp2), c("knn", "pqn"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_pqn(m)
  expect_identical(poplin_data(pp2, "pqn"), out)

  out2 <- normalize_pqn(m, ref_samples = colnames(m)[1:4])
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_pqn works.", {
  pp2 <- pp
  pp2 <- normalize_pqn(pp2, xin = "knn", xout = "pqn")
  expect_identical(poplin_data_names(pp2), c("knn", "pqn"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_pqn(m)

  expect_identical(poplin_data(pp2, "pqn"), out)

  out2 <- normalize_pqn(m, ref_samples = colnames(m)[1:4])
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_cyclicloess works.", {
  skip_if_not_installed("limma")
  pp2 <- pp
  pp2 <- normalize_cyclicloess(pp2, xin = "knn", xout = "cyclic",
                               pre_log2 = TRUE)
  expect_identical(poplin_data_names(pp2), c("knn", "cyclic"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_cyclicloess(m, pre_log2 = TRUE)
  expect_identical(poplin_data(pp2, "cyclic"), out)

  out2 <- normalize_cyclicloess(m, pre_log2 = TRUE, type = "affy")
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_sum works.", {
  pp2 <- pp
  pp2 <- normalize_sum(pp2, xin = "knn", xout = "sum")
  expect_identical(poplin_data_names(pp2), c("knn", "sum"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_sum(m)

  expect_identical(poplin_data(pp2, "sum"), out)

  out2 <- normalize_sum(m, rescale = TRUE)
  expect_false(isTRUE(all.equal(out, out2)))
})


test_that("normalize_median works.", {
  pp2 <- pp
  pp2 <- normalize_median(pp2, xin = "knn", xout = "median")
  expect_identical(poplin_data_names(pp2), c("knn", "median"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_median(m)

  expect_identical(poplin_data(pp2, "median"), out)

  out2 <- normalize_median(m, rescale = TRUE)
  expect_false(isTRUE(all.equal(out, out2)))
})


test_that("normalize_mean works.", {
  pp2 <- pp
  pp2 <- normalize_mean(pp2, xin = "knn", xout = "mean")
  expect_identical(poplin_data_names(pp2), c("knn", "mean"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_mean(m)

  expect_identical(poplin_data(pp2, "mean"), out)

  out2 <- normalize_mean(m, rescale = TRUE)
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_mad works.", {
  pp2 <- pp
  pp2 <- normalize_mad(pp2, xin = "knn", xout = "mad")
  expect_identical(poplin_data_names(pp2), c("knn", "mad"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_mad(m)

  expect_identical(poplin_data(pp2, "mad"), out)

  out2 <- normalize_mad(m, rescale = TRUE)
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_vsn works.", {
  skip_if_not_installed("vsn")
  pp2 <- pp
  pp2 <- normalize_vsn(pp2, xin = "knn", xout = "vsn")
  expect_identical(poplin_data_names(pp2), c("knn", "vsn"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_vsn(m)

  expect_identical(poplin_data(pp2, "vsn"), out)

  out2 <- normalize_vsn(m, lts.quantile = 0.8)
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("normalize_scale works.", {
  pp2 <- pp

  ## Auto scaling
  pp2 <- normalize_scale(pp2, xin = "knn", xout = "auto")
  expect_identical(poplin_data_names(pp2), c("knn", "auto"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_scale(m)

  expect_identical(poplin_data(pp2, "auto"), out)

  ## Range scaling
  pp2 <- normalize_scale(pp2, xin = "knn", xout = "range", type = "range")
  expect_identical(poplin_data_names(pp2), c("knn", "auto", "range"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_scale(m, type = "range")

  expect_identical(poplin_data(pp2, "range"), out)

  ## Pareto scaling
  pp2 <- normalize_scale(pp2, xin = "knn", xout = "pareto", type = "pareto")
  expect_identical(poplin_data_names(pp2), c("knn", "auto", "range", "pareto"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_scale(m, type = "pareto")

  expect_identical(poplin_data(pp2, "pareto"), out)

  ## Vast scaling
  pp2 <- normalize_scale(pp2, xin = "knn", xout = "vast", type = "vast")
  expect_identical(poplin_data_names(pp2),
                   c("knn", "auto", "range", "pareto", "vast"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_scale(m, type = "vast")

  expect_identical(poplin_data(pp2, "vast"), out)

  ## Level scaling
  pp2 <- normalize_scale(pp2, xin = "knn", xout = "level", type = "level")
  expect_identical(poplin_data_names(pp2),
                   c("knn", "auto", "range", "pareto", "vast", "level"))

  m <- poplin_data(pp2, "knn")
  out <- normalize_scale(m, type = "level")

  expect_identical(poplin_data(pp2, "level"), out)

})
