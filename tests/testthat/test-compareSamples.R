test_that("compareSamples works.", {

  ## Incorrect group varname
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_grp")
  )

  ## Incorrect group level specification
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                   class1 = "WW", class2 = "KK")
  )
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                   class1 = "WT", class2 = "KK")
  )
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                   class1 = "WW", class2 = "KK")
  )

  ## Incorrect covariates varnames
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                   covariates = c("covar3", "covar4")) ## not covar3 and covar4
  )
  expect_error(
    compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                   covariates = c("covar1", "covar3")) ## not covar3
  )

  ## Confidence interval when confint is enabled
  mod1 <- compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                        confint = TRUE)
  expect_s3_class(mod1, "data.frame")
  expect_true(all(c("CI.L", "CI.R") %in% names(mod1)))

  mod2 <- compareSamples(faahko_sub, i = "knn_vsn", group = "sample_group",
                         confint = FALSE)
  expect_s3_class(mod2, "data.frame")
  expect_false(all(c("CI.L", "CI.R") %in% names(mod2)))

})
