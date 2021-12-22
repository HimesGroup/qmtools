test_that("poplin_raw is the alias of assay.", {
  expect_equal(poplin_raw_names(d), assayNames(d))
  expect_equal(poplin_raw_list(d), assays(d))
  expect_equal(poplin_raw(d, "raw"), assay(d, "raw"))
  expect_equal(poplin_raw(d, "raw_filled"), assay(d, "raw_filled"))
})
