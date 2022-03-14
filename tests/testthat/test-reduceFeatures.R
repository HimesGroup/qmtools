test_that("reduceFeatures works.", {
  ## PCA with missing
  m <- assay(faahko_sub, i = "raw")
  out1 <- reduceFeatures(faahko_sub, i = "raw", name = "pca", method = "pca")
  expect_s3_class(out1, "reduced.pca")
  out2 <- reduceFeatures(m, method = "pca")
  expect_identical(out1, out2)

  ## PCA
  m <- assay(faahko_sub, i = "knn_vsn")
  out1 <- reduceFeatures(faahko_sub, i = "knn_vsn", name = "pca", method = "pca",
                         ncomp = 3)
  expect_s3_class(out1, "reduced.pca")
  out2 <- reduceFeatures(faahko_sub, i = "knn_vsn", name = "pca", method = "pca",
                         ncomp = 2)
  out3 <- reduceFeatures(m, method = "pca", ncomp = 2)
  expect_false(identical(dim(out1), dim(out3)))
  expect_identical(out2, out3)

  ## t-SNE
  m <- assay(faahko_sub, i = "knn_vsn")
  set.seed(1e7)
  out1 <- reduceFeatures(faahko_sub, i = "knn_vsn", name = "tsne",
                         method = "tsne", perplexity = 3)
  expect_s3_class(out1, "reduced.tsne")
  set.seed(1e7)
  out2 <- reduceFeatures(m, method = "tsne", perplexity = 3)
  expect_identical(out1, out2)
  expect_error(
    reduceFeatures(faahko_sub, i = "raw", method = "tsne", perplexity = 3)
  )

  ## PLS-DA
  y <- factor(colData(faahko_sub)$sample_group)
  m <- assay(faahko_sub, i = "knn_vsn")
  out1 <- reduceFeatures(faahko_sub, i = "knn_vsn", name = "tsne",
                         method = "plsda", y = y)
  expect_s3_class(out1, "reduced.plsda")
  out2 <- reduceFeatures(m, method = "plsda", y = y)
  expect_identical(out1, out2)
  expect_error(
    reduceFeatures(faahko_sub, i = "raw", method = "plsda", y = y) # missing
  )
  expect_error(
    reduceFeatures(faahko_sub, i = "raw", method = "plsda",
                   y = colData(faahko_sub)$sample_group) # not factor
  )
})
