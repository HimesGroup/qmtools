pp <- empty
poplin_data_list(pp) <- poplin_data_list(faahko_poplin)

test_that("poplin_reduce works.", {
  pp2 <- pp

  pp2 <- poplin_reduce(pp2, xin = "knn_cyclic", xout = "pca")
  expect_identical(poplin_reduced_names(pp2), "pca")
  m <- poplin_data(pp2, "knn_cyclic")
  out <- poplin_reduce(m)
  expect_identical(poplin_reduced(pp2, "pca"), out)

  skip_if_not_installed("Rtsne")
  set.seed(100)
  pp2 <- poplin_reduce(pp2, xin = "knn_cyclic", xout = "tsne",
                       method = "tsne", perplexity = 3)
  expect_identical(poplin_reduced_names(pp2), c("pca", "tsne"))

  m <- poplin_data(pp2, "knn_cyclic")
  set.seed(100)
  out <- poplin_reduce(m, method = "tsne", perplexity = 3)
  expect_identical(poplin_reduced(pp2, "tsne"), out)
})

test_that("reduce_pca works.", {
  pp2 <- pp

  ## Non-missing
  pp2 <- reduce_pca(pp2, xin = "knn_cyclic", xout = "svd")
  expect_identical(poplin_reduced_names(pp2), "svd")
  
  m <- poplin_data(pp2, "knn_cyclic")
  out <- reduce_pca(m)
  expect_identical(poplin_reduced(pp2, "svd"), out)

  out2 <- reduce_pca(m, ncomp = 3)
  expect_false(isTRUE(all.equal(out, out2)))

  ## Missing
  skip_if_not_installed("pcaMethods")
  pp2 <- reduce_pca(pp2, xin = "raw_filled", xout = "nipals")
  expect_identical(poplin_reduced_names(pp2), c("svd", "nipals"))

  m <- poplin_raw(pp2, "raw_filled")
  out <- reduce_pca(m)
  expect_identical(poplin_reduced(pp2, "nipals"), out)

  out2 <- reduce_pca(m, ncomp = 3)
  expect_false(isTRUE(all.equal(out, out2)))
})

test_that("reduce_tsne works.", {
  skip_if_not_installed("Rtsne")
  pp2 <- pp

  ## Non-missing
  set.seed(100)
  pp2 <- reduce_tsne(pp2, xin = "knn_cyclic", xout = "tsne", perplexity = 3)
  expect_identical(poplin_reduced_names(pp2), "tsne")
  
  m <- poplin_data(pp2, "knn_cyclic")
  set.seed(100)
  out <- reduce_tsne(m, perplexity = 3)
  expect_identical(poplin_reduced(pp2, "tsne"), out)

  set.seed(1000)
  out2 <- reduce_tsne(m, perplexity = 3)
  expect_false(isTRUE(all.equal(out, out2)))

  ## Missing
  expect_error(pp2 <- reduce_tsne(pp2, xin = "raw", xout = "tsne"))
})

test_that("reduce_plsda works.", {
  skip_if_not_installed("pls")
  pp2 <- pp
  y <- factor(colData(pp2)$sample_group, levels = c("WT", "KO"))

  ## Non-missing
  pp2 <- reduce_plsda(pp2, xin = "knn_cyclic", xout = "plsda", y = y)
  expect_identical(poplin_reduced_names(pp2), "plsda")
  
  m <- poplin_data(pp2, "knn_cyclic")
  out <- reduce_plsda(m, y = y)
  expect_identical(poplin_reduced(pp2, "plsda"), out)

  out2 <- reduce_plsda(m, y = y, ncomp = 3)
  expect_false(isTRUE(all.equal(out, out2)))

  ## Missing in y
  y2 <- y
  y2[1] <- NA
  expect_error(pp2 <- reduce_plsda(pp2, xin = "knn_cyclic", xout = "plsda",
                                   y = y2))
  
  ## Missing in x
  expect_error(pp2 <- reduce_plsda(pp2, xin = "raw", xout = "plsda", y = y))
})
