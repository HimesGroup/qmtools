pp <- faahko_poplin
y <- factor(colData(pp)$sample_group, levels = c("WT", "KO"))

test_that("poplin_naplot works", {
  expect_error(poplin_naplot(pp, xin = "raw"), NA)

  m <- poplin_raw(pp, "raw")
  expect_error(poplin_naplot(m), NA)

  expect_error(poplin_naplot(m, label = TRUE), NA)
})

test_that("poplin_corplot works", {
  expect_error(poplin_corplot(pp, xin = "knn"), NA)

  m <- poplin_data(pp, "knn")
  expect_error(poplin_corplot(m), NA)

  expect_error(poplin_corplot(m, label = TRUE), NA)
})

test_that("poplin_boxplot works", {
  expect_error(poplin_boxplot(pp, xin = "knn", pre_log2 = TRUE), NA)

  m <- poplin_data(pp, "knn")
  expect_error(poplin_boxplot(m, pre_log2 = TRUE), NA)

  expect_error(poplin_boxplot(m, group = y, pre_log2 = FALSE, ylab = "I"), NA)
})

test_that("poplin_scoreplot works", {
  
  ## PCA
  expect_error(poplin_scoreplot(pp, xin = "pca", group = y), NA)

  m <- poplin_reduced(pp, "pca")
  expect_error(poplin_scoreplot(m, group = y, label = TRUE), NA)

  expect_error(poplin_scoreplot(m, group = y, label = TRUE, ellipse = TRUE), NA)

  ## t-SNE
  expect_error(poplin_scoreplot(pp, xin = "tsne", group = y), NA)

  m <- poplin_reduced(pp, "tsne")
  expect_error(poplin_scoreplot(m, group = y, label = TRUE), NA)

  expect_error(poplin_scoreplot(m, group = y, label = TRUE, ellipse = TRUE,
                                xlab = "X"), NA)

  ## PLS-DA
  expect_error(poplin_scoreplot(pp, xin = "plsda"), NA)

  m <- poplin_reduced(pp, "plsda")
  expect_error(poplin_scoreplot(m, group = y, label = TRUE), NA)

  expect_error(poplin_scoreplot(m, group = y, label = TRUE, ellipse = TRUE,
                                legend = FALSE), NA)
})

test_that("poplin_biplot works", {
  
  ## PCA
  expect_error(poplin_biplot(pp, xin = "pca", group = y), NA)

  m <- poplin_reduced(pp, "pca")
  expect_error(poplin_biplot(m, group = y, label = TRUE), NA)

  expect_error(poplin_biplot(m, group = y, label = TRUE,
                             arrow_label = FALSE), NA)

  ## PLS-DA
  expect_error(poplin_biplot(pp, xin = "plsda", group = y), NA)

  m <- poplin_reduced(pp, "plsda")
  expect_error(poplin_biplot(m, group = y, label = TRUE), NA)

  expect_error(poplin_biplot(m, group = y, label = TRUE,
                             arrow_col = "orange"), NA)

})
