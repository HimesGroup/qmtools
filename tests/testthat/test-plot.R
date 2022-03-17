g <- colData(faahko_sub)$sample_group

test_that("plotMiss works.", {
    p1 <- plotMiss(faahko_sub, i = "raw")
    expect_s3_class(p1, "patchwork")
    p2 <- plotMiss(assay(faahko_sub, i = "raw"), group = g)
    expect_s3_class(p2, "patchwork")
})

test_that("plotBox works.", {
    p1 <- plotBox(faahko_sub, i = "raw")
    expect_s3_class(p1, "ggplot")
    p2 <- plotBox(assay(faahko_sub, i = "raw"), group = g)
    expect_s3_class(p2, "ggplot")
})

test_that("plotReduced works.", {
    out_pca <- reduceFeatures(faahko_sub, i = "knn_vsn", method = "pca")
    out_tsne <- reduceFeatures(faahko_sub, i = "knn_vsn", method = "tsne",
                               perplexity = 1)
    out_plsda <- reduceFeatures(faahko_sub, i = "knn_vsn", method = "plsda",
                                y = factor(g))
    p1 <- plotReduced(out_pca, group = g)
    expect_s3_class(p1, "ggplot")
    p2 <- plotReduced(out_tsne, group = g)
    expect_s3_class(p2, "ggplot")
    p3 <- plotReduced(out_plsda, biplot = TRUE)
    expect_s3_class(p3, "ggplot")
})

test_that("plotRTgroup works.", {
    se <- clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "rtmed")

    expect_null(plotRTgroup(se, i = "knn_vsn", group = "FG.01", type = "graph"))
    expect_null(plotRTgroup(se, i = "knn_vsn", group = "FG.01", type = "pairs"))
})
