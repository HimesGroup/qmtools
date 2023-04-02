test_that("clusterFeatures works.", {

    ## Incorrect rtime varname
    expect_error(
        clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "zzz")
    )

    ## Function generates "rtime_group" and "feature_group" in rowData
    se <- clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "rtmed")
    expect_s4_class(se, "SummarizedExperiment")
    expect_true(all(c("rtime_group", "feature_group") %in% names(rowData(se))))

    ## Function with cor_grouping = "none" generate no feature group column
    se <- clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "rtmed",
                          cor_grouping = "none")
    expect_true("rtime_group" %in% names(rowData(se)))
    expect_false("feature_group" %in% names(rowData(se)))

    ## For missing values, cor_use must be dealt with `cor_use`
    expect_error(
        clusterFeatures(faahko_sub, 2, rtime_var = "rtmed", log2 = TRUE)
    )
    se <- clusterFeatures(faahko_sub, 2, rtime_var = "rtmed",
                          cor_use = "pairwise.complete.obs", log2 = TRUE)
    expect_s4_class(se, "SummarizedExperiment")
    expect_true(all(c("rtime_group", "feature_group") %in% names(rowData(se))))

    ## Test different grouping options
    se <- clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "rtmed",
                          rt_grouping = "closest", cor_grouping = "connected")
    expect_s4_class(se, "SummarizedExperiment")
    expect_true(all(c("rtime_group", "feature_group") %in% names(rowData(se))))

    se <- clusterFeatures(faahko_sub, i = "knn_vsn", rtime_var = "rtmed",
                          rt_grouping = "consecutive",
                          cor_grouping = "SimilarityMatrix")
    expect_s4_class(se, "SummarizedExperiment")
    expect_true(all(c("rtime_group", "feature_group") %in% names(rowData(se))))

})

