test_that("reduceFeatures work.", {
    ## Missing
    m <- assay(faahko_sub, i = "raw")
    out1 <- removeFeatures(faahko_sub, i = "raw", method = "missing",
                           group = colData(faahko_sub)$sample_group, cut = 0.9)
    expect_s4_class(out1, "SummarizedExperiment")
    out2 <- removeFeatures(m, method = "missing",
                           group = colData(faahko_sub)$sample_group, cut = 0.9)
    expect_identical(assay(out1, "raw"), out2)

    ## RSD
    m <- assay(faahko_sub, i = "raw")
    out1 <- removeFeatures(faahko_sub, i = "raw", method = "rsd",
                           qc_samples = 1:6)
    expect_s4_class(out1, "SummarizedExperiment")
    out2 <- removeFeatures(m, method = "rsd", qc_samples = 1:6)
    expect_identical(assay(out1, "raw"), out2)

    ## ICC
    m <- assay(faahko_sub, i = "raw")
    out1 <- removeFeatures(faahko_sub, i = "raw", method = "icc",
                           qc_samples = 1:6, bio_samples = 7:12)
    expect_s4_class(out1, "SummarizedExperiment")
    out2 <- removeFeatures(m, method = "icc", qc_samples = 1:6,
                           bio_samples = 7:12)
    expect_identical(assay(out1, "raw"), out2)


    ## QC/blank ratio
    m <- assay(faahko_sub, i = "raw")
    out1 <- removeFeatures(faahko_sub, i = "raw", method = "blankratio",
                           qc_samples = 1:6, blank_samples = 7:12)
    expect_s4_class(out1, "SummarizedExperiment")
    out2 <- removeFeatures(m, method = "blankratio", qc_samples = 1:6,
                           blank_samples = 7:12)
    expect_identical(assay(out1, "raw"), out2)
})
