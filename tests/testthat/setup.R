data(faahko_se)

## Subset to make tests quicker
faahko_sub <- faahko_se[1:50, ]
expect_true(anyNA(assay(faahko_sub, 1)))

imputation_methods <- c("knn", "rf", "bpca", "QRILC", "MLE",
                        "MinDet", "MinProb", "min", "zero",
                        "mixed", "nbavg", "with", "none")

normalization_methods <- c("pqn", "div.sum", "div.mean",
                           "div.median", "div.mad",
                           "center.mean", "center.median",
                           "diff.median",
                           "cyclicloess", "vsn",
                           "quantiles", "quantiles.robust",
                           "feature.scale")
