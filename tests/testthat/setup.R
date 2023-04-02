data(faahko_se)

## Subset to make tests quicker
faahko_sub <- faahko_se[1:50, ]
expect_true(anyNA(assay(faahko_sub, 1)))

## Add some covariates
faahko_sub$covar1 <- rep(c("A", "A", "A", "B", "B", "B"), 2)
faahko_sub$covar2 <- rep(c("C", "D"), 6)

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
