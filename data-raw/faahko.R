## faahKO SummarizedExperiment object
library(faahKO)
library(xcms)
library(poplin)
library(usethis)

data(faahko3)
faahko_se <- quantify(faahko3, filled = FALSE)
metadata(faahko_se) <- list()
assay(faahko_se, "raw_filled") <- featureValues(faahko3, filled = TRUE)
faahko_poplin <- as(faahko_se, "poplin")
use_data(faahko_se, faahko_poplin, overwrite = TRUE, version = 3)
