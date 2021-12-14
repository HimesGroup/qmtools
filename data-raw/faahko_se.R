## faahKO SummarizedExperiment object
library(faahKO)
library(xcms)
library(usethis)

data(faahko3)
faahko_se <- quantify(faahko3)
faahko_poplin <- as(faahko_se, "poplin")
use_data(faahko_se, faahko_poplin, overwrite = TRUE, version = 3)
