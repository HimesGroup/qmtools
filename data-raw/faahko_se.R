## faahKO SummarizedExperiment object
library(faahKO)
library(xcms)
library(usethis)

data(faahko3)
faahko_se <- quantify(faahko3)
use_data(faahko_se, overwrite = TRUE, version = 3)
