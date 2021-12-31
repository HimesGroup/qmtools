## faahKO data 
library(faahKO)
library(xcms)
library(poplin)
library(usethis)
library(VIM)
library(limma)
library(Rtsne)
library(pls)

set.seed(1e7)

data(faahko3)
faahko_se <- quantify(faahko3, filled = FALSE)
metadata(faahko_se) <- list()
assay(faahko_se, "raw_filled") <- featureValues(faahko3, filled = TRUE)
faahko_poplin <- as(faahko_se, "poplin")
faahko_poplin <- poplin_impute(
  faahko_poplin, method = "knn", xin = "raw", xout = "knn"
)
faahko_poplin <- poplin_normalize(
  faahko_poplin, method = "cyclicloess",
  xin = "knn", xout = "knn_cyclic", pre_log2 = TRUE
)
faahko_poplin <- poplin_reduce(
  faahko_poplin, method = "pca",
  xin = "knn_cyclic", xout = "pca", center = TRUE
)
faahko_poplin <- poplin_reduce(
  faahko_poplin, method = "tsne",
  xin = "knn_cyclic", xout = "tsne", normalize = TRUE,
  perplexity = 3
)
y <- factor(colData(faahko_poplin)$sample_group, levels = c("WT", "KO"))
faahko_poplin <- poplin_reduce(
  faahko_poplin, method = "plsda",
  xin = "knn_cyclic", xout = "plsda", y = y)
use_data(faahko_poplin, overwrite = TRUE, version = 3)
