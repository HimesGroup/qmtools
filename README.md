# qmtools

Overview
----------

The goal of qmtools is to provide basic tools for quantitative
metabolomics data processing, including

  - Missing value imputation
  - Data normalization
  - Dimension reduction
  - Feature filtering
  - Feature clustering
  - Visualization
  
  
Installation
---------------

To install this package, start R (version "4.2") and enter:

```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# The following initializes usage of Bioc devel
BiocManager::install(version='devel')

BiocManager::install("qmtools")
```

Or install via GitHub:

```r
remotes::install_github("HimesGroup/qmtools")
```
