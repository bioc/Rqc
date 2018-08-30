# Rqc

[![Build Status](https://travis-ci.org/labbcb/Rqc.svg?branch=master)](https://travis-ci.org/labbcb/Rqc)
[![codecov](https://codecov.io/gh/labbcb/Rqc/branch/master/graph/badge.svg)](https://codecov.io/gh/labbcb/Rqc)


Rqc is an optimized tool designed for quality control and assessment of 
high-throughput sequencing data. It performs parallel processing of entire files
and produces a report which contains a set of high-resolution graphics.

## Installation
Install via Bioconductor, stable version
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("Rqc")
```

Install via GitHub, development vertion
```r
install.packages("devtools")
library(devtools)
install_github("labbcb/Rqc")
```
