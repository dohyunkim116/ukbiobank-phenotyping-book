# R program PCA_UKB.tab created 2020-07-04 by ukb2r.cpp Mar 14 2018 14:22:05
# modified to use fread

library(data.table)

PCs <- fread("raw_data/PCA_UKB.tab", header=TRUE, sep="\t")
