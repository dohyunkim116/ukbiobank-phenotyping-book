# R program sampleQC_UKB.tab created 2020-07-04 by ukb2r.cpp Mar 14 2018 14:22:05

library(data.table)

sampleqc <- fread("raw_data/sampleQC_UKB.tab", header=TRUE, sep="\t")
sampleqc <- data.frame(sampleqc)

lvl.0009 <- c(0,1)
lbl.0009 <- c("Female","Male")
sampleqc$f.22001.0.0 <- ordered(sampleqc$f.22001.0.0, levels=lvl.0009, labels=lbl.0009)
lvl.1002 <- c(1)
lbl.1002 <- c("Caucasian")
sampleqc$f.22006.0.0 <- ordered(sampleqc$f.22006.0.0, levels=lvl.1002, labels=lbl.1002)
lvl.0001 <- c(1)
lbl.0001 <- c("Yes")
sampleqc$f.22019.0.0 <- ordered(sampleqc$f.22019.0.0, levels=lvl.0001, labels=lbl.0001)
sampleqc$f.22020.0.0 <- ordered(sampleqc$f.22020.0.0, levels=lvl.0001, labels=lbl.0001)
lvl.0682 <- c(-1,0,1,10)
lbl.0682 <- c("Participant excluded from kinship inference process","No kinship found","At least one relative identified","Ten or more third-degree relatives identified")
sampleqc$f.22021.0.0 <- ordered(sampleqc$f.22021.0.0, levels=lvl.0682, labels=lbl.0682)
sampleqc$f.22027.0.0 <- ordered(sampleqc$f.22027.0.0, levels=lvl.0001, labels=lbl.0001)
lvl.100264 <- c(0,1)
lbl.100264 <- c("No","Yes")
sampleqc$f.22028.0.0 <- ordered(sampleqc$f.22028.0.0, levels=lvl.100264, labels=lbl.100264)
sampleqc$f.22029.0.0 <- ordered(sampleqc$f.22029.0.0, levels=lvl.100264, labels=lbl.100264)
sampleqc$f.22030.0.0 <- ordered(sampleqc$f.22030.0.0, levels=lvl.100264, labels=lbl.100264)


names(sampleqc) <- c("f.eid", "batch", "genetic_sex", "heterozygosity_noncorrected", "heterozygosity_PCAcorrected",
               "Missingness", "genetic_ethnic_grouping", "plate", "well",
               paste0("PC", 1:40), 
               "sex_aneuploidy",  "used_in_genetic_PCA",
                "genetic_kinship",  "x_probe_intensity", "y_probe_intensity", "DNA_conc", "AM_QC_metric_cluster",  
		"AM_QC_metric_dqc",  "outlier_het_missing", "use_in_phasing_autosomes", "use_in_phasing_x", "use_in_phasing_xy")