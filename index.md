--- 
title: "UK Biobank Phenotyping"
author: "Do Hyun Kim"
date: "2023-01-15"
site: "bookdown::bookdown_site"
documentclass: book
bibliography: references.bib
fontsize: 12pt
link-citations: yes
---

# Generating time-to-event data using UK biobank data {-}

This book presents steps for phenotyping survival outcomes using data from UK Biobank (UKB) study [@sudlow-ukb]. The following are the outcomes we phenotyped:

- Diabetes
- Myocardial Infarction
- Unstable Angina
- Ischemic Stroke
- Hemorrhagic Stroke
- Stroke
- Percutaneous Coronary Intervention (PCI)
- Composite cardiovascular disease (CVD)
- Diabetic Eye Disease or Diabetic Retinopathy (DR)
- Chronic/Diabetic Kidney Disease (CKD/DKD)

We will refer UKB assessment center data as "UKB data" and the associated primary care data as "UKB primary care data." These two sources of data will be prepared separately and combined to create outcome event tables which will be used to phenotype survival outcomes.


