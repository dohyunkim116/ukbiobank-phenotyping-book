# Filter out subjects with sex mismatch {#exclude-sex-mismatch-subjects}

We exclude any subjects who have mismatch between self-reported sex and genetically determined sex from both processed demographic data and the master event table. The IDs of subjects with sex mismatch will be generated and later be used in preparing a primary care data in `prep_pcp_data`.

* Generated files: 
  - `sex_mismatch_subject_ids.RDS`
  - `demog_selected.RDS`
  - `all_ukb_events_tab.RDS`




```r
library(tidyverse)
library(data.table)
library(lubridate)
```

We remove any subjects from the final demographics table as well as final master event table.\par

Load the reformatted UKB assessment center data containing information about subject's sex determined by genetics. The object name of the dataset is `sampleqc`. The column name containing the genetically determined sex of subjects is `genetic_sex`. 

```r
sampleqc <- readRDS("generated_data/sampleQC_UKB.RDS")
```

Load curated all ukb events table and demographic table

```r
pre_demog_sel <- readRDS("generated_data/pre_demog_sel.RDS")
pre_all_ukb_events_tab <- readRDS("generated_data/pre_all_ukb_events_tab.RDS")
```

Get subject IDs that should be excluded because of mismatch in self-reported sex and genetically determined sex. Note that everyone in demographics table has self-reported sex information.

```r
# this filtering step does not filter out 
# any subject with missing genetically determined sex information
sex_mismatch_subject_ids <- pre_demog_sel %>% select(f.eid,SEX) %>% 
  full_join(sampleqc %>% select(f.eid,genetic_sex)) %>% 
  filter(SEX != genetic_sex) %>% .$f.eid 
```

Filter demographic table

```r
demog_sel <- pre_demog_sel %>% filter(!(f.eid %in% sex_mismatch_subject_ids))
```

Filter master event table

```r
all_ukb_events_tab <- pre_all_ukb_events_tab %>% filter(!(f.eid %in% sex_mismatch_subject_ids))
```


```r
saveRDS(sex_mismatch_subject_ids,"generated_data/sex_mismatch_subject_ids.RDS")
saveRDS(demog_sel,"generated_data/demog_selected.RDS")
saveRDS(all_ukb_events_tab,"generated_data/all_ukb_events_tab.RDS")
```










