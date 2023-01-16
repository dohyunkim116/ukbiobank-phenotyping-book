# Filter out subjects with sex mismatch {#exclude-sex-mismatch-subjects}

We exclude any subjects who have mismatch between self-reported sex and genetically determined sex from both processed demographic data and the master event table. The ID's of subjects with sex mismatch will be generated and later be used in preparing a primary care data in the next chapter.



Load packages.

```r
library(tidyverse)
library(data.table)
library(lubridate)
```

Load the reformatted UKB assessment center data containing information about subject's genetically determined sex. The object name of the dataset is `sampleqc`. The column name containing the genetically determined sex of subjects is `genetic_sex`. 

```r
sampleqc <- readRDS("generated_data/sampleQC_UKB.RDS")
```

Load curated master ukb event table and demographic table.

```r
pre_demog_sel <- readRDS("generated_data/pre_demog_sel.RDS")
pre_all_ukb_events_tab <- readRDS("generated_data/pre_all_ukb_events_tab.RDS")
```

Get subject ID's that should be excluded because of mismatch in self-reported sex and genetically determined sex. Note that everyone in demographic table has self-reported sex information. However, if a subject is missing genetically determined sex information, then we cannot acertain whether there is a mismatch. Thus, these patients are assumed to have consistent sex information.

```r
sex_mismatch_subject_ids <- pre_demog_sel %>% select(f.eid,SEX) %>% 
  full_join(sampleqc %>% select(f.eid,genetic_sex)) %>% 
  filter(SEX != genetic_sex) %>% .$f.eid 
```

Filter the demographic table.

```r
demog_sel <- pre_demog_sel %>% filter(!(f.eid %in% sex_mismatch_subject_ids))
```

Filter the master event table.

```r
all_ukb_events_tab <- pre_all_ukb_events_tab %>% filter(!(f.eid %in% sex_mismatch_subject_ids))
```

Save sex mismatch subject ID's and filtered demographic table and the master event table.

```r
saveRDS(sex_mismatch_subject_ids,"generated_data/sex_mismatch_subject_ids.RDS")
saveRDS(demog_sel,"generated_data/demog_selected.RDS")
saveRDS(all_ukb_events_tab,"generated_data/all_ukb_events_tab.RDS")
```











