# Phenotype outcomes using combination of tables {#phenotype-outcome-combine}

In this chapter, we combine event tables generated in the previous chapters to generate

- diabetes event table and diabetes first occurrence event table
- diabetic eye disease first occurrence event table
- diabetic first occurrence event table



Load packages.

```r
library(tidyverse)
library(data.table)
source("functions.R")
```

__DM__

Load the following datasets:

- diabetes event table generated using UKB assessment center data (chapter \@ref(phenotype-outcomes-ukb))
- diabetes event table generated using PCP data (chapter \@ref(phenotype-outcomes-pcp))


```r
dm_pc <- readRDS("generated_data/dm_pc.RDS")
dm_ukb <- readRDS("generated_data/dm_ukb.RDS")
```

Create diabetes event table and diabetes first occurrence event table. The `categorize_dm_types()` function categorizes events into different diabetes types. The function also resolves any conflict in types of diabetes for each subject with more than one recorded diabetes types.

```r
dm <- full_join(dm_ukb %>% select(f.eid, event_dt, dm_type), dm_pc %>% select(f.eid, event_dt, dm_type)) %>% 
  distinct() %>% 
  group_by(f.eid) %>%
  mutate(dm_type = categorize_dm_types(dm_type)) %>%
  ungroup()

dm_firstoccur <- dm %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1) %>% ungroup()
```

Import ID's of subjects represented in the primary care data.

```r
gp_subject_ids <- readRDS("generated_data/gp_subject_ids.RDS")
```

Show a contingency table of diabetes types and availability of the primary care data for the diabetes cohort.

```r
dm_firstoccur %>% mutate(in_gp = f.eid %in% gp_subject_ids) %>% 
  select(dm_type, in_gp) %>% 
  table() %>%
  addmargins()
```

```r
saveRDS(dm,"generated_data/dm.RDS")
saveRDS(dm_firstoccur,"generated_data/dm_firstoccur.RDS")
```

__DR__

Load the following datasets:

- diabetic eye disease first occurrence event table generated using UKB assessment center data (chapter \@ref(phenotype-outcomes-ukb))
- diabetic eye disease event table generated using PCP data (chapter \@ref(phenotype-outcomes-pcp))


```r
dr_pc <- readRDS("generated_data/dr_pc.RDS")
dr_firstoccur_ukb <- readRDS("generated_data/dr_firstoccur_ukb.RDS")
```

Create diabetic eye disease first occurrence event table.

```r
dr_firstoccur <- 
  dr_pc %>% full_join(dr_firstoccur_ukb) %>% 
  group_by(f.eid) %>% arrange(event_dt) %>% slice(1) %>% ungroup() 
```


```r
saveRDS(dr_firstoccur,"generated_data/dr_firstoccur.RDS")
```

__DKD__

Load the following datasets:

- diabetic kidney disease event table generated using UKB assessment center data (chapter \@ref(phenotype-outcomes-ukb))
- diabetic kidney disease event table generated using PCP data (chapter \@ref(phenotype-outcomes-pcp))
- macroabuminuria first occurrence event table generated using the biomarker trajectory data (chapter \@ref(traj-combine))
- prolonged low eGFR first occurrence event table generated using the biomarker trajector data (chapter \@ref(traj-combine))


```r
kidney_disease_case_pc <- readRDS("generated_data/kidney_disease_case_pc.RDS")
dkd_ukb <- readRDS("generated_data/dkd_ukb.RDS")
macroabu_firstoccur <- readRDS("generated_data/macroabu_firstoccur.RDS")
prolonged_low_egfr_firstoccur <- readRDS("generated_data/prolonged_low_egfr_firstoccur.RDS")
```

Next, we create the following event tables using `low_egfr_firstoccur` and `macroabu_firstoccur` to capture additional DKD cases.

```r
prolonged_low_egfr_firstoccur <- prolonged_low_egfr_firstoccur %>% filter(!is.na(event_dt))
macroabu_firstoccur <- macroabu_firstoccur %>% filter(!is.na(event_dt)) %>% select(f.eid,event_dt)
```

Finally, we merge the following event tables to generate first occurrence diabetic kidney disease event table:

- kidney disease case multiple event table from PCP data
- diabetic kidney disease multiple event table from UKB assessment center data
- macroabuminuria first occurrence event table
- prolonged low eGFR first occurrence event table


```r
dkd_firstoccur <- 
  kidney_disease_case_pc %>% 
  full_join(dkd_ukb) %>%
  full_join(prolonged_low_egfr_firstoccur) %>%
  full_join(macroabu_firstoccur) %>%
  filter(!is.na(event_dt)) %>% 
    group_by(f.eid) %>% arrange(event_dt) %>% slice(1) %>% ungroup()
```


```r
saveRDS(dkd_firstoccur,"generated_data/dkd_firstoccur.RDS")
```
