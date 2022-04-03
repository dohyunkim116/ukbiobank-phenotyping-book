# Phenotype outcome events using UKB assessment center data {#phenotype-outcomes-ukb}

Here, we use UKB all events table created in \@ref(curate-all-ukb_events-table) to generate first occurrence and multiple event tables. We phenotype the following outcomes, which will be used to create time-to-event tables:

  - Myocardial Infarction (MI)
  - Unstable Angina
  - Ischemic Stroke
  - Hemorrhagic Stroke
  - Stroke
  - PCI
  - Composite CVD
  
The following outcomes and their event tables will be used to apply different control exclusion criteria to different time-to-event tables:

  - Diabetic eye disease control exclusion events
  - Diabetic kidney disease control exclusion events
  - Cardiovascular control exclusion events
  - Cerebrovascular control exclusion events
  - Non-coronary revascularization control exclusion events

The general phenotyping procedure for complication outcomes and control exclusion outcomes are the same. All we have to do is define patterns we want to search and match these patterms from all events table.

The searching step is abstracted away in a function named `get_phenotype_tab` which resides in `functions.R`. The function takes in defined patterns in regex and search the patterns from the UKB all events table. The function outputs event table containing subjects with events matching input patterns and pattern types. These are the types of patterns the function accepts:

- UKB-defined field patterns and custom-defined field patterns
- ICD10 code patterns
- OPCS4 code patterns
- Self-reported condition code patterns
- Self-reported operation code patterns

The UKB-defined field patterns match fields that are associated with first occurrence fields and algorithimically defined fields as defined by UKB study. The custom field pattern matches a custom field that one has defined when creating all UKB events table. Note that the only custom field that exists now is `dr_self` field used in phenotyping diabetes related eye disease. The code patterns including ICD10, OPCS4, self-reported condition and self-reported operation match the codes that represent some clinical event. Thus, to phenotype an outcome, one should first identify the fields and codes associated with an outcome, define the patterns, and pass them to function along with the master event table. Internally, `get_phenotype_tab` function uses `grepl` function in `dplyr` package to filter the master event table.

Input and output files:

- Input:

  - `event_tab.RDS`

- Output:

  - `mi_ukb.RDS`
  - `mi_firstoccur_ukb.RDS`
  - `unstable_angina_ukb.RDS`
  - `unstable_angina_firstoccur_ukb.RDS`
  - `stroke_infarct_ukb.RDS`
  - `stroke_infarct_firstoccur_ukb.RDS`
  - `stroke_hem_ukb.RDS`
  - `stroke_hem_firstoccur_ukb.RDS`
  - `stroke_ukb.RDS`
  - `stroke_firstoccur_ukb.RDS`
  - `pci_ukb.RDS`
  - `pci_firstoccur_ukb.RDS`
  - `cvd_ukb.RDS`
  - `cvd_firstoccur_ukb.RDS`
  - `dr_firstoccur_ukb.RDS`
  - `dkd_ukb.RDS`
  - `dkd_firstoccur_ukb.RDS`
  - `dr_control_exclusion_events_ukb.RDS`
  - `dkd_control_exclusion_events_ukb.RDS`
  - `cardio_control_exclusion_events_ukb.RDS`
  - `cerebro_control_exclusion_events_ukb.RDS`
  - `other_revas_control_exclusion_events_ukb.RDS`




```r
library(tidyverse)
library(data.table)
source("functions.R")
```

## Phenotype outcome events using UKB assessment center data

Import the master event table obtained from UKB assessment datasets

```r
event_tab <- readRDS("generated_data/all_ukb_events_tab.RDS")
```

We phenotype the following outcome events using the UKB assessment center data in this R markdown:

- Diabetes (DM)
- Myocardial infarction (MI)
- Unstable angina
- Ischemic stroke
- Hemorrhagic stroke
- Stroke
- Percutaneous Coronary Intervention (PCI)
- Composite CVD
- Diabetic eye disease (DR)
- Diabetic Kidney Disease (CKD)

### DM

Define patterns to search

```r
dm_field_patterns <- "130706|130708|130714"
```

Get recurrent DM event table and first occurrence DM event table

```r
dm_ukb <- get_phenotype_tab(field_patterns = dm_field_patterns,event_tab = event_tab,firstoccur = F)
dm_firstoccur_ukb <- get_phenotype_tab(field_patterns = dm_field_patterns,event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(dm_ukb,"generated_data/dm_ukb.RDS")
saveRDS(dm_firstoccur_ukb,"generated_data/dm_firstoccur_ukb.RDS")
```

### MI

Outcome fields:

- f.131298.0.0: the date of first occurrence of MI
- f.131300.0.0: the date of subsequent occurrence of MI (?)
- f.42000.0.0: the date of algorithmically defined MI outcome

ICD10 code prefixes: 

- I21
- I22
- I23

Define patterns to search

```r
mi_field_patterns <- "131298|131300|42000"
mi_icd10_patterns <- "^I21|^I22|^I23"
```

Get recurrent MI event table and first occurrence MI event table

```r
mi_ukb <- 
  get_phenotype_tab(field_patterns = mi_field_patterns, icd10_patterns_any = mi_icd10_patterns,event_tab = event_tab,firstoccur = F)
mi_firstoccur_ukb <- 
  get_phenotype_tab(field_patterns = mi_field_patterns, icd10_patterns_any = mi_icd10_patterns,event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(mi_ukb,"generated_data/mi_ukb.RDS")
saveRDS(mi_firstoccur_ukb,"generated_data/mi_firstoccur_ukb.RDS")
```

### Unstable Angina

ICD10 code prefixes: 

- I200


```r
unstable_angina_icd10_patterns <- "I200"
```

Get recurrent and first occurrence unstable angina event tables

```r
unstable_angina_ukb <- get_phenotype_tab(icd10_patterns_any = unstable_angina_icd10_patterns, event_tab = event_tab,firstoccur = F)
unstable_angina_firstoccur_ukb <- get_phenotype_tab(icd10_patterns_any = unstable_angina_icd10_patterns, event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(unstable_angina_ukb,"generated_data/unstable_angina_ukb.RDS")
saveRDS(unstable_angina_firstoccur_ukb,"generated_data/unstable_angina_firstoccur_ukb.RDS")
```

### Ischemic Stroke

Outcome fields:
- 131366: Date I63 first reported (cerebral infarction)
- 42008: algorithmically defined outcome for ischemic stroke 

ICD10 code prefixes: 
- I63

Self-reported condition codes:
- 1583


```r
stroke_infarct_field_patterns <- "131366|42008"
stroke_infarct_icd10_patterns <- "^I63"
stroke_infarct_selfrep_patterns <- "^1583$"
```

Get recurrent and first occurrence ischemic stroke event tables

```r
stroke_infacrt_ukb <- get_phenotype_tab(field_patterns = stroke_infarct_field_patterns,
                                        icd10_patterns_any = stroke_infarct_icd10_patterns,
                                        selfrep_patterns = stroke_infarct_selfrep_patterns,
                                        event_tab = event_tab,firstoccur = F)

stroke_infacrt_firstoccur_ukb <- get_phenotype_tab(field_patterns = stroke_infarct_field_patterns,
                                        icd10_patterns_any = stroke_infarct_icd10_patterns,
                                        selfrep_patterns = stroke_infarct_selfrep_patterns,
                                        event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(stroke_infacrt_ukb,"generated_data/stroke_infarct_ukb.RDS")
saveRDS(stroke_infacrt_firstoccur_ukb,"generated_data/stroke_infarct_firstoccur_ukb.RDS")
```


### Hemorrhagic Stroke

Outcome fields:

- 131360: Subarachnoid Hemorrhage
- 131362: Intracerebral Hemorrhage
- 131364: Other non-traumatic Hemorrhage, 
- 42010: Intracerebral Hemorrhage
- 42012: Subarachnoid Hemorrhage

ICD10 code prefixes: 
- I60
- I61
- I62

Self-reported condition codes:
- 1086


```r
stroke_hem_field_patterns <- "131360|131362|131364|42010|42012"
stroke_hem_icd10_patterns <- "^I60|^I61|^I62"
stroke_hem_selfrep_patterns <- "^1086$"
```

Get recurrent and first occurrence hemorrhagic stroke event tables

```r
stroke_hem_ukb <- get_phenotype_tab(field_patterns = stroke_hem_field_patterns,
                                            icd10_patterns_any = stroke_hem_icd10_patterns,
                                            selfrep_patterns = stroke_hem_selfrep_patterns,
                                            event_tab = event_tab,firstoccur = F)

stroke_hem_firstoccur_ukb <- get_phenotype_tab(field_patterns = stroke_hem_field_patterns,
                                            icd10_patterns_any = stroke_hem_icd10_patterns,
                                            selfrep_patterns = stroke_hem_selfrep_patterns,
                                            event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(stroke_hem_ukb,"generated_data/stroke_hem_ukb.RDS")
saveRDS(stroke_hem_firstoccur_ukb,"generated_data/stroke_hem_firstoccur_ukb.RDS")
```


### Stroke

Outcome fields:
- 42006: unspecified stroke
- 131368: unspecified stroke

ICD10 code prefix
- I64

Self-reported condition codes:
- 1081

Outcome tables:
- recurrent ischemic stroke event table
- recurrent hemorrhagic stroke event table


```r
stroke_unspec_field_patterns <- "42006|131368"
stroke_unspec_icd10_patterns <- "^I64"
stroke_other_selfrep_patterns <- "^1081$"
```

Get recurrent stroke event table

```r
stroke_infarct_hem_ukb <- 
  full_join(stroke_infacrt_ukb,stroke_hem_ukb) %>% arrange(f.eid,event_dt)

stroke_unspec_ukb <- get_phenotype_tab(field_patterns = stroke_unspec_field_patterns,
                                   icd10_patterns_any = stroke_unspec_icd10_patterns,
                                   event_tab = event_tab,firstoccur = F)

stroke_other_ukb <- get_phenotype_tab(selfrep_patterns = stroke_other_selfrep_patterns,
                                   event_tab = event_tab,firstoccur = F)

stroke_ukb <- 
  stroke_infarct_hem_ukb %>% 
  full_join(stroke_unspec_ukb, by = c("f.eid","event_dt")) %>%
  full_join(stroke_other_ukb, by = c("f.eid","event_dt")) %>% 
  arrange(f.eid,event_dt)
```

Get first occurrence stroke event table

```r
stroke_firstoccur_ukb <- stroke_ukb %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1)
```


```r
saveRDS(stroke_ukb,"generated_data/stroke_ukb.RDS")
saveRDS(stroke_firstoccur_ukb,"generated_data/stroke_firstoccur_ukb.RDS")
```


### PCI

OPCS4 codes:
- K40, K41, K42, K43, K44, K45, K46, K483, K49, K501, K75, K76

Self-reported operation codes:
- 1070 (Coronary Angioplasty)
- 1095 (Coronary bypass grafts)


```r
# OPCS patterns start with 'K' so do not have to start the pattern with '^'
pci_opcs_patterns <- "K40|K41|K42|K43|K44|K45|K46|K483|K49|K501|K75|K76"
pci_selfrep_op_patterns <- "^1070$|^1095$"
```

Get recurrent and first occurrence pci event tables

```r
pci_ukb <- get_phenotype_tab(opcs_patterns = pci_opcs_patterns,
                                     selfrep_op_patterns = pci_selfrep_op_patterns,
                                     event_tab = event_tab,firstoccur = F)

pci_firstoccur_ukb <- get_phenotype_tab(opcs_patterns = pci_opcs_patterns,
                                     selfrep_op_patterns = pci_selfrep_op_patterns,
                                     event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(pci_ukb,"generated_data/pci_ukb.RDS")
saveRDS(pci_firstoccur_ukb,"generated_data/pci_firstoccur_ukb.RDS")
```

### CVD death

ICD10 code prefixes (primary death):

-I*


```r
cvd_death_icd10_death_primary_patterns <- "^I"
```


```r
cvd_death_ukb <- get_phenotype_tab(icd10_patterns_pd = cvd_death_icd10_death_primary_patterns,
                                     event_tab = event_tab,firstoccur = F)
cvd_death_firstoccur_ukb <- get_phenotype_tab(icd10_patterns_pd = cvd_death_icd10_death_primary_patterns,
                                     event_tab = event_tab,firstoccur = T)
```

### Composite CVD

Composite CVD was defined to be any of the following events:

- MI
- Ischemic stroke
- unstable angina
- PCI
- CVD death

Thus, phenotyping composite CVD requires recurrent event tables of MI, Ischemic stroke, unstable angina, PCI and CVD death

Get recurrent and first occurrence composite CVD event tables

```r
cvd_ukb <- 
  mi_ukb %>%
  full_join(stroke_infacrt_ukb) %>%
  full_join(unstable_angina_ukb) %>%
  full_join(pci_ukb) %>% 
  full_join(cvd_death_ukb) %>% distinct() %>% arrange(f.eid,event_dt)

cvd_firstoccur_ukb <- cvd_ukb %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1)
```


```r
saveRDS(cvd_ukb,"generated_data/cvd_ukb.RDS")
saveRDS(cvd_firstoccur_ukb,"generated_data/cvd_firstoccur_ukb.RDS")
```


### DR

ICD10 codes:

- E1*.3: Diabetes Mellitus with Ophthalmic Complications
- H36.0: Diabetic Retinopathy
- H28.0: Diabetic Cataract

ICD9 codes:

- 2504: Diabetes with Ophthalmic manifestations
- 3620: Diabetic retinopathy

Self-reported condition codes:

- 1276

Custom defined fields:

- dr_self: the date of DR diagnosis (wrangling of f.5901.0.0, f.5901.1.0, f.5901.2.0, f.5901.3.0 where they record age at which DR was diagnosed at four different time points)

Define patterns

```r
dr_icd10 <- "E103|E113|E133|E143|H360|H280"
dr_icd9 <- "^2504|^3620"
dr_self <- "1276"
dr_custom <- "dr_self"
```

Get first occurrence composite CVD event tables. Note that we are not generating multiple events table for DR.

```r
dr_firstoccur_ukb <- get_phenotype_tab(icd10_patterns_any = dr_icd10,
                                       icd9_patterns_any = dr_icd9,
                                       selfrep_patterns = dr_self,
                                       custom_field_patterns = dr_custom,
                                    event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(dr_firstoccur_ukb,"generated_data/dr_firstoccur_ukb.RDS")
```

### CKD

Outcome fields:

- 42026: algorithmically defined End-Stage Renal Disease (ESRD)

ICD10 codes:

- E1.2: Diabetes Mellitus with renal complication
- N18[0345]: CKD Stage 3-5, end stage
- N083: Glomerular disorders in Diabetes Mellitus

ICD9 codes:
- 2503: Diabetes with renal manifestations
- 5859: Renal failure

Self-reported condition codes:

- 1607


```r
dkd_field_patterns <- "42026"
dkd_icd10 <- "E102|E112|E132|E142|N180|N183|N184|N185|N083"
dkd_icd9 <- "^2503|^5859"
dkd_self <- "1607"
```

Get recurrent and first occurrence composite diabetic kidney disease event tables

```r
dkd_ukb <- get_phenotype_tab(field_patterns = dkd_field_patterns,
                             icd10_patterns_any = dkd_icd10,
                             icd9_patterns_any = dkd_icd9,
                             selfrep_patterns = dkd_self,
                  event_tab = event_tab,firstoccur = F)

dkd_firstoccur_ukb <- get_phenotype_tab(field_patterns = dkd_field_patterns,
                                        icd10_patterns_any = dkd_icd10,
                                        icd9_patterns_any = dkd_icd9,
                                        selfrep_patterns = dkd_self,
                  event_tab = event_tab,firstoccur = T)
```


```r
saveRDS(dkd_ukb,"generated_data/dkd_ukb.RDS")
saveRDS(dkd_firstoccur_ukb,"generated_data/dkd_firstoccur_ukb.RDS")
```

## Phenotype control exclusion events using UKB assessment center data

### Cardiovascular control exclusion events

Define patterns

```r
exclude_ctrl_cardio_icd10 <- "I2|I3|I5|I6|I7"
exclude_ctrl_cardio_self <- "1074|1076|1077|1078|1079|1080|1471|1489|1490|1492|1584|1585|1586|1587|1588|1589|1590|1591|1592"
exclude_ctrl_cardio_selfop <- "1069|1096|1097|1098|1099|1100|1101|1104|1523|1553|1554"
```


```r
cardio_control_exclusion_events <- 
  get_phenotype_tab(icd10_patterns_any = exclude_ctrl_cardio_icd10,
                  selfrep_patterns = exclude_ctrl_cardio_self,
                  selfrep_op_patterns = exclude_ctrl_cardio_selfop,event_tab = event_tab,firstoccur = F)
```

How many unique subjects with cardio control exclusion events?

```r
cardio_control_exclusion_events %>% .$f.eid %>% unique %>% length()
```

```
## [1] 105642
```


```r
saveRDS(cardio_control_exclusion_events,"generated_data/cardio_control_exclusion_events_ukb.RDS")
```

### Cerebrovascular control exclusion events

Define patterns

```r
exclude_ctrl_cereb_icd10 <- "G45|G46|I65|I66|I67|I68|I69"
exclude_ctrl_cereb_self <- "1082"
```



```r
cerebro_control_exclusion_events <- 
  get_phenotype_tab(icd10_patterns_any = exclude_ctrl_cereb_icd10,
                    selfrep_patterns = exclude_ctrl_cereb_self,event_tab = event_tab,firstoccur = F)
```

How many unique subjects with cerebro control exclusion events?

```r
cerebro_control_exclusion_events %>% .$f.eid %>% unique %>% length()
```

```
## [1] 18092
```


```r
saveRDS(cerebro_control_exclusion_events,"generated_data/cerebro_control_exclusion_events_ukb.RDS")
```

### Non-coronary revascularization procedure control exclusion events

```r
other_revasc_selfop <- "1071|1102|1105|1107|1108|1109|1110"
```


```r
other_revas_control_exclusion_events <-
  get_phenotype_tab(selfrep_op_patterns = other_revasc_selfop, event_tab = event_tab, firstoccur = F)
```

How many unique subjects with non-cornoary revascularization procedure control exclusion events?

```r
other_revas_control_exclusion_events %>% .$f.eid %>% unique %>% length()
```

```
## [1] 2294
```


```r
saveRDS(other_revas_control_exclusion_events,"generated_data/other_revas_control_exclusion_events_ukb.RDS")
```


### Renal disease control exclusion events

Define patterns

```r
exclude_ctrl_renal_icd10 <- "N0|N1|N2|Z49|Z992"
exclude_ctrl_renal_icd9 <- "^58[0-9]|^59[1-4]|^590[23]|^V420|^V454|^V560|^V568"
exclude_ctrl_renal_self <- "1192|1193|1194|1519|1520|1608|1609"
exclude_ctrl_renal_selfop <- "1195|1487"
exclude_ctrl_renal_opcs <- "M01|M02|M03|X40|X41|X42"
```


```r
dkd_control_exclusion_events <-
  get_phenotype_tab(icd10_patterns_any = exclude_ctrl_renal_icd10,
                    icd9_patterns_any = exclude_ctrl_renal_icd9,
                    selfrep_patterns = exclude_ctrl_renal_self,
                    selfrep_op_patterns = exclude_ctrl_renal_selfop,
                    opcs_patterns = exclude_ctrl_renal_opcs,event_tab = event_tab,firstoccur = F)
```

How many unique subjects with renal control exclusion events?

```r
dkd_control_exclusion_events %>% .$f.eid %>% unique %>% length()
```

```
## [1] 53782
```


```r
saveRDS(dkd_control_exclusion_events,"generated_data/dkd_control_exclusion_events_ukb.RDS")
```

### Eye disease control exclusion events

Define patterns

```r
exclude_eye_icd10 <- "H25|H26|H28|H34|H35|H36|H40|H42"
exclude_eye_icd9 <- "^36[256]"
exclude_eye_self <- "1275|1277|1278|1281|1282|1527|1538|1530"
exclude_eye_selfop <- "1434|1435|1436|1437"
```


```r
dr_control_exclusion_events <- 
  get_phenotype_tab(icd10_patterns_any = exclude_eye_icd10,
                    icd9_patterns_any = exclude_eye_icd9,
                    selfrep_patterns = exclude_eye_self,
                    selfrep_op_patterns = exclude_eye_selfop,event_tab = event_tab,firstoccur = F)
```

How many unique subjects with cardio control exclusion events?

```r
dr_control_exclusion_events %>% .$f.eid %>% unique %>% length()
```

```
## [1] 83259
```


```r
saveRDS(dr_control_exclusion_events,"generated_data/dr_control_exclusion_events_ukb.RDS")
```






























