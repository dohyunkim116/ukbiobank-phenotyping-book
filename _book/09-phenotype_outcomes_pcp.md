# Phenotype outcomes using primary care data{#phenotype-outcomes-pcp}

We acheive the following in this folder:

- Generate __diabetes first occurrence__ and multiple event table (`phenotype_outcome_events_pcp.Rmd`)
- Generate __advanced chronic disease/diabetic kidney disease__ first occurrence and multiple event tables (`phenotype_outcome_events_pcp.Rmd`)
- Generate __diabetic eye disease__ first occurrence and multiple event tables (`phenotype_outcome_events_pcp.Rmd`)
- Generate __kidney disease control exclusion__ event table (`phenotype_control_exclusion_events_pcp.Rmd`)
- Generate __non-diabetic eye disease__ event table that will be used to exclude controls from generated time-to-event data later (`phenotype_control_exclusion_events_pcp.Rmd`)

We utilize outcome-specific dictionaries generated in `make_outcome_specific_code_dictionary` to subset a primary care data and to generate different event tables.

Input and output files:

- Input:

  - `entire_gp_clinical_30March2021_formatted.txt`
  - `dm_dict.RDS`
  - `dm_eye_disease_dict.RDS`
  - `kidney_disease_case_dict.RDS`
  - `nondm_eye_disease_dict.RDS`
  - `kidney_disease_control_exclusion_dict.RDS`
  
- Output:

  - `dm_pc.RDS`
  - `dm_firstoccur_pc.RDS`
  - `dr_pc.RDS`
  - `dr_firstoccur_pc.RDS`
  - `kidney_disease_case_pc.RDS`
  - `kidney_disease_case_firstoccur_pc.RDS`
  - `kidney_disease_control_exclusion_pc.RDS`
  - `nondm_eye_disease_pc.RDS`

## Phenotype outcome events using primary care data
  



```r
library(tidyverse)
library(data.table)
source("reference.R")
```

Import primary care data

```r
pc <- fread("generated_data/entire_gp_clinical_30March2021_formatted.txt")
```

Import the following outcome-specific dictionaries:

- diabetes code dictionary
- diabetic eye disease code dictionary
- kidney disease code dictionary


```r
dm_dict <- readRDS("generated_data/dm_dict.RDS")
dr_dict <- readRDS("generated_data/dm_eye_disease_dict.RDS")
kidney_disease_case_dict <- readRDS("generated_data/kidney_disease_case_dict.RDS")
```

We phenotype the following outcome events in this R markdown:

- Diabetes (DM)
- Diabetic eye disease (DR)
- Kidney disease

### DM

We pick out the individuals with terms that we are confident that they had diabetes. These terms were selected with the help of a physician, and defined in `reference.R` as an object, `dm_descriptions`. If a subject had terms predating the certain diagnosis which are less certain, we assume that the earlier diagnosis was the better start date. 

For example, suppose we have a subject with following diabetes events:

- 2012: "Diabetic dietary review"
- 2013: "Type II diabetes"

This individual is considered diabetic (type 2) and the first date is 2012.  

Whereas, the following subject is not included as diabetes patient because the codes are not strong enough to indicate diabetes:

- 2012: "Diabetic dietary review"
- 2013: "Diabetic dietary review"

Filter PCP data for any DM indication

```r
diabetics_any <- pc %>%
  select(f.eid, code, event_dt) %>%
  filter(code %in% unique(dm_dict$code)) %>% distinct()
```

Get DM codes with clear DM indication

```r
dm_codes_confident <-  dm_dict %>% filter(term_description %in% dm_descriptions) %>% .$code %>% unique
```

Filter PCP data with any DM indication for clear DM indication. Note that `diabetics_simple` is the recurrent DM event table.

```r
diabetics_simple <- 
  diabetics_any %>% 
  group_by(f.eid) %>% 
  filter(any(code %in% dm_codes_confident)) %>%
  mutate(event_dt = as.Date(event_dt)) %>%
  select(f.eid,event_dt) %>% distinct() %>% 
  arrange(f.eid,event_dt)
```

Select first occurrence diabetes event

```r
dm_firstoccur_pc <- diabetics_simple %>%
  group_by(f.eid) %>% arrange(event_dt) %>% slice(1)
```


```r
saveRDS(diabetics_simple,"generated_data/dm_pc.RDS")
saveRDS(dm_firstoccur_pc,"generated_data/dm_firstoccur_pc.RDS")
```

### DR

Phenotype recurrent diabetic eye disease event table

```r
dr_pc <- pc %>% filter(code %in% dr_dict$code) %>% 
  select(f.eid,event_dt) %>% 
  mutate(event_dt=as.Date(event_dt)) %>% 
  distinct() %>% arrange(f.eid,event_dt)
```

Select first occurrence 

```r
dr_firstoccur_pc <- dr_pc %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1)
```


```r
saveRDS(dr_pc,"generated_data/dr_pc.RDS")
saveRDS(dr_firstoccur_pc,"generated_data/dr_firstoccur_pc.RDS")
```

### CKD

Phenotype recurrent (**chronic?**) kidney disease event tables

```r
kidney_disease_case_pc <- 
  pc %>% filter(code %in% kidney_disease_case_dict$code) %>% 
  select(f.eid,event_dt) %>%
  mutate(event_dt = as.Date(event_dt)) %>%
  distinct() %>% arrange(f.eid,event_dt)
```

Select first occurrence 

```r
kidney_disease_case_firstoccur_pc <- kidney_disease_case_pc %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1)
```


```r
saveRDS(kidney_disease_case_pc,"generated_data/kidney_disease_case_pc.RDS")
saveRDS(kidney_disease_case_firstoccur_pc,"generated_data/kidney_disease_case_firstoccur_pc.RDS")
```

## Phenotype control exclusion events using primary care data

Import the following outcome-specific dictionaries:

- Non-diabetic eye disease dictionary
- kidney disease control exclusion event code dictionary


```r
nondm_eye_disease_dict <- readRDS("generated_data/nondm_eye_disease_dict.RDS")
kidney_disease_control_exclusion_dict <- readRDS("generated_data/kidney_disease_control_exclusion_dict.RDS")
```

We phenotype the following outcome events that will be used as control exclusion events in this R markdown:

- Non-diabetic eye disease events
- Kidney disease control exclusion events

### Non-diabetic eye disease events

Phenotype non-DM eye disease to be used as control exclusion events

```r
nondm_eye_disease_pc <- 
  pc %>% filter(code %in% nondm_eye_disease_dict$code) %>% select(f.eid,event_dt) %>% mutate(event_dt=as.Date(event_dt))
```



### Kidney disease control exclusion events

Phenotype control exclusion kidney disease events

```r
kidney_disease_control_exclusion_pc <- 
  pc %>% filter(code %in% kidney_disease_control_exclusion_dict$code) %>%
  mutate(event_dt = as.Date(event_dt))
```






















