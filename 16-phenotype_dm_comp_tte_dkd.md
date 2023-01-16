# Generate time-to-event data for diabetes related kidney disease {#tte-dkd}

In this chapter, we phenotype right-censored time-to-event data for diabetic kidney disease. An initial time-to-event data are created, and consequent cases and controls are identified following the identical procedure described in chapter \@ref(tte-macro). The initial time-to-event data are refined by applying the following exclusion criteria: 

  - case
    - has a complication event after the censoring date
    - has a prior complication event identified before a first evidence of diabetes
    - has an initiation date that is not between the first evidence of diabetes and the date of complication event
    - has less than 5 years of follow-up time since the first evidence of diabetes before a kidney disease event
  
  - control
     - has some event represented in diabetic kidney disease control exclusion event table (generated in chapter \@ref(phenotype-outcomes-ukb))
    - has some event represented in kidney disease control exclusion event table (generated in chapter \@ref(phenotype-outcomes-pcp))
    - has been found to have microalbuminuria at least once
    - does not have eGFR measurements available
    - has been found to have eGFR level less than 60
    - has a first evidence of diabetes after the censoring date
    - has less than 5 years of follow-up time since the first evidence of diabetes before censoring date
    - has a first evidence of diabetes 6 months after the study initiation date
    - is not represented in the primary care data
    
We identify subjects who have had microalbuminuria at least once using miicroalbuminuria first occurrence event table generated in chapter \@ref(traj-combine). Subjects with eGFR level less than 60 are identified using eGFR trajectory data created in chapter \@ref(traj-combine).



Load pacakges and functions.

```r
library(tidyverse)
library(data.table)
source("functions.R")
```

Load the following datasets:

- diabetes first occurrence data
- selected demographic data
- first occurrence DKD event table
- eGFR trajectory data
- microalbuminuria first occurrence event table


```r
dm_firstoccur <- readRDS("generated_data/dm_firstoccur.RDS")
demog <- readRDS("generated_data/demog_selected.RDS")
dkd_firstoccur <- readRDS("generated_data/dkd_firstoccur.RDS")
egfr_traj <- readRDS("generated_data/trajectory_egfr.RDS")
microabu_firstoccur <- readRDS("generated_data/microabu_firstoccur.RDS")
```

Load the following control exclusion event tables:

- DKD control exclusion event table from UKB assessment center data
- DKD control exclusion event table from PCP data


```r
dkd_control_exclusion_events_ukb <- readRDS("generated_data/dkd_control_exclusion_events_ukb.RDS")
kidney_disease_control_exclusion_events_pc <- readRDS("generated_data/kidney_disease_control_exclusion_pc.RDS")
```

Create additional control exclusion event tables:

- a table containing subjects who have been recorded to have ever had eGFR level less than 60 
- a table containing subjects who have been recorded to have ever had microalbuminuria


```r
egfr_lt60_ever_event_tab <- egfr_traj %>% filter(measurement < 60, !is.na(measurement), !is.na(event_dt))
microabu_ever_event_tab <- microabu_firstoccur %>% filter(!is.na(event))
```

Merge control exclusion event tables.

```r
ckd_control_exclusion_events <- bind_rows(list(dkd_control_exclusion_events_ukb,
            kidney_disease_control_exclusion_events_pc,
            egfr_lt60_ever_event_tab,microabu_ever_event_tab))
```

Define control exclusion subject ID's.

```r
ctrl_exclusion_ids <- ckd_control_exclusion_events$f.eid %>% unique
```

The controls should have linked primary care dataset and at least one eGFR measurement.

```r
pcp_subejct_ids <- readRDS("generated_data/gp_subject_ids.RDS")
eGFR_avail_subject_ids <- (egfr_traj %>% filter(!is.na(measurement) & !is.na(event_dt))) %>% .$f.eid
```

Define control inclusion subject ID's.

```r
ctrl_inclusion_ids <- intersect(pcp_subejct_ids,eGFR_avail_subject_ids) %>% unique
```


```r
dkd_tte <- phenotype_time_to_event(dm_firstoccur,dkd_firstoccur,demog,
                                  control_exclusion_ids = ctrl_exclusion_ids,
                                  control_inclusion_ids = ctrl_inclusion_ids)
```

We filter out cases that do not have at least 5 years of follow-up time.

```r
dkd_tte <- 
  dkd_tte %>%
  filter(event == 0 | (event == 1 & (lubridate::decimal_date(event_dt_comp) - lubridate::decimal_date(event_dt_dm) >= 5)))
```


```r
saveRDS(dkd_tte,"generated_data/dkd_tte.RDS")
```


