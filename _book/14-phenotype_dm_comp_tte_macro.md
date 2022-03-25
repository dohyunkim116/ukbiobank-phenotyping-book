# Generate time-to-event data for macrovascular diabetes complications {#tte-macro}

Diabetes first occurrence event table (`dm_firstoccur.RDS`) was generated in `phenotype_outcome_events_combine` and the demographic data (`demog_selected.RDS`) was generated in `prep_ukb_assessment`.

Here are the mascrovascular diabetic complications and their time-to-event datasets we are phenotyping:

- Myocardial Infarction (MI)
- Unstable Angina
- Ischemic Stroke
- Hemorrhagic Stroke
- Stroke
- PCI
- Composite CVD

The following table shows macrovascular complication event table files and where they were generated:

|Macrovascular complication outcome|file|folder|
|------------|---|---|
| Myocardial Infarction (MI) | `mi_firstoccur_ukb.RDS`| `phenotype_events_ukb` |
| Unstable Angina | `unstable_angina_ukb.RDS` | `phenotype_events_ukb` |
| Ischemic Stroke | `stroke_infarct_firstoccur_ukb.RDS` | `phenotype_events_ukb` |
| Hemorrhagic Stroke | `stroke_hem_firstoccur_ukb.RDS` | `phenotype_events_ukb` |
| Stroke | `stroke_ukb.RDS` | `phenotype_events_ukb` |
| PCI | `pci_firstoccur_ukb.RDS` | `phenotype_events_ukb` |
| Composite CVD | `cvd_firstoccur_ukb.RDS` | `phenotype_events_ukb` |

The following table shows macrovascular complications and associated control exclusion criteria:

|Macrovascular complication|Control exclusion outcome|
|------------|---|
|Myocardial Infarction|cardiovascular control exclusion events|
|Unstable Angina||
|Ischemic Stroke|cerebrovascular control exclusion events|
|Hemorrhagic Stroke|cerebrovascular control exclusion events|
|Stroke|cerebrovascular control exclusion events|
|PCI|non-coronary revascularization procedure control exclusion events|
|Composite CVD| control exclusion events associated with MI, Ischmic stroke, unstable angina and PCI|

The following table shows control exclusion event table files and where they were generated:

|Control exclusion outcome|file|folder|
|------------|---|---|
|cardiovascular control exclusion events|`cardio_control_exclusion_events_ukb.RDS`|`phenotype_events_ukb`|
|cerebrovascular control exclusion events|`cerebro_control_exclusion_events_ukb.RDS`|`phenotype_events_ukb`|
|non-coronary revascularization procedure control exclusion events|`other_revas_control_exclusion_events_ukb.RDS`|`phenotype_events_ukb`|

Applying these exclusion criteria to initial time-to-event data gives us the final time-to-event data for macrovascular diabetic complications. 




```r
library(tidyverse)
library(data.table)
source("functions.R")
```

Load phenotyped DM data

```r
dm_firstoccur <- readRDS("generated_data/dm_firstoccur.RDS")
```

Load demographic data

```r
demog <- readRDS("generated_data/demog_selected.RDS")
```

## MI

Load MI first occurrence data phenotyped from UKB assessment data

```r
mi_firstoccur_ukb <- readRDS("generated_data/mi_firstoccur_ukb.RDS") %>% select(f.eid,event_dt)
```

Load cardiovascular event table that will be used to exclude controls

```r
cardio <- readRDS("generated_data/cardio_control_exclusion_events_ukb.RDS")
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_cardio <- cardio$f.eid %>% unique
```

Phenotype time-to-event

```r
mi_tte <- phenotype_time_to_event(dm_firstoccur, mi_firstoccur_ukb,
                        demog, control_exclusion_ids = ctrl_exclusion_ids_cardio)
```


```r
saveRDS(mi_tte,"generated_data/mi_tte.RDS")
```

## Unstable angina

Load unstable angina first occurrence data phenotyped from UKB assessment data

```r
unstable_angina_firstoccur_ukb <- 
  readRDS("generated_data/unstable_angina_firstoccur_ukb.RDS") %>% select(f.eid,event_dt)
```

Phenotype time-to-event

```r
unstable_angina_tte <- phenotype_time_to_event(dm_firstoccur,unstable_angina_firstoccur_ukb,demog)
```


```r
saveRDS(unstable_angina_tte,"generated_data/unstable_angina_tte.RDS")
```

## Ischemic stroke

Load ischemic stroke first occurrence data phenotyped from UKB assessment data

```r
ischemic_stroke_firstoccur_ukb <- readRDS("generated_data/stroke_infarct_firstoccur_ukb.RDS")
```

Load cerebrovascular event table that will be used to exclude controls

```r
cerebro <- readRDS("generated_data/cerebro_control_exclusion_events_ukb.RDS")
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_cerebro <- cerebro$f.eid %>% unique
```

Phenotype time-to-event

```r
ischemic_stroke_tte <- phenotype_time_to_event(dm_firstoccur,ischemic_stroke_firstoccur_ukb,demog,
                                               control_exclusion_ids = ctrl_exclusion_ids_cerebro)
```


```r
saveRDS(ischemic_stroke_tte,"generated_data/ischemic_stroke_tte.RDS")
```


## Hemorrhagic Stroke

Load hemorrhagic stroke first occurrence data phenotyped from UKB assessment data

```r
hem_stroke_firstoccur_ukb <- readRDS("generated_data/stroke_hem_firstoccur_ukb.RDS")
```

Load cerebrovascular event table that will be used to exclude controls

```r
cerebro <- readRDS("generated_data/cerebro_control_exclusion_events_ukb.RDS")
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_cerebro <- cerebro$f.eid %>% unique
```

Phenotype time-to-event

```r
hem_stroke_tte <- phenotype_time_to_event(dm_firstoccur,hem_stroke_firstoccur_ukb,demog,
                                          control_exclusion_ids = ctrl_exclusion_ids_cerebro)
```


```r
saveRDS(hem_stroke_tte,"generated_data/hem_stroke_tte.RDS")
```

## Stroke

Load stroke first occurrence data phenotyped from UKB assessment data

```r
stroke_firstoccur_ukb <- readRDS("generated_data/stroke_firstoccur_ukb.RDS")
```

Load cerebrovascular event table that will be used to exclude controls

```r
cerebro <- readRDS("generated_data/cerebro_control_exclusion_events_ukb.RDS")
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_cerebro <- cerebro$f.eid %>% unique
```

Phenotype time-to-event

```r
stroke_tte <- phenotype_time_to_event(dm_firstoccur,stroke_firstoccur_ukb,demog,
                                      control_exclusion_ids = ctrl_exclusion_ids_cerebro)
```


```r
saveRDS(stroke_tte,"generated_data/stroke_tte.RDS")
```


## PCI

Load PCI first occurrence data phenotyped from UKB assessment data

```r
pci_firstoccur_ukb <- readRDS("generated_data/pci_firstoccur_ukb.RDS")
```

Load non-coronary revascularization event table that will be used to exclude controls

```r
other_revas <- readRDS("generated_data/other_revas_control_exclusion_events_ukb.RDS")
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_other_revas <- other_revas$f.eid %>% unique
```

Phenotype time-to-event

```r
pci_tte <- phenotype_time_to_event(dm_firstoccur,pci_firstoccur_ukb,demog,
                                   control_exclusion_ids = ctrl_exclusion_ids_other_revas)
```


```r
saveRDS(pci_tte,"generated_data/pci_tte.RDS")
```


## Composite CVD (MI, Unstable Angina, Ischemic Stroke, PCI or CVD death)

Load composite CVD first occurrence data phenotyped from UKB assessment data

```r
cvd_firstoccur_ukb <- readRDS("generated_data/cvd_firstoccur_ukb.RDS")
```

Create control exclusion table for CVD. They are combination of cardio, cerebro and other revasculrization events that were used to exclude controls when phenotyping MI, unstable angina, ischemic sstroke and PCI time-to-event data.

```r
composite_cvd_control_exclusion_events <- list(cardio,cerebro,other_revas) %>% bind_rows()
```

Define control exclusion subject IDs

```r
ctrl_exclusion_ids_cvd <- composite_cvd_control_exclusion_events$f.eid %>% unique
```

Phenotype time-to-event

```r
cvd_tte <- phenotype_time_to_event(dm_firstoccur,cvd_firstoccur_ukb,demog,
                                   control_exclusion_ids = ctrl_exclusion_ids_cvd)
```


```r
saveRDS(cvd_tte,"generated_data/cvd_tte.RDS")
```








