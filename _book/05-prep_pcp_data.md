# Prepare UKB Primary Care Data {#prep_pcp_data}

The priamry care data contain information about 500,000 patients on their visits to primary care physicians. A reason for a visit is recorded as a code. The date of visit associated with the code is also available. We achieve the following in this folder:

1. Convert date field to date class
2. Convert special dates to their mapping
3. Generate subject ID file (used later for excluding control subjects when phenotyping DR and CKD)
4. Exclude subjects with sex mismatch

The special dates and their mappings are:

|Special date|Map|
|------------|---|
|1900-01-01|Missing|
|1901-01-01|Missing|
|2037-07-07|Missing||
|1902-02-02|DOB of a subject|
|1903-03-03|DOB of a subject|

As we did in \@ref(exclude-sex-mismatch-subjects), we exclude any subjects who have mismatch between self-reported sex and genetically determined sex from the primary care data. However, one can check that the number of unique subjects remain the same after applying sex mismatch filtering.


Input and output files:

- Input: 
  - `sex_mismatch_subject_ids.RDS` (from \@ref(exclude-sex-mismatch-subjects))
  - `demog_selected.RDS` (from \@ref(curate-demog-table)
  - `entire_gp_clinical_30March2021.txt` (from `raw_data` folder. This table can be downloaded by approved researchers from the UKB Record Repository if you have requested field 42040.)
  
- Output:
  - `entire_gp_clinical_30March2021_formatted.txt`
  - `gp_subject_ids.RDS`

This step cleans primary care table gp_clinical on March 31, 2021.  Reformatted primary care table will be used for phenotyping diabetes, and diabetes complications outcomes.\par




```r
library(data.table)
library(tidyverse)
source("functions.R")
```

Import primary care data

```r
gp_clinical <- fread("raw_data/entire_gp_clinical_30March2021.txt")
```

Fix the date format so that other scripts can use it more easily.

```r
gp_clinical$event_dt <- as.Date(gp_clinical$event_dt, tryFormats = "%d/%m/%Y")
```

Create code and terminology fields in place of read_2 and read_3 fields:

```r
gp_clinical <- gp_clinical %>% mutate(code = ifelse(read_2 != "", read_2, read_3)) %>%
  mutate(terminology = ifelse(read_2 != "", "read2", "read3")) %>%
  select(-read_2, -read_3) %>%
  distinct()
```

Rename fid (subject id) field

```r
gp_clinical <- gp_clinical %>% rename(f.eid = eid)
```

Load demographic data

```r
demog <- readRDS("generated_data/demog_selected.RDS")
```

Add a field indicating whether the date of event is special

```r
gp_clinical$special_dt <- is_special_date(gp_clinical$event_dt)
```

Convert the special dates

```r
demog_dob <- demog %>% select(f.eid, DOB)
gp_clinical <- gp_clinical %>% left_join(demog_dob, by = "f.eid")
gp_clinical$event_dt <- cleandates(gp_clinical$event_dt,gp_clinical$DOB)
gp_clinical <- gp_clinical %>% select(-DOB)
```

Save subject IDs represented in the primary care data

```r
gp_subject_ids <- gp_clinical$f.eid %>% unique()
saveRDS(gp_subject_ids,"generated_data/gp_subject_ids.RDS")
```

Remove any events with missing dates

```r
gp_clinical <- gp_clinical %>% filter(!is.na(event_dt))
```

Filter out subjects with sex mismatch

```r
sex_mismatch_subject_ids <- readRDS("generated_data/sex_mismatch_subject_ids.RDS")
gp_clinical <- gp_clinical %>% filter(!(f.eid %in% sex_mismatch_subject_ids))
```

Save the primary care data

```r
fwrite(gp_clinical, "generated_data/entire_gp_clinical_30March2021_formatted.txt", sep="\t", row.names=F, quote=T)
```













