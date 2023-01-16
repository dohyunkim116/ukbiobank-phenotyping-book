# Clean raw UKB assesment center data {#reformat-raw-data}

We first reformat raw UKB assessment center data using the R scripts (ending with `.r`) in `raw_data` folder for easier downstream data wrangling. These scripts were first auto-generated using the `ukbconv` utility, which automatically applies encodings to data fields. We have slightly modified the auto-generated scripts to achieve a faster reading of data by using `fread()` function from `data.table` package rather than the default `read.table()` function. We also changed the default naming of the R objects storing each dataset to something more descriptive which reflects the contents. The `ukbconv` utility can be obtained from the [download](https://biobank.ndph.ox.ac.uk/showcase/download.cgi) section of the UKB showcase website. We used the `ukbconv` with a flag `-i` to specify a subset of fields to be included in each dataset. For further details on downloading, decrypting, and converting the format of your main dataset(s), see [UK Biobank's insructions](https://biobank.ndph.ox.ac.uk/~bbdatan/Accessing_UKB_data_v2.3.pdf).



Load packages.

```r
library(tidyverse)
library(data.table)
```

Execute the reformatting scripts. 

```r
source("raw_data/demog_UKB_MOD.r")
source("raw_data/assessment_center_UKB.r")
source("raw_data/first_occurrences_UKB_MOD.r")
source("raw_data/ICD_UKB_MOD.r")
source("raw_data/OPCS_procedures_UKB_MOD.r")
source("raw_data/sampleQC_UKB_MOD.r")
source("raw_data/labs_UKB_MOD.r")
```

Save reformatted UKB assessment center data.

```r
saveRDS(demog,"generated_data/demog_UKB.RDS")
saveRDS(bd,"generated_data/assessment_center_UKB.RDS")
saveRDS(firstoccurs,"generated_data/first_occur_UKB.RDS")
saveRDS(procs,"generated_data/OPCS_procedures_UKB.RDS")
saveRDS(ICD,"generated_data/ICD_UKB.RDS")
saveRDS(sampleqc,"generated_data/sampleQC_UKB.RDS")
saveRDS(labs,"generated_data/labs_UKB.RDS")
```



















