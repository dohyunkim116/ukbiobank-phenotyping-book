# Clean raw UKB assesment center data {#reformat-raw-data}

We reformat raw UKB assessment center data for easier data wrangling downstream. The scripts ending with `.r` contain reformatting instructions and running these scripts reformats the data. These scripts reside in `raw_data` folder. These scripts were first auto-generated using the ukbconv utility, which automatically applies encodings to data fields. This program can be obtained from the ![download](https://biobank.ndph.ox.ac.uk/showcase/download.cgi) section of the UKB showcase website. We used the ukbconv -i flag to specify a subset of fields to be included in each dataset.  For further details on downloading, decrypting, and converting the format of your main dataset(s), see ![UK Biobank's insructions](https://biobank.ndph.ox.ac.uk/~bbdatan/Accessing_UKB_data_v2.3.pdf). We slightly modified these scripts to use a faster reading function (fread() from data.table), rather than the default read.table(), and to change the name of the R objects storing each dataset (from "bd" to something descriptive of the contents of the dataset).




```r
library(tidyverse)
library(data.table)
```

Execute the reformatting scripts. 

```r
source("raw_data/demog_UKB_MOD.r")
source("raw_data/assessment_center_UKB.r")
source("raw_data/first_occurrences_UKB_MOD.r")
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130032.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130056.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130098.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130110.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130166.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130172.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130182.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130238.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130268.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130278.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130290.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130294.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130332.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130346.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130348.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130350.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130352.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130354.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130356.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130358.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130360.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130362.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130364.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130366.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130368.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130370.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130372.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130374.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130376.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130378.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130380.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130382.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130384.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130386.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130388.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130390.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130392.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130394.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130396.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130398.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130400.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130402.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130404.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130406.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130408.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130410.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130412.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130414.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130416.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130418.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130420.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130422.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130424.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130426.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130428.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130430.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130432.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130434.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130436.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130438.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130440.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130442.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130444.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130446.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130448.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130450.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130452.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130454.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130456.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130458.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130460.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130462.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130464.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130466.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130468.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130470.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130472.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130474.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130476.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130478.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130480.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130482.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130484.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130486.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130488.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130490.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130492.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130494.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130496.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130498.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130500.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130502.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130504.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130506.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130508.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130510.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130512.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130514.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130516.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130518.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130520.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130522.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130524.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130526.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130528.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130530.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130532.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130534.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130536.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130538.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130540.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130542.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130544.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130546.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130548.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130550.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130552.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130554.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130556.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130558.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130560.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130562.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130564.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130566.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130568.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130570.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130572.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130574.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130576.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130578.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130580.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130582.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130584.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130586.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130588.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130590.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130592.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130594.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130596.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130598.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130600.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130602.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130604.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130606.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130608.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130610.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130612.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130614.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130616.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130618.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130620.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130690.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130750.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130754.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130834.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.130956.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.131034.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.131510.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.131752.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132304.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132308.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132316.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132384.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132386.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132392.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132400.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132402.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132404.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132406.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132408.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132412.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132414.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132418.0.0' not found in column name header (case
## sensitive), skipping.
```

```
## Warning in fread("raw_data/first_occurrences_UKB.tab", header = TRUE, select =
## keep_fields): Column name 'f.132424.0.0' not found in column name header (case
## sensitive), skipping.
```

```r
source("raw_data/ICD_UKB_MOD.r")
source("raw_data/OPCS_procedures_UKB_MOD.r")
source("raw_data/sampleQC_UKB_MOD.r")
source("raw_data/labs_UKB_MOD.r")
```

Save reformatted UKB assessment center data

```r
saveRDS(demog,"generated_data/demog_UKB.RDS")
saveRDS(bd,"generated_data/assessment_center_UKB.RDS")
saveRDS(firstoccurs,"generated_data/first_occur_UKB.RDS")
saveRDS(procs,"generated_data/OPCS_procedures_UKB.RDS")
saveRDS(ICD,"generated_data/ICD_UKB.RDS")
saveRDS(sampleqc,"generated_data/sampleQC_UKB.RDS")
saveRDS(labs,"generated_data/labs_UKB.RDS")
```



















