# Create a master code dictionary for primary care data {#make-code-dictionary}

To phenotype events using the primary care data, we need to identify sets of codes that represent clinical outcomes. To identify these codes, we first curate a master code dictionary which contains all of the codes and their meanings associated with clinical events in the primary care data. The master dictionary is a combination dictionaries for Read v2 and Read v3 coding classification schemes. Read v3 is also called Clinical Terms Version 3 (CTV3). Additional Read v3 codings and their meanings are available through a dictionary called "TPP local." TPP is one of the electronic health care record system suppliers for general practice (primary care) in England.

The purpose of curating the master dictionary is to create an outcome-specific dictionary. For example, for phenotyping diabetes, we identify a certain set of codes that are known to indicate diabetes. Since there does not exist a standard, pre-defined list of codes indicating diabetes, we also search the descriptions of the keyword in the dictionary in an attempt to fully capture codes associated with diabetes. This process requires an extensive manual inspection to avoid mistakenly selecting codes that do not actually indicate diabetes. However, once the list of codes specific to diabetes is created, identifying diabetes subjects from the primary care data is straightforward. The identical process is adopted to curate DKD and DR specific code dictionaries.



Load packages.

```r
library(data.table)
library(tidyverse)
```

Import Read v2 code dictionary data.

```r
read2_dict_nondrug <- openxlsx::read.xlsx("raw_data/all_lkps_maps_v3.xlsx", sheet = "read_v2_lkp")
read2_dict_drug <- openxlsx::read.xlsx("raw_data/all_lkps_maps_v3.xlsx", sheet = "read_v2_drugs_lkp")
```

Combine drug and nondrug Read v2 dictionaries. Keep only `read_code` and `term_description` fields.

```r
read2_dict <- full_join(read2_dict_drug,read2_dict_nondrug) %>%
  select(read_code,term_description)
```

Import Read v3 code dictionary data

```r
read3_dict <- openxlsx::read.xlsx("raw_data/all_lkps_maps_v3.xlsx", sheet = "read_ctv3_lkp")
```

Note that Read v3 dictionary has identical codes that start with period and without, for example, .9m05 vs 9m05. In the UKB database, the one starting with '.' is never used, so we filter out these codes.

```r
read3_dict <- read3_dict %>% 
  select(read_code, term_description) %>%
  filter(!(grepl('^\\.', read_code)))
```

We check whether Read v2 and Read v3 codes overlap. It seems all codes in Read v2 dictionary appear in Read v3 dictionary except for the following:

```r
read2_dict$read_code[!(read2_dict$read_code %in% read3_dict$read_code)]
```

Import TPP local codes which are also used in primary care data. These begin with 'Y' (Note: `fread()` automatically resolved the parsing error that `read_delim()` could not, so we used `fread()` to import the TPP local code data).

```r
tpp_local <- fread("raw_data/tpp_local.txt")
tpp_local %>% head(10)
```

TPP codes do not appear in Read v2 or Read v3 dictionaries.

```r
tpp_local %>% filter(code %in% read2_dict$read_code)
tpp_local %>% filter(code %in% read3_dict$read_code)
```

Finally, we merge all of the dictionaries. The master dictionary is intended to cover all of the descriptions in Read v2 and Read v3 code dictionaries. Therefore, we expect to see one-to-many mapping from code to descriptions. This will ensure we do not miss descriptions when we phenotype outcomes using the primary care data in later chapters.

```r
full_dict <- 
  full_join(read2_dict %>% rename(code = read_code) %>% mutate(terminology="read2"),
            read3_dict %>% rename(code = read_code) %>% mutate(terminology="read3")) %>%
  full_join(tpp_local %>% mutate(terminology="read3", terminology_note = "TPP Local Code")) %>%
  distinct()
```

Save the full code dictionary for primary care events.

```r
saveRDS(full_dict,"generated_data/full_dict.RDS")
```


