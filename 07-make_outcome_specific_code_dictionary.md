# Make outcome-specific code dictionary {#make-outcome-specific-code-dictionary}

In this chapter, we create outcome-specific dictionaries. To do this, we define the following:

- inclusion codes or code patterns
- exclusion codes or code patterns
- inclusion keywords or keyword patterns
- exclusion keywords or keyword patterns

Using these, we subset the master dictionary (`full_dict.RDS`) to generate outcome-specific dictionary. The following are the event tables that require the primary care data and thus require outcome-specific dictionary:

- Diabetes event table
- Diabetic eye disease table for capturing diabetic eye disease cases
- Kidney disease case event table for capturing diabetic kidney disease cases
- Non-diabetic eye disease table for excluding controls from time-to-event data for DR
- Kidney disease event table for excluding controls from time-to-event data for DKD

We generate the following outcome-specific dictionaries:

- diabetes specific dictionary 
- diabetic eye disease dictionary
- kidney disease case dictionary
- non-diabetic eye disease dictionary
- kidney disease control exclusion event dictionary



Load packages.

```r
library(tidyverse)
library(data.table)
library(readxl)
```

Import the master dictionary.

```r
full_dict <- readRDS("generated_data/full_dict.RDS")
```

We first define the global exclusion keywords which we will be used to exclude certain codes from all of the outcome-specific dictionaries. These codes are related to the absence of a condition, screening for a condition, normal result for a condition, family history of a condition, various administrative statuses and more, and are not wanted when we capture the codes that are specific to certain outcomes.

```r
global_exclusion_keyword_patterns <- " normal| absent|inspection|examination|evaluation|referral|^no |^O/E- no |^O/E - no |excluded|not seen|seen by|not examined|did not attend|assessable|offered|^trauma| trauma|malnutrition|screening for|^FH|family history|interpret|under care|refused|unsuitable|at risk|resolved|disease screening|not indicated|letter|excepted|exception|fear of|education|sharing consent|tumour|pregnancy|gestational|diabetic child|complication of care|steriod-induced|lymphoma|cytomegalovirus|rubella|blastoma|histoplasm|toxoplasm|neoplasm|herpes|tubercul|pediculus| observation$|injury|streptococcal"
```
  
## Prepare diabetes code dictionary

### Define keywords

Define diabetes specific exclusion keywords.

```r
dm_specific_exclusion_keyword_patterns <- "|serum|antibody|remission|relative|association member|high risk of diabetes|suspected diabetes|non-diabetes|breath test|questionnaire|risk score|category score|risk calculator|inhibitor function|C-peptide level|factor binding protein 3|stress test|Insulin tolerance test|Urine screening test|X-ray|NHS Diabetes Prevention|Provision of diabetes clinical summary|diabetes mellitus screen|leaflet given|declined|C1-esterase|pituitary|helicobacter|ineligible|invite|invitation|insulinoma|steroid|secondary diabetes|pre-diabet|prediabet|insipid|provision of written information|not required|national audit|diabetes screen|renal diabetes|non-diabet|^diabetic nurse$|^Diabetic liaison nurse$|jamaica|secondary pancreatic diabetes|driving|neonatal|Addison|PABA test|growth factor|Plasma insulin level|key contact|eligibiliby|CHA2DS2|Professional judgement|Diabetes mellitus: no|non diabetic|information prescription|mother has|preg.|bronzed|Diabetes dietitian|Urine Ketone Test|deleted|refuse|gastropathy|pneumon|Frequency of hypoglycaem|^insulin level$|drug-induced|drug induced|hyperglyceridaemia"
```

Define diabetes exclusion keywords.

```r
dm_exclusion_keyword_patterns <- paste0(global_exclusion_keyword_patterns,dm_specific_exclusion_keyword_patterns)
```

Define inclusion keywords.

```r
dm_inclusion_keyword_patterns <- "diabetic|diabetes|diabeto|insulin|hyperglyc|hypoglyc|glycemic control"
```

### Define codes

Define diabetes exclusion codes.

```r
dm_exclusion_codes <- c("ZV653", "C3760", "J4z0", "Y3045", "7L1L2", "Y0015", "X789v", "Y7ITk", "Y2200", "42c..", "42W..", "42WZ.","66Ae.", "66Ae0","66AF.","C1…","XaCET","XaCEU", "XaCEV")
dm_exclusion_code_patterns <- paste(c("^42W","^42c"),collapse = '|')
```

Import diabetes code lists.

```r
dm_codelist <- fread("raw_data/dm_code_lists/opensafely-diabetes-2020-04-15.csv") %>% 
  full_join(fread("raw_data/dm_code_lists/opensafely-type-1-diabetes-2020-06-29.csv") 
            %>% mutate(Category=1)) %>%
  full_join(fread("raw_data/dm_code_lists/opensafely-type-2-diabetes-2020-06-29.csv") 
            %>% mutate(Category=2)) %>%
  full_join(fread("raw_data/dm_code_lists/opensafely-diabetes-exeter-group-2020-07-06.csv") 
            %>% dplyr::rename(CTV3PreferredTermDesc = ctvterm)) %>%
  dplyr::rename(term_description = CTV3PreferredTermDesc) %>%
  dplyr::rename(code = CTV3ID) %>%
  mutate(Category = as.character(Category)) %>%
  full_join(
    fread("raw_data/dm_code_lists/read_diabetescomplications_caliber.txt") %>%
    full_join(fread("raw_data/dm_code_lists/read_diabetes_expanded_caliber.txt")) %>%
    full_join(fread("raw_data/dm_code_lists/read_diabetes_caliber.txt")) %>%
      dplyr::rename(code = Clinical_code) %>%
      dplyr::rename(term_description = Clinical_term) %>%
      dplyr::rename(Category = `Category_(code)`)
    )
```

Define inclusion codes.

```r
dm_inclusion_codes <- dm_codelist$code
```

Define additional diabetes code patterns.

```r
dm_inclusion_code_patterns <- paste(c("^66A","^C10","^F420"),collapse = '|')
```

### Create diabetes code dictionary

Create diabetes code dictionary.

```r
dm_dict <- full_dict %>% 
  filter(grepl(dm_inclusion_keyword_patterns, term_description, ignore.case = T)|
           code %in% dm_inclusion_codes|
           grepl(dm_inclusion_code_patterns, code)) %>%
  filter(!grepl(dm_exclusion_keyword_patterns, term_description, ignore.case = T),
         !(code %in% dm_exclusion_codes),
         !grepl(dm_exclusion_code_patterns, code))
```

We want to filter outcome-specific dictionaries to only those codes that actually occur in the primary care data, to expedite review of the terms that we include.
First, read in the distinct terms in PC data. 

```r
terms_actual <- fread("generated_data/entire_gp_clinical_30March2021_formatted.txt", 
                      select = "code") %>%
  distinct()
```

Now, filter the DM dictionary to terms that exist in the PC data. 

```r
dm_dict_actual <- dm_dict %>% 
  filter(code %in% terms_actual$code) 

dm_dict_review <- dm_dict_actual %>% 
  distinct(code, term_description) %>% 
  distinct(code, .keep_all = T) 
```

Now, map the Read v2 terms in the dictionary to CTV3, and vice versa, to make sure we capture equivalent terms. The term mappings are provided by UKB in Resource 592. 

```r
map23 <- read_xlsx("raw_data/all_lkps_maps_v3.xlsx", sheet=14) 
map32 <- read_xlsx("raw_data/all_lkps_maps_v3.xlsx", sheet=19) 
read_map <- map32 %>%
  select(READV3_CODE, READV2_CODE, IS_ASSURED) %>%
  filter(IS_ASSURED == 1) %>%
  dplyr::rename(code = READV3_CODE, mapped_code = READV2_CODE) %>%
  mutate(terminology = "read3", mapped_terminology="read2") %>%
  select(-IS_ASSURED) %>%
  rbind(
    map23 %>% select(READV2_CODE, READV3_CODE) %>%
      dplyr::rename(code = READV2_CODE, mapped_code = READV3_CODE) %>%
      mutate(terminology = "read2", mapped_terminology ="read3")
    ) %>%
  distinct() %>% 
  filter(code != mapped_code) %>%
  filter(!grepl("\\.\\.", mapped_code) & !grepl("\\.\\.", code)) %>% #remove very broad mappings
  filter(code %in% terms_actual$code & mapped_code %in% terms_actual$code) #Only keep pairs that exist in gp_clinical
```

Get any additional mapped codes to include in the DM dictionary. 

```r
dm_terms_map <- left_join(dm_dict_actual, read_map) %>%
  filter(!is.na(mapped_code)) %>%
  filter(!(mapped_code %in% dm_dict$code)) %>%
  arrange(code) %>%
  select(-terminology_note) %>%
  left_join(full_dict %>%
              dplyr::rename(mapped_code = code, mapped_description = term_description,
                            mapped_terminology=terminology)) %>%
  group_by(mapped_code) %>%
  slice(1) %>%
  distinct() %>%
  filter(!(grepl("[Dd]rug induced", mapped_description)))
```

Combine the new terms with the original DM dictionary. 

```r
dm_dict_final <- rbind(dm_dict_actual,
                 dm_terms_map %>%
                   select(code = mapped_code, term_description = mapped_description, 
                          terminology = mapped_terminology, terminology_note)
)
```

Save diabetes code dictionary.

```r
saveRDS(dm_dict_final,"generated_data/dm_dict.RDS")
```

## Prepare diabetic eye disease code dictionary

### Define keywords

Define inclusion keywords.

```r
inclusion_keyword_patterns <- 
  "macul|retin|ophthalmic manifestation|ophthalmic complication|cataract|glaucoma|eye disease|diabetic iritis"
```

Define inclusion keywords for case.

```r
inclusion_keyword_patterns_case <- "diabetic|diabetes|diabet retinopathy"
```

Define exclusion keywords.

```r
exclusion_keyword_patterns <-
  " no maculopathy|^normal |translocation|cataract screen|retinol|incretin|diabetic retinopathy screening|retinal screening|steroid|drug induced|ratiation induced|branch of|vocational asses|electroretinography|buckling|tamponade|biopsy|retinopexy|cretin|glaucoma screen|pigment|fundoscopy|migraine|Commotio retinae|berlin|angiography|melanocytic macule|melanotic macule|acute retinal necrosis|radiation retinopathy|solar retinopathy|retinal dialysis|toxic maculopathy|eruption|retinacul|Acitretin|tretinoin|alopecia|branch of retinal artery$|rash"

exclusion_keywords <- c("O/E - retina", "Retinal photography", "Retinoscopy", 
                                   "Senile macular disorder screen", "Digital imaging of retina", 
                                   "[SO]Retina", "Retinal scan - laser", "Macula", "Retina",
                                   "Retinitis", "Fundus flavimaculatus", "Macula observation",
                                    "Superior temporal branch of retinal vein", "Senile macular disorder screening",
                                   "Retinal vein", "Branch of Retinal vein","Retinal artery","Limiting membrane of retina",
                                  "Nerve fibre layer of retina","Inferior temporal quadrant of retina","Central retinal vein",
                                  "Secondary syphilitic chorioretinitis", "Maculopapular", "Parafoveal retina", "Retinal structure",
                                   "Tomaculous neuropathy", "CMV retinitis", "Choroidal and retinal structures", "Retinal arteriole")
```

Define exclusion keywords for case.

```r
exclusion_keyword_patterns_case <- "non-diab|non diab"
```

### Define codes

Define inclusion codes.

```r
inclusion_code_patterns <- "^F42"
inclusion_codes <- c("F4407")
```

Define exclusion codes.

```r
exclusion_code_patterns <- c("^hg1|^hh51|^j24|^kaA|^m[5-7]")
```

### Create diabetic eye disease code dictionary

Create eye disease code dictionary.

```r
eye_disease_dict <- full_dict %>% 
  filter(grepl(inclusion_keyword_patterns, term_description, ignore.case = T) |
           grepl(inclusion_code_patterns, code, ignore.case = T) |
           code %in% inclusion_codes) %>%
  filter(!grepl(global_exclusion_keyword_patterns, term_description, ignore.case = T) &
         !grepl(exclusion_keyword_patterns, term_description, ignore.case = T) &
           !grepl(exclusion_code_patterns, code, ignore.case = F) &
         !(term_description %in% exclusion_keywords))
```

Get terms that actually occur in the PC data.

```r
eye_disease_actual <- eye_disease_dict %>%
  filter(code %in% terms_actual$code)
```

Create diabetic eye disease code dictionary.

```r
dm_eye_disease_dict <- 
  eye_disease_actual %>% 
  filter(grepl(inclusion_keyword_patterns_case, term_description, ignore.case=T) & 
           !grepl(exclusion_keyword_patterns_case, term_description, ignore.case=T))
```

Review unique terms.

```r
dr_pc_review <- dm_eye_disease_dict %>%
  distinct(code, term_description) %>%
  distinct(code, .keep_all = T)
```

See if there are any additional mapped terms. For diabetic retinopathy, there are none. 

```r
dr_terms_map <- left_join(dm_eye_disease_dict, read_map) %>%
  filter(!is.na(mapped_code)) %>%
  filter(!(mapped_code %in% dm_eye_disease_dict$code)) %>%
  arrange(code) %>%
  select(-terminology_note) %>%
  left_join(full_dict %>%
              dplyr::rename(mapped_code = code, mapped_description = term_description,
                            mapped_terminology=terminology)) %>%
  group_by(mapped_code) %>%
  slice(1) %>%
  distinct() 
dr_terms_map
```

Generate non-diabetic eye disease code dictionary.

```r
nondm_eye_disease_dict <-
  eye_disease_actual %>% filter(!(code %in% dm_eye_disease_dict$code))
```

Review non-diabetic eye disease terms. 

```r
nondm_eye_review <- nondm_eye_disease_dict %>%
  distinct(code, term_description) %>%
  distinct(code, .keep_all = T)
```

See if there are any additional mapped terms. 

```r
nondm_eye_disease_terms_map <- left_join(nondm_eye_disease_dict, read_map) %>%
  filter(!is.na(mapped_code)) %>%
  filter(!(mapped_code %in% nondm_eye_disease_dict$code)) %>%
  arrange(code) %>%
  select(-terminology_note) %>%
  left_join(full_dict %>%
              dplyr::rename(mapped_code = code, mapped_description = term_description,
                            mapped_terminology=terminology)) %>%
  group_by(mapped_code) %>%
  slice(1) %>%
  distinct()  %>%
  filter(!grepl("hypertensive", mapped_description))
nondm_eye_disease_terms_map
```

Combine the new terms with the original non-diabetic eye disease dictionary. 

```r
nondm_eye_disease_dict_final <- rbind(nondm_eye_disease_dict,
                 nondm_eye_disease_terms_map %>%
                   select(code = mapped_code, term_description = mapped_description, 
                          terminology = mapped_terminology, terminology_note)
)
```

Save diabetic eye disease code dictionary and non-diabetic eye disease code dictionary.

```r
saveRDS(dm_eye_disease_dict,"generated_data/dm_eye_disease_dict.RDS")
saveRDS(nondm_eye_disease_dict_final,"generated_data/nondm_eye_disease_dict.RDS")
```

## Prepare diabetic kidney disease code dictionary

### Define Keywords

Define inclusion keywords.

```r
inclusion_keyword_patterns <- "chronic kidney|chronic renal|ckd|glomerul|kidney failure|renal failure|uraemi|uremi|nephropath|proteinuri|albuminuri|nephrotic|nephrosis|nephritic|renal manifestation|renal complication|end stage renal|end stage kidney|esrd"
```

Define inclusion keywords for case.

```r
inclusion_keyword_patterns_case <- "stage 3|stage 4|stage 5|end stage|chronic renal failure|chronic kidney failure|chronic uraemia|G3|G4|G5|A3|diabetes|diabetic|persistent proteinur|persistent albuminur|persistent microalbuminur|persistent macroalbuminur|ns - nephrotic syndrome$|^nephrotic syndrome NOS$"

inclusion_keywords_case <- c("Nephrotic syndrome")
```

Define exclusion keywords.

```r
exclusion_keyword_patterns <- "nephritic factor|antibody|disease screening|rate testing|predicted stage|acute|induced by|metals|cadmium|lead|mercury|toxic|abortion|pregnancy|gestational|delivery|calculated by|^glomerular filtration rate$|nephropathy screen|invite|incipient|rate using|laboratory study|monitoring administration|Glomerular function test|benign|haemolytic|B12 deficiency|pyonephrosis|analgesic|Exercise|Adrenal|test strip|proteinuria negative|test urine sample"
```

### Define codes

Import diabetic kidney disease code lists and define inclusion codes.

```r
ckd_codelist <- fread("raw_data/kd_code_lists/opensafely-kidney-transplant-2020-07-15.csv") %>%
  full_join(fread("raw_data/kd_code_lists/opensafely-dialysis-2020-07-16.csv")) %>%
  full_join(fread("raw_data/kd_code_lists/opensafely-chronic-kidney-disease-2020-04-14.csv")) %>%
  dplyr::rename(term_description = CTV3PreferredTermDesc) %>%
  dplyr::rename(code = CTV3ID) 
inclusion_codes <- ckd_codelist$code
```

### Create diabetic kidney disease code dictionary

Create kidney disease dictionary.

```r
kidney_disease_dict <- full_dict %>% 
  filter(grepl(inclusion_keyword_patterns,term_description, ignore.case = T) |
           code %in% inclusion_codes) %>%
  filter(!grepl(global_exclusion_keyword_patterns, term_description, ignore.case = T) &
           !grepl(exclusion_keyword_patterns, term_description, ignore.case = T))
```

Filter to terms that actually appeared in the PC data to expedite review.

```r
kidney_disease_actual <- kidney_disease_dict %>%
  filter(code %in% terms_actual$code)
```

Generate kidney disease dictionary.

```r
kidney_disease_case_dict <- kidney_disease_actual %>% 
  filter(grepl(inclusion_keyword_patterns_case,term_description,ignore.case = T)|
           term_description %in% inclusion_keywords_case)
```

Review terms.

```r
kidney_disease_case_review <- kidney_disease_case_dict %>%
  distinct(code, term_description) %>%
  distinct(code, .keep_all = T)
```

See if there are any additional mapped terms. For kidney disease cases, there are none. 

```r
kidney_disease_case_terms_map <- left_join(kidney_disease_case_dict, read_map) %>%
  filter(!is.na(mapped_code)) %>%
  filter(!(mapped_code %in% kidney_disease_case_dict$code)) %>%
  arrange(code) %>%
  select(-terminology_note) %>%
  left_join(full_dict %>%
              dplyr::rename(mapped_code = code, mapped_description = term_description,
                            mapped_terminology=terminology)) %>%
  group_by(mapped_code) %>%
  slice(1) %>%
  distinct()  
kidney_disease_case_terms_map
```

Generate kidney disease control exclusion dictionary.

```r
kidney_disease_control_exclusion_dict <- 
  kidney_disease_actual %>% filter(!(code %in% kidney_disease_case_dict$code))
```

Review terms.

```r
kidney_disease_control_exclusion_review <-
  kidney_disease_control_exclusion_dict %>%
  distinct(code, term_description) %>%
  distinct(code, .keep_all =T)
```

See if there are any additional mapped terms.

```r
kidney_disease_control_exclusion_terms_map <- left_join(kidney_disease_control_exclusion_dict, read_map) %>%
  filter(!is.na(mapped_code)) %>%
  filter(!(mapped_code %in% kidney_disease_control_exclusion_dict$code)) %>%
  arrange(code) %>%
  select(-terminology_note) %>%
  left_join(full_dict %>%
              dplyr::rename(mapped_code = code, mapped_description = term_description,
                            mapped_terminology=terminology)) %>%
  group_by(mapped_code) %>%
  slice(1) %>%
  distinct()  
kidney_disease_control_exclusion_terms_map
```
Combine the new terms with the original kidney disease control exclusion dictionary. 

```r
kidney_disease_control_exclusion_dict_final <- rbind(kidney_disease_control_exclusion_dict,
                 kidney_disease_control_exclusion_terms_map %>%
                   select(code = mapped_code, term_description = mapped_description, 
                          terminology = mapped_terminology, terminology_note)
)
```

Save kidney disease case dictionary and kidney disease control exclusion code dictionary.

```r
saveRDS(kidney_disease_case_dict,"generated_data/kidney_disease_case_dict.RDS")
saveRDS(kidney_disease_control_exclusion_dict_final,"generated_data/kidney_disease_control_exclusion_dict.RDS")
```

