This repository contains the analysis pipeline reproducing results
presented in this [paper]().

## Execution

The pipeline consists of a series of Rmarkdown files which should be
executed in order. For example,
`01-incidence_rate_and_tte_fut_summary.Rmd` should be executed first,
followed by `02-prep_data_baseline_demog.Rmd`, then
`03-prep_data_biomarker.Rmd` and so on. Note, execution times for
`05-impute.Rmd`, `06-primary_analysis.Rmd`,
`07-primary_analysis_dm2.Rmd`, `09-risk_prediction.Rmd` and
`10-risk_prediction_dm2.Rmd` are greater than 20 minutes and require
multiple cores.

## Data

There are several data which are required to run the analysis pipeline.
They are either directly sourced from `generated_data folder` which is
identical to the one in the [`main`
branch](https://github.com/dohyunkim116/ukbiobank-phenotyping-book/tree/main),
or from `phenotypes_all.RDS` which is generated in the folder
`generate_phenotypes_all_dot_RDS` (`phenotypes_all.RDS` is not generated
by the [UKB phenotyping
book](https://dohyunkim116.github.io/ukbiobank-phenotyping-book/)).
`phenotypes_all.RDS` is required to execute
`02-prep_data_baseline_demog.Rmd`. The raw UKB data required to generate
`phenotypes_all.RDS` are:

- `sampleQC_UKB.tab`
- `PCA_UKB.tab`
- `OPCS_procedures_UKB.tab`
- `labs_UKB.tab`
- `first_occurrences_UKB.tab`
- `entire_hesin_diag_11Feb2021`
- `entire_hesin_24March2021`
- `entire_gp_registrations_30March2021`
- `diet_exercise_UKB`
- `demog_UKB.tab`

We provide the scripts that reformat the raw data, which are in
`generate_phenotypes_all_dot_RDS` folder. The raw data can be requested
from UK Biobank study with appropriate access permission. Please refer
to the introductory paragraph in [chapter
1](https://dohyunkim116.github.io/ukbiobank-phenotyping-book/1-reformat-raw-data.html#reformat-raw-data)
of the phenotyping book for more information.

`phenotypes_all.RDS` (a data frame when you load it to the R
environment) should also contain numeric fields named `PGS000024` and
`PGS000036` representing T1D and T2D PGS scores, but they are missing
from the data generation pipeline. We do not provide the pipeline that
generates these scores here due to its complexity (requiring processing
the data using command lines, including PLINK). Nonetheless, PGS scores
can now be accessed through UKB data fields, 26283-26286. More
information about these fields can be found
[here](https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=300).
