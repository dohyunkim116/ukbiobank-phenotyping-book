library(tidyverse)

# merge_long function that takes codes table and dates table to merge and make it into a long format. 
# Works on ICD10, OPCS4, Self-reported and Self-reported operation data
merge_long <- function(code_field,date_field,code_tab,date_tab,type){
  wide_tab <- full_join(code_tab, date_tab, by="f.eid")
  wide_tab %>%
    pivot_longer(cols = starts_with(c(all_of(code_field),all_of(date_field))),
                 names_to = ".value",names_pattern="(^f\\.[0-9]{5})") %>%
    rename(code=code_field,event_dt=date_field) %>% mutate(type = type)
}

# Given vector of dates "dates", returns vector of Boolean values indicating which are "special", i.e. 
# "1900-01-01", "1901-01-01", and "2037-07-07" indicate missing values and
# "1902-02-02" and "1903-03-03" indicate DOBs.
is_special_date <- function(dates){
  dates %in% as.Date(c("1900-01-01", "1901-01-01", "2037-07-07","1902-02-02", "1903-03-03"))
}


# cleandates
# Accepts a Vector of dates and a vector of Dates of birth (DOB).
# Based on UKB special date values, replaces special dates with appropriate meanings.
# 1900-01-01 means the date is unknown.  1901-01-01 = date given is before patients DOB. 2037-07-07 = event given is in the future (probable placeholder).
# The above dates are replaced with missing.
# 1902-02-02 means the date matches the patients DOB.  1903-03-03 means it was in the same year as their DOB. These were masked due to potentially sensitive info.
# The above dates are replaced with the DOB.
# If remove_special = T, then we set dates matching 1902-02-02 and 1903-03-03 with NA
cleandates <- function(dates, DOBs, remove_special = F) {
  dates[dates %in% as.Date(c("1900-01-01", "1901-01-01", "2037-07-07"))] <- NA 
  if(remove_special){
    dates[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] <- NA
  }
  else {
    dates[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] <- 
      DOBs[dates %in% as.Date(c("1902-02-02", "1903-03-03"))]   
  }
  return(dates)
}

# Define eGFR formula
compute_egfr <- function(creat_blood,age,sex){
  sex <- tolower(sex)
  if(is.na(sex)) return(NA)
  
  kappa_egfr <- NA
  alpha_egfr <- NA
  
  if(sex == "female"){
    kappa_egfr <- 61.9
  }
  else if(sex == "male"){
    kappa_egfr <- 79.6
  }
  if(sex == "female"){
    alpha_egfr  <- -0.329
  }
  else if(sex == "male"){
    alpha_egfr <- -0.411
  }
  
  if(is.na(kappa_egfr) || is.na(alpha_egfr)) return(NA)
  
  egfr <- 141*pmin(creat_blood/kappa_egfr,1)^alpha_egfr*pmax(creat_blood/kappa_egfr, 1)^(-1.209)*0.993^age
  egfr * ifelse(sex=="female", 1.018, 1)
}

# Input:
# field_patterns
# icd10_patterns
# opcs_patterns
# self_rep_patterns
# selfrep_op_patterns
# event_tab
# firstoccur: indicate whether to outout first occurrences or all events
# Output:
# DM complication cases matching provided patterns
get_phenotype_tab <- function(field_patterns = NULL,icd10_patterns_any = NULL, 
                              icd10_patterns_pd = NULL, icd10_patterns_sd = NULL,
                              icd9_patterns_any = NULL,
                              opcs_patterns = NULL,
                              selfrep_patterns = NULL, selfrep_op_patterns = NULL, custom_field_patterns = NULL,
                              event_tab,firstoccur=T){
  pattern_search_list <- 
    list(outcome_fields = field_patterns, OPCS4 = opcs_patterns,
         selfrep = selfrep_patterns, selfrep_op = selfrep_op_patterns, 
         custom_fields = custom_field_patterns, ICD9 = icd9_patterns_any)
  if(!is.null(icd10_patterns_any)){
    pattern_search_list[["ICD10"]] <- icd10_patterns_any
    pattern_search_list[["ICD10_death_primary"]] <- icd10_patterns_any
    pattern_search_list[["ICD10_death_secondary"]] <- icd10_patterns_any
  }
  else {
    pattern_search_list[["ICD10_death_primary"]] <- icd10_patterns_pd
    pattern_search_list[["ICD10_death_secondary"]] <- icd10_patterns_sd
  }
  pattern_search_list <- compact(pattern_search_list)
  
  #print(names(pattern_search_list))
  #print(pattern_search_list)
  tab <- lapply(1:length(pattern_search_list),function(i){
    pattern_val <- pattern_search_list[[i]]
    type_val <- names(pattern_search_list)[i]
    #print(type_val)
    event_tab %>% filter(grepl(pattern_val,key) & type == type_val) %>% select(f.eid,event_dt,key,type)
  }) %>% do.call(rbind,.) %>% distinct 
  #return(tab)
  
  if(firstoccur){
    return(tab %>% group_by(f.eid) %>% arrange(event_dt) %>% slice(1) %>% ungroup())
  }
  else {
    return(tab %>% select(f.eid,event_dt) %>% distinct() %>% arrange(f.eid,event_dt))
  }
}

# Returns combination of data frame arguments usable as an argument for phenotype_tte()
pre_phenotype_tte <- function(dm_firstoccur,comp_firstoccur,demog,
                              control_exclusion_ids=NULL,
                              control_inclusion_ids=NULL){
  tab <- dm_firstoccur %>% left_join(comp_firstoccur,by="f.eid",suffix = c("_dm","_comp")) 
  
  # Define observed DM complication events. If event_dt_comp = NA, then
  # a subject did not have DM complication event. Otherwise, 
  # then we assume a subject did have DM complication event.
  tab <- tab %>% mutate(event=ifelse(is.na(event_dt_comp),0,1))
  
  # prior complication indicator for excluding cases
  tab <- tab %>% mutate(prior_comp = (event_dt_dm > event_dt_comp))
  
  tab <- tab %>% left_join(demog %>% select(f.eid,date_censored,date_init),by="f.eid")
  
  # nonsensical observation indicator for excluding cases and controls
  tab <- tab %>% mutate(nonsense_case = (event_dt_comp > date_censored))
  tab <- tab %>% mutate(nonsense_ctrl = (event_dt_dm > date_censored))
  
  # follow-up-time less than 5 years indicator for excluding controls
  tab <- tab %>% mutate(fut_less_than_5yrs = ((lubridate::decimal_date(date_censored) - lubridate::decimal_date(event_dt_dm) < 5)))
  
  # control exclusion indicator built using IDs in control exclusion tables
  if(!is.null(control_exclusion_ids)){
    tab <- tab %>% mutate(ctrl_exclude = (f.eid %in% control_exclusion_ids))
  }
  else{
    tab <- tab %>% mutate(ctrl_exclude = 0)
  }
  
  # control inclusion indicator built using IDs represented in tables
  if(!is.null(control_inclusion_ids)){
    tab <- tab %>% mutate(ctrl_include = (f.eid %in% control_inclusion_ids))
  }
  else {
    tab <- tab %>% mutate(ctrl_include = 1)
  }
  
  # indicator noting initiation date is NOT inside the interval between 
  # the date of first occurrence of DM first-occurrence and 
  # the date of first complication event to exclude cases
  tab <- tab %>% mutate(init_pre_dm = (date_init < event_dt_dm),
                        init_post_comp = (date_init > event_dt_comp))
  
  # indicator noting the date of first occurrence of DM is more than 6 months
  # after the study initiation date
  tab %>% mutate(dm_post_init_gt_6mo = 
                 (event_dt_dm > lubridate::add_with_rollback(date_init, months(6),roll_to_first = TRUE)))
}
#Return a dataframe with subjects marked with different criteria by which they can be filtered
pre_phenotype_tte_interval <- function(dm_date_known, comp_date_known, demog,
                                       control_exclusion_ids=NULL,
                                       control_inclusion_ids=NULL){
  #Merge dm and comp dates and known flags
  tab <- dm_date_known %>% left_join(comp_date_known, by='f.eid', suffix=c('_dm','_comp'))
  
  #Fix ncolumn names if known flags are not given for complication dates
  if(!is.null(tab$known)){
    tab <- tab %>% rename('known_dm' = 'known')
  }
  
  #Mark controls as 0 and cases as 1
  tab <- tab %>% mutate(event = ifelse(is.na(event_dt_comp),0,1))
  
  #Mark cases where complication occured before dm diagnosis
  tab <- tab %>% mutate(prior_comp = (event_dt_dm > event_dt_comp))
  
  #Merge with demographic data to access censoring dates
  tab <- tab %>% left_join(demog %>% select(f.eid,date_censored), by='f.eid')
  
  #Mark nonsense cases where censoring happened before complication
  tab <- tab %>% mutate(nonsense_case = (event_dt_comp>date_censored))

  #Mark nonsense controls where dm diagnosis happened after censoring date
  tab <- tab %>% mutate(nonsense_ctrl = (event_dt_dm>date_censored))
  
  ##Mark controls where follow-up time is less than 5 years
  tab <- tab %>% mutate(fut_less_than_5yrs = (decimal_date(date_censored)-decimal_date(event_dt_dm)<5))
  
  #If exclusion ids are given, mark controls to be excluded
  if(!is.null(control_exclusion_ids)){
    tab <- tab %>% mutate(ctrl_exclude = (f.eid %in% control_exclusion_ids))
  }else{
    tab <- tab %>% mutate(ctrl_exclude = 0)
  }
  
  #If inclusion ids are given, mark controls to be included
  if(!is.null(control_inclusion_ids)){
    tab <- tab %>% mutate(ctrl_include = (f.eid %in% control_inclusion_ids))
  }else{
    tab <- tab %>% mutate(ctrl_include = 1)
  }
}
# Filters data frame argument by row, adds "time_to_event" column, and returns relevant columns.
phenotype_tte <- function(pre_phenotype_tte_tab){
  # Filter out certain rows (which rows are we filtering for?)
  tte <- pre_phenotype_tte_tab %>%
    filter(event == 0 | (event == 1 & nonsense_case == 0 & prior_comp == 0 &  init_pre_dm == 0 & init_post_comp == 0)) %>%
    filter(event == 1 | (event == 0 & nonsense_ctrl == 0 & ctrl_exclude == 0 & fut_less_than_5yrs == 0 & 
                           ctrl_include == 1 & dm_post_init_gt_6mo == 0))
  
  tte %>% 
    mutate(time_to_event = ifelse(event == 1, 
                                  lubridate::decimal_date(event_dt_comp) - lubridate::decimal_date(date_init),
                                  lubridate::decimal_date(date_censored) - lubridate::decimal_date(date_init))) %>% 
    select(f.eid,time_to_event,event,prior_comp,init_pre_dm,init_post_comp,
           ctrl_exclude, fut_less_than_5yrs, ctrl_include, dm_post_init_gt_6mo, nonsense_case, nonsense_ctrl,
           date_init,date_censored,event_dt_dm,event_dt_comp)
}
#Filter out subjects based on criteria from pre_phenotype_tte_interval
#Define lower and upper bounds on time from dm diagnosis to complication
phenotype_tte_interval <- function(tab, dm_alternative, 
                                   comp_alternative,
                                   filter_cases_lt5yrs){
  
  #Filter out subjects based on pre_phenotype_tte_interval criteria
  tab <- tab %>% 
    filter(event==0 | event==1 & nonsense_case==0 & prior_comp==0) %>%
    filter(event==1 | event==0 & nonsense_ctrl==0 & fut_less_than_5yrs==0 &
             ctrl_exclude==0 & ctrl_include==1)
  
  #Set the lower bound for all subjects from dm date to censored date for controls and 
  #from dm date to comp date for cases
  tab <- tab %>% mutate(lower = ifelse(event, 
                                       decimal_date(event_dt_comp)-decimal_date(event_dt_dm),
                                       decimal_date(date_censored)-decimal_date(event_dt_dm)))
                                                      
  #Set all upper bounds equal to all lower bounds
  tab <- tab %>% mutate(upper = lower)
  
  #If an alternative dm date has been given, reset upper bounds to be from this 
  #alternative to complication or censoring date for unknown dm subjects
  if(!is.null(dm_alternative)){
    tab <- tab %>% left_join(dm_alternative, by='f.eid', all.x=T) %>%
      rename('event_dt_dm_alt' = 'event_dt')
    tab <- tab %>% mutate(upper = ifelse(known_dm, upper, 
                                          ifelse(event, decimal_date(event_dt_comp)-decimal_date(event_dt_dm_alt),
                                                 decimal_date(date_censored)-decimal_date(event_dt_dm_alt))))
  }else if(!is.null(comp_alternative)){
    #If an alternative complication date is given, reset lower bounds to be from dm date
    #to this new complication date for cases with unknown complication date
    #Filter out subjects with unknown dm date
    tab <- tab %>% filter(known_dm==1)
    if(length(comp_alternative)==1){
      #If comp alternative is 0, set lower bounds of unknown comp date cases to 0
      tab <- tab %>% mutate(lower = ifelse(!event | known_comp, lower, 0))
    }else{
      tab <- tab %>% left_join(comp_alternative, by='f.eid')
      tab <- tab %>% rename('event_dt_comp_alt' = 'event_dt')
      tab <- tab %>% mutate(lower = ifelse(!event | known_comp, upper,
                                           ifelse(is.na(event_dt_comp_alt),0,decimal_date(event_dt_comp_alt)-decimal_date(event_dt_dm))))
    }
  }else{
    #If no alternative dates are provided we assume we want right censored data
    #Filter out all subjects where dm or comp date is unknown
    tab <- tab %>% filter(known_dm==1 & (is.na(known_comp)|known_comp==1))
  }
  #Optionally filter out cases with lower bounds less than 5 years
  if(filter_cases_lt5yrs){
    tab <- tab %>% filter(!event | lower>=5)
  }
  return(tab)
}
# Run pre_phenotype_tte() on arguments, then run phenotype_tte() on returned data frame.
# Return relevant columns of data frame returned by phenotype_tte().
phenotype_time_to_event <- function(dm_firstoccur,comp_firstoccur,demog,
                                    control_exclusion_ids=NULL,
                                    control_inclusion_ids=NULL){
  
  pre_phenotype_tte(dm_firstoccur,comp_firstoccur,demog,
                    control_exclusion_ids,control_inclusion_ids) %>%
    phenotype_tte() %>% select(f.eid,time_to_event,event,event_dt_dm,event_dt_comp)
}
#Run pre_phenotype_tte_interval on arguments
#Then run phenotype_tte_interval on returned dataframe and the dm and comp diagnosis date alternatives
phenotype_time_to_event_interval <- function(dm_date_known, comp_date_known, demog,
                                             control_exclusion_ids=NULL, 
                                             control_inclusion_ids=NULL, 
                                             dm_alternative=NULL,
                                             comp_alternative=NULL,
                                             filter_cases_lt5yrs=0){
  tte_interval <- pre_phenotype_tte_interval(dm_date_known, comp_date_known,demog,
                             control_exclusion_ids, 
                             control_inclusion_ids) %>% 
    phenotype_tte_interval(dm_alternative, comp_alternative, filter_cases_lt5yrs)
  if(is.null(tte_interval$known_comp)){
    tte_interval <- tte_interval %>% select(f.eid,event,known_dm,lower,upper)
  }else{
    tte_interval <- tte_interval %>% select(f.eid,event,known_comp,lower,upper)
  }
  return(tte_interval)
}
