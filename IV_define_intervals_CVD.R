library(tictoc)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(openxlsx)

setwd("/u/project/biostat-chair/aubreyje/IntervalCensoringProject/")

dm_IV <- fread("dm_dates.txt") 

dm <- dm_IV %>%
  mutate(years_DM_to_CVD_point_est = 
           decimal_date(date_first_known_CVD) - decimal_date(date_first_known_DM),
         first_linkage = pmin(date_first_gp, date_first_hosp, na.rm = T),
         last_linkage = pmax(date_last_gp, date_last_hosp, na.rm = T),
         last_contact = pmax(date_last_assessment, last_linkage, na.rm=T),
         first_contact = pmin(date_first_assessment, first_linkage, na.rm=T),
         CVD = as.numeric(!is.na(date_first_known_CVD)),
         died = as.numeric(!is.na(date_death)),
         evidence_t1d = !is.na(date_firstoc_t1d) | gp_t1d | (!is.na(selfrep_t1d) & selfrep_t1d),
         evidence_t2d = !is.na(date_firstoc_t2d) | gp_t2d | (!is.na(selfrep_t2d) & selfrep_t2d),
         dm_type = ifelse(evidence_t2d & !evidence_t1d, "Type II",
                          ifelse(evidence_t1d & !evidence_t2d, "Type I",
                                 ifelse(evidence_t1d & evidence_t2d, "Both noted","Unspecified")))) 
table(dm$dm_type, useNA = 'always')  
# Both noted      Type I     Type II Unspecified        <NA> 
#   3802        1038       33733        4212           0

dm %>% filter(date_first_known_DM > last_contact) %>%
       select(date_first_known_DM, last_contact, date_death, dm_on_deathcert)
#All of them either got it from their death certificate OR
#It is just a rounding error up to the first day of the next month 

#If the DM first diagnosis date is known (DM diagnosis date = T_{DM}) and CVD complication 
# has already occurred, then the time-to-event is exactly observed 
# (L_i = R_i = T_{CVD} - DM diagnosis date). 
dm_known_CVD <- dm %>% 
  filter(FLAG_date_first_DM_known == 1 & CVD == 1) %>%
  mutate(CVD_L_i = years_DM_to_CVD_point_est,
         CVD_R_i = years_DM_to_CVD_point_est,
         censor_type_CVD = "Observed")


# If the DM first diagnosis date is known and CVD complication has not occurred yet 
# (T_{CVD}=\infty), then the time-to-event is lower bounded by (L_i = 
# max(last UKB assessment time of this individual, last admission record date, 
# last primary care record date) - T_{DM}). Right-censored (R_i = \infty). 
dm_known_noCVD <- dm %>% 
  filter(FLAG_date_first_DM_known == 1 & CVD == 0) %>%
  mutate(CVD_L_i = decimal_date(last_contact) - decimal_date(date_first_known_DM),
         CVD_R_i = Inf,
         censor_type_CVD = "Right-censored (DM date known)")


# If the DM first diagnosis date is unknown (DM diagnosis date before $T_{DM}$) and 
# CVD complication has already occurred, then the time-to-event is lower bounded by 
# (L_i = T_{CVD} - min(first primary care or admission record time, T_{DM})) 
# and upper bounded by (R_i = T_{CVD} - birth date). interval-censored. 
dm_unknown_CVD <- dm %>% 
  filter(FLAG_date_first_DM_known == 0 & CVD == 1) %>%
  mutate(CVD_L_i = decimal_date(date_first_known_CVD) - 
           #decimal_date(pmin(first_linkage, date_first_known_DM, na.rm=T)), ##?????
           #I do not believe the above is appropriate
           #I use DM instead until we can discuss.
           #Imageine firstpc < real onset < known DM < CVD. firstpc cannot define lower bound
           decimal_date(date_first_known_DM),
         CVD_R_i = decimal_date(date_first_known_CVD) - decimal_date(DOB),
         censor_type_CVD = "Interval-censored")


# If the DM first diagnosis date is unknown (DM diagnosis date before $T_{DM}$) 
# and CVD complication has not occurred yet (T_{CVD}=\infty), then the time-to-event 
# is lower bounded by (L_i = max(last UKB assessment time of this individual, 
# last admission record date, last primary care record date) - T_{DM}). 
# Right-censored (R_i = \infty).
dm_unknown_noCVD <- dm %>% 
  filter(FLAG_date_first_DM_known == 0 & CVD == 0) %>%
  mutate(CVD_L_i = decimal_date(last_contact) - decimal_date(date_first_known_DM),
         CVD_R_i = Inf,
         censor_type_CVD = "Right-censored (DM date unknown)")


dm_all <- rbind(dm_known_CVD, dm_known_noCVD, dm_unknown_noCVD, dm_unknown_CVD) %>%
  mutate(CVD_timeframe = CVD_R_i - CVD_L_i)

#Sanity check
table(dm_all$CVD_R_i >= dm_all$years_DM_to_CVD_point_est &
        dm_all$years_DM_to_CVD_point_es >= dm_all$CVD_L_i)

l0 <- dm_all %>%
  filter(CVD_L_i > 0) 

dat_4plot <- l0 %>%
  select(f.eid, censor_type_CVD, dm_type, CVD_L_i, CVD_R_i) %>%
  pivot_longer(CVD_L_i:CVD_R_i, names_to = "Interval", values_to = "years")

g4 <- ggplot(data=dat_4plot, 
             aes(x=censor_type_CVD, y=years, fill=Interval))+
  geom_boxplot()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=-90)) +
  ggtitle("Lower and Upper bounds by censoring type, among L_i > 0") +
  facet_grid(~dm_type) +
  geom_text(data=dat_4plot %>% 
              group_by(censor_type_CVD,dm_type,Interval) %>% 
              summarise(n=n()), 
            aes(x=censor_type_CVD, y=85,label= paste0("n=", n)), 
            nudge_y=1, nudge_x=0) 
g4
table(l0$censor_type_CVD)
# Interval-censored                         Observed 
# 372                             3756 
# Right-censored (DM date known) Right-censored (DM date unknown) 
# 27985                             5485

ggsave("boxplots.bydmtype.png", g4, width=15, height=10, dpi=400)

dict_key <- c( "f.eid" = "Participant ID, identical to eid/IID/FID",
               "date_first_known_DM" = "Date of first evidence of diabetes. Sources are primary care data (gp_clinical table), self-report (field 20002+20008), first occurrences fields for E10/E11/E14, death records, and hospital admissions.",
               "gp_t2d"  = "Whether there is evidence of specifically TYPE 2 diabetes in gp_clinical",
               "gp_t1d"   = "Whether there is evidence of specifically TYPE 1 diabetes in gp_clinical",
               "DOB"  = "Date of birth.  Year and month are true, while the day is the first of the month to protect participants' privacy.",
               "age_2010" = "Age on 1 January 2010",
               "BMI" = "Body mass Index",
               "EUR" = "Whether the participant belongs to a population that is both genetically determined to be of European ancestry, AND to self report white British ancestry.",
               "in_gp_clin" = "Whether the participant is in the gp_clinical table",
               "SEX"  = "Sex",
               "ethnic_self" = "Self-reported ethnic background",
               "smoke_ever" = "Ever smoked",
               "packyears_smoke" = "Pack-years of cigarette smoking",
               "max_qualification" = "Maximum educational attainment (UK)",
               "ISCED_level" = "Educational attainment in standardized international term",
               "ISCED_level_gt2" = "Whether ISCED level is 3/4/5",
               "MET_minutes" = "Physical activity level measured by MET minutes per week",
               "MET_decile" = "MET minutes broken into deciles",
               "MET_tertile" = "MET minutes broken into tertiles",
               "PC1"  = "Genetic principal component 1",
               "PC2"  = "Genetic principal component 2",
               "PC3" = "Genetic principal component 3",                    
               "PC4" = "Genetic principal component 4",
               "PC5"  = "Genetic principal component 5",
               "PC6" = "Genetic principal component 6",                     
               "PC7" = "Genetic principal component 7",
               "PC8" = "Genetic principal component 8",
               "PC9" = "Genetic principal component 9",                     
               "PC10" = "Genetic principal component 10",
               "PGS000024"  = "Polygenic risk score for T1D for EUR cohort",
               "PGS000036"  = "Polygenic risk score for T2D for EUR cohort",              
               "used_in_genetic_PCA" = "See Data-field 22020 https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=22020",
               "date_firstoc_t2d" = "Date of first occurrence of terms/fields mapped to E11. See Data-field 130708",
               "date_firstoc_t1d"  = "Date of first occurrence of terms/fields mapped to E10. See Data-field 130706",        
               "date_first_selfrep_dm" = "Date of first diabetes, self reported (field 20002+20008, codes 1220,1222,1223)",
               "selfrep_t1d" = "Whether the participant self reported t1d (field 20002, code 1222)",
               "selfrep_t2d" = "Whether the participant self reported t1d (field 20002, code 1223)",              
               "age_first_selfrep_dm" = "Self-reported age of first diabetes",
               "date_first_known_CVD" = "Date first known CVD (MI, PCI, Stroke, Unstable Angina, CVD death)",
               "term_first_known_CVD" = "Term or data field(f.*) corresponding to first known CVD",    
               "source_first_known_CVD" = "Source of term/field corresponding to first known CVD",
               "date_first_gp" = "Date of first record in gp_clinical table",
               "date_last_gp" = "Date of last record in gp_clinical table",            
               "date_first_hosp" = "Date of first record in hesin table (hospital admissions)",
               "date_last_hosp" = "Date of last record in hesin table (hospital admissions)", 
               "date_first_assessment" = "Date of first UKB assessment center visit (field 53)",   
               "date_last_assessment" = "Date of last UKB assessment center visit (field 53)",
               "FLAG_miss_selfrep_date_DM" = "Whether the participant is misssing a self-reported date of first diabetes diagnosis.",
               "FLAG_gp_2yr_pre_DM" = "Whether the particpant has primary care records (gp_clinical) in both of the 2 calendar years preceding their first evidence of diabetes",
               "FLAG_date_first_DM_known" = "Whether or not we consider the date of diabetes onset to be 'known'. I.e either self-reported or we have records from the 2 preceding years of primary care data",
               "date_death"   = "Date of death according to death record (field 40000)",
               "dm_on_deathcert"  = "Whether diabetes was listed on the death certificate (primary or secondary COD)",        
               "years_DM_to_CVD_point_est" = "Point estimate for years from diabetes onset to CVD",
               "first_linkage" = "Date of first record in either gp_clinical or hesin (primary care or hospital admissions).",
               "last_linkage" =  "Date of laste record in either gp_clinical or hesin (primary care or hospital admissions).",           
               "last_contact" = "Maximum of last_linkage and date_last_assessment ",
               "first_contact" = "Minimum of first_linkage and date_first_assessment ",
               "CVD" = "Whether the patient developed CVD  (MI, PCI, Stroke, Unstable Angina, CVD death)",                     
               "died"  = "Whether the patient died",
               "evidence_t1d" = "Whether there is any indication of T1D specifically (self-report, first occurrence, or primary care terms)",
               "evidence_t2d" = "Whether there is any indication of T2D specifically (self-report, first occurrence, or primary care terms)" ,         
               "dm_type" = "Type of diabetes.  Only evidence of type I, only evidence of type II, evidence of both, or evidence of diabetes without any evidence of which type.",
               "CVD_L_i" = "Lower bound of time in years from first DM to first CVD",
               "CVD_R_i" = "Upper bound of time in years from first DM to first CVD",                 
               "censor_type_CVD" = "What type of censoring is this case (Right, Interval, or Observed)",
               "CVD_timeframe" = "Years between upper and lower bounds of time-to-event for CVD")

data_dict <- data.frame(variable = names(dm_all)) %>%
  mutate(description = dict_key[variable])

fwrite(dm_all, "CensoringProject_CVD.txt", sep = "\t", row.names=F)
write.xlsx(data_dict, "CensoringProject_CVD_data_dictionary.xslx")
fwrite(data_dict, "CensoringProject_CVD_data_dictionary.txt", row.names=F, sep = "\t")

