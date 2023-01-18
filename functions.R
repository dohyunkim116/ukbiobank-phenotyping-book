library(tidyverse)

get_biomarker_col_name <- function(x){
  paste0(paste0(rep(c("cv","avg"),each=length(x)),'_'),x)
}

get_hr <- function(est,se){
  hr <- exp(est) %>% formatC(digits = 2, format = "f")
  l <- (exp(est) - 1.96 * se) %>% formatC(digits = 2, format = "f")
  u <- (exp(est) + 1.96 * se) %>% formatC(digits = 2, format = "f")
  paste0(hr,' (',l,'-',u,')')
}

get_formatted_hr <- function(hr,l,u){
  temp <- c(hr,l,u) %>% formatC(digits = 2, format = "f")
  #hr <- hr %>% formatC(digits = 2, format = "f")
  #l <- l %>% formatC(digits = 2, format = "f")
  #u <- u %>% formatC(digits = 2, format = "f")
  paste0(temp[1],' (',temp[2],'-',temp[3],')')  
}

get_formatted_pval <- function(p){
  if(p < 0.001){
    return("<0.001")
  }
  round(p,3) %>% as.character()
}



reformat_labels_auc_tab <- function(tab_long){
  tab_long <- tab_long %>% mutate(outcome = 
                                    ifelse(outcome %in% c("CVD","DKD","DR","PCI"), outcome,
                                           case_when(
                                             outcome == "Hemorrhagic stroke" ~ "HS",
                                             outcome == "Ischemic Stroke" ~ "IS",
                                             outcome == "Myocardial infraction" ~ "MI",
                                             outcome == "Stroke" ~ "ST",
                                             outcome == "Unstable angina" ~ "UA")
                                    )
  )
  term_vec <- tab_long[["term"]]
  term_vec <- as.factor(term_vec)
  term_levels <- levels(term_vec)
  term_levels[term_levels == "SEX"] <- "Sex"
  term_levels[term_levels == "age_init"] <- "Age"
  term_levels[term_levels == "BMI"] <- "BMI"
  term_levels[term_levels == "smoke_ever"] <- "Smoked"
  term_levels[term_levels == "CCI"] <- "CCI"
  term_levels[term_levels == "MET_hours"] <- "MET"
  term_levels[term_levels == "ISCED_level_gt2"] <- "ISCED"
  term_levels[term_levels == "PGS000024"] <- "GRS1"
  term_levels[term_levels == "PGS000036"] <- "GRS2"
  term_levels[term_levels == "ethnic_selfAsian"] <- "Asian"
  term_levels[term_levels == "ethnic_selfBlack"] <- "Black"
  term_levels[term_levels == "ethnic_selfOther"] <- "Other"
  term_levels[term_levels == "self_insulin"] <- "Insulin"
  term_levels[term_levels == "self_bpdrugs"] <- "BP Med."
  term_levels[term_levels == "self_cholesteroldrugs"] <- "Chol. Med."
}

change_outcome_names <- function(x){
  tibble(outcome = as.character(x)) %>% mutate(outcome = 
                                   ifelse(outcome %in% c("CVD","DKD","DR","PCI"), outcome,
                                          case_when(
                                            outcome == "Hemorrhagic stroke" ~ "HS",
                                            outcome == "Ischemic Stroke" ~ "IS",
                                            outcome == "Myocardial infraction" ~ "MI",
                                            outcome == "Stroke" ~ "ST",
                                            outcome == "Unstable angina" ~ "UA")
                                   )) %>%
    as_vector()
}

convert <- function(biomarker_term){
  if(biomarker_term == "slope_log_uacr"){
    sum_stat_name <- "Slope"
    biomarker_name <- "logUACR"
    return(paste(c(sum_stat_name,biomarker_name),collapse = ' '))
  }
  
  temp <- str_split(biomarker_term,'_')[[1]]
  sum_stat <- temp[1]
  biomarker <- temp[2]
  
  if(sum_stat == "avg"){
    sum_stat_name <- "Mean"
  }
  else if(sum_stat == "cv"){
    sum_stat_name <- "CV"
  }
  else if(sum_stat == "slope"){
    sum_stat_name <- "Slope"
  }
  else if(sum_stat == "int"){
    sum_stat_name <- "Intercept"
  }
  
  if(biomarker == "sbp"){
    biomarker_name <- "SBP"
  }
  else if(biomarker == "dbp"){
    biomarker_name <- "DBP"
  }
  else if(biomarker == "chol"){
    biomarker_name <- "Chol."
  }
  else if(biomarker == "trig"){
    biomarker_name <- "Trig."
  }
  else if(biomarker == "hdl"){
    biomarker_name <- "HDLc"
  }
  else if(biomarker == "ldl"){
    biomarker_name <- "LDLc"
  }
  else if(biomarker == "hba1c"){
    biomarker_name <- "HbA1c"
  }
  else if(biomarker == "glucose"){
    biomarker_name <- "Glucose"
  }
  else if(biomarker == "egfr"){
    biomarker_name <- "eGFR"
  }
  else if(biomarker == "uacr"){
    biomarker_name <- "UACR"
  }
  
  paste(c(sum_stat_name,biomarker_name),collapse = ' ')
}

rename_levels <- function(levels){
  levels[levels == "SEX"] <- "Sex"
  levels[levels == "age_init"] <- "Age"
  levels[levels == "BMI"] <- "BMI"
  levels[levels == "smoke_ever"] <- "Ever smoked"
  levels[levels == "CCI"] <- "CCI"
  levels[levels == "MET_hours"] <- "MET"
  levels[levels == "ISCED_level_gt2"] <- "ISCED"
  levels[levels == "PGS000024"] <- "GRS1"
  levels[levels == "PGS000036"] <- "GRS2"
  levels[levels == "ethnic_selfAsian"] <- "Asian"
  levels[levels == "ethnic_selfBlack"] <- "Black"
  levels[levels == "ethnic_selfOther"] <- "Other"
  levels[levels == "self_insulin"] <- "Insulin"
  levels[levels == "self_bpdrugs"] <- "BP Med."
  levels[levels == "self_cholesteroldrugs"] <- "Chol. Med."
  match_vec <- str_detect(levels,"avg|cv|slope|int")
  levels[match_vec] <- 
    lapply(levels[match_vec],function(x){
      convert(x)
    }) %>% unlist()
  levels
}

reformat_labels <- function(term_vec){
  term_levels <- levels(term_vec)
  levels(term_vec) <- rename_levels(term_levels)
  term_vec
}

get_primary_analysis_ft <- function(tab_wide,fontsize_val=6){
  outcome_levels <- 
    names(tab_wide)[-1] %>% 
    str_split('_') %>% 
    lapply(function(x)x[2]) %>% 
    unlist %>% unique
  header_temp <- attr(tab_wide,"header")
  header_label <- c("",rep(c("HR (95% CI)","P"),length(header_temp)),function(x)x)
  #header_label <- c("",rep(c("HR (95% CI)","P"),length(outcome_levels)),function(x)x)
  names(header_label) <- colnames(tab_wide)
    
  flextable(tab_wide) %>% 
    set_header_labels(values = header_label) %>% 
    add_header_row(
      colwidths = c(1,rep(2,length(header_temp))),
      values = c("",header_temp)
    ) %>% 
    font(fontname = "Times New Roman",part = "all") %>%
    fontsize(size = fontsize_val,part = "all") %>%
    bold(j = 1) %>%
    italic(j = c(3,5,7),italic = TRUE, part = "header") %>%
    padding(padding = 2.5, part = "all") %>%
    line_spacing(space = 0.75, part = "body") %>%
    align(i = c(1,2), part = "header", align = "center") %>%
    align(j = c(3,5,7), part = "body", align = "right") %>%
    valign(part = "body",valign = "center") %>%
    set_table_properties(layout = "autofit") %>%
    border_remove() %>%
    hline_top(part = "header",border =  officer::fp_border(width = 1)) %>%
    hline(part = "header",border =  officer::fp_border(width = 1)) %>%
    hline_bottom(part = "body",border =  officer::fp_border(width = 1))
}

get_auc <- function(event,pred){
  temp <- pROC::ci.auc(event~pred) %>% c()
  tibble(auc.l = temp[1], auc=temp[2], auc.u = temp[3])  
}

get_pred_eval_df2_long <- function(validation_dat_list,model,outcome){
  lapply(1:length(validation_dat_list),function(i){
    validation_dat <- validation_dat_list[[i]]
    selected_var <- model$pooled$term %>% as.vector()  
    beta_hat <- model$pooled$estimate %>% as.matrix()
    X <- validation_dat %>% select(selected_var) %>% as.matrix()
    pred_risk <- X%*%beta_hat
    tibble(event = validation_dat_list[[1]]$event, pred_risk = pred_risk, rep = i, outcome = outcome)
  }) %>% bind_rows()
}

plot_roc2 <- function(pred_eval_df2_long,title_val){
  require(plotROC)
  outcome <- pred_eval_df2_long$outcome[1]
  roc_plot <- pred_eval_df2_long %>%
    ggplot(aes(d=event,m=pred_risk,color=as.factor(rep))) +
    geom_roc(n.cuts=5) + xlab("1-Specificity") + ylab("Sensitivity")
  auc_tab <- calc_auc(roc_plot) %>%
    mutate(rep = c("1","2","3","4","5")) %>%
    mutate(AUC = round(AUC,2)) %>%
    mutate(label = paste0(rep,' (',round(AUC,3),')'))
  roc_plot + scale_color_discrete(name = "Replication (AUC)", labels = auc_tab$label) + 
    ggtitle(title_val,subtitle = outcome)
}


get_avg_pred_risk <- function(validation_dat_list, model){
  selected_var <- model$pooled$term %>% as.vector()  
  beta_hat <- model$pooled$estimate %>% as.matrix()
  
  pred_risk_mat <- lapply(validation_dat_list,function(validation_dat){
    X <- validation_dat %>% select(selected_var) %>% as.matrix()
    tibble(f.eid = validation_dat$f.eid, risk_score = X %*% beta_hat)
  }) %>% reduce(left_join, by = "f.eid") %>% select(-f.eid) %>% as.matrix
  rowSums(pred_risk_mat,na.rm = T)/ncol(pred_risk_mat)
  
  #pred_risk_mat <- lapply(validation_dat_list,function(validation_dat){
  #  X <- validation_dat %>% select(selected_var) %>% as.matrix()
  #  return(X%*%beta_hat)
  #}) %>% do.call(cbind,.)
  #rowSums(pred_risk_mat,na.rm = F)/ncol(pred_risk_mat)
}

get_pred_eval_df <- function(validation_dat_list,model,outcome,biomarker=NULL){
  avg_pred_risk <- get_avg_pred_risk(validation_dat_list,model)
  if(is.null(biomarker)){
    return(tibble(event = validation_dat_list[[1]]$event, avg_pred_risk = avg_pred_risk, outcome = as.factor(outcome)))
  }
  return(tibble(event = validation_dat_list[[1]]$event, avg_pred_risk = avg_pred_risk, outcome = outcome, biomarker = as.factor(biomarker)))
}

plot_roc_by_biomarker <- function(pred_eval_df_long){
  require(plotROC)
  roc_plot <- pred_eval_df_long %>%
    ggplot(aes(d=event,m=avg_pred_risk,color=biomarker)) +
    geom_roc(n.cuts=5) + xlab("1-Specificity") + ylab("Sensitivity")
  auc_tab <- calc_auc(roc_plot) %>%
    mutate(biomarker=levels(pred_eval_df_long$biomarker)) %>%
    mutate(AUC = round(AUC,2)) %>%
    mutate(label = paste0(biomarker,' (',round(AUC,3),')'))
  roc_plot + scale_color_discrete(name = "biomarker (AUC)", labels = auc_tab$label)
}

plot_roc_by_outcome <- function(pred_eval_df_long){
  require(plotROC)
  roc_plot <- pred_eval_df_long %>%
    ggplot(aes(d=event,m=avg_pred_risk,color=outcome)) +
    geom_roc(n.cuts=5) + xlab("1-Specificity") + ylab("Sensitivity")
  auc_tab <- calc_auc(roc_plot) %>%
    mutate(outcome=levels(pred_eval_df_long$outcome)) %>%
    mutate(AUC = round(AUC,2)) %>%
    mutate(label = paste0(outcome,' (',round(AUC,3),')'))
  roc_plot + scale_color_discrete(name = "Outcome (AUC)", labels = auc_tab$label)
}

uv_coxph_pool <- function(tte_dat_list,preds_rhs){
  lapply(tte_dat_list,function(dat){
    rhs <- paste(preds_rhs,collapse = '+')
    f <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs))
    coxph(f,dat)      
  }) %>% mice::pool()
}

get_vote <- function(tte_dat_list,preds_rhs,core_covs = NULL){
  iter.max.val <- 50
  lapply(tte_dat_list,function(dat){
    dat_sel <- select(dat,all_of(c("time_to_event","event",preds_rhs)))
    complete_rows <- complete.cases(dat_sel)
    dat_sel <- dat_sel[complete_rows,]
    rhs <- paste(preds_rhs,collapse = '+')
    f <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs))
    res <- do.call("coxph", list(formula=f,data=dat_sel,control = coxph.control(iter.max = iter.max.val)))
    if(is.null(core_covs)){
      labels(step(res,trace=0)[["terms"]])
    }
    else{
      labels(step(res,trace=0,scope = list(lower = as.formula(paste0('~',paste(core_covs,collapse = "+")))))[["terms"]])  
    }
  }) %>% unlist() %>% table()
}




pooled_coxph_step_additive <- function(tte_dat_list,preds_rhs,core_covs = NULL){
  
  vote <- get_vote(tte_dat_list,preds_rhs,core_covs)
  covs_start <- vote[vote > 2] %>% names()
  candidate_covs <- setdiff(covs_start,core_covs)
  iter.max.val <- 50
  
  done <- F  
  while(!(done || length(candidate_covs) == 0)){
    temp <- lapply(candidate_covs,function(cov){
      mod0_list <- 
        lapply(tte_dat_list,function(tte_dat){
          rhs0 <- paste(setdiff(covs_start,cov),collapse = '+')
          f0 <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs0))
          coxph(f0,tte_dat,control = coxph.control(iter.max = iter.max.val))
        })
      mod1_list <- 
        lapply(tte_dat_list,function(tte_dat){
          rhs1 <- paste(covs_start,collapse = '+')
          f1 <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs1))
          coxph(f1,tte_dat,control = coxph.control(iter.max = iter.max.val))
        })
      mitml::testModels(mod1_list,mod0_list)$test[4]
    }) %>% unlist()  
    
    if(any(temp > 0.05)){
      candidate_covs <- candidate_covs[-which.max(temp)]
      covs_start <- c(core_covs,candidate_covs)
    } 
    else {
      done <- T
    }
  }
  return(lapply(tte_dat_list,function(tte_dat){
    rhs <- paste0(covs_start,collapse =  '+')
    f <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs))
    coxph(f,tte_dat,control = coxph.control(iter.max = iter.max.val))
  }) %>% mice::pool())
}

uv_coxph_step <- function(tte_dat_list,main_preds,covs,covs_drugs,strat_drug = NULL,is_use = NULL){
  lapply(tte_dat_list,function(dat){
    preds_model <- c("time_to_event","event",main_preds,covs,covs_drugs,strat_drug)
    dat_sel <- dat %>% select(all_of(preds_model)) %>% na.omit()
    rhs <- paste(c(main_preds,covs,covs_drugs),collapse = '+')
    f <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs))
    if(is.null(strat_drug)){
      res <- do.call("coxph", list(formula=f,data=dat_sel))
    }
    else {
      if(is_use)
        res <- do.call("coxph", list(formula=f,data=dat_sel,subset = which(as.logical(dat_sel[[strat_drug]]))))
      else
        res <- do.call("coxph", list(formula=f,data=dat_sel,subset = which(!as.logical(dat_sel[[strat_drug]]))))
    }
    labels(step(res,trace=0)[["terms"]])
  })
}

get_subset_val <- function(tte_dat,strat_drug,is_use){
  if (is.null(strat_drug)) {
    subset_val <- rep(T,nrow(tte_dat))
  }
  else {
    if(is_use)
      subset_val <- which(as.logical(tte_dat[[strat_drug]]))
    else
      subset_val <- which(!as.logical(tte_dat[[strat_drug]]))
  }
  subset_val
}

pooled_step <- function(tte_dat_list,main_preds,covs,covs_drugs,strat_drug = NULL,is_use = NULL){
  vote <- table(unlist(uv_coxph_step(tte_dat_list,main_preds,covs,covs_drugs,strat_drug,is_use)))
  covs_start <- vote[vote > 2] %>% names()
  
  done <- F  
  while(done){
    temp <- lapply(1:length(covs_start),function(i){
      mod0_list <- 
        lapply(tte_dat_list,function(tte_dat){
          rhs0 <- paste(covs_start[-i],collapse = '+')
          f0 <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs0))
          coxph(f0,tte_dat,subset = get_subset_val(tte_dat,strat_drug,is_use))
        })
      mod1_list <- 
        lapply(tte_dat_list,function(tte_dat){
          rhs1 <- paste(covs_start,collapse = '+')
          f1 <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs1))
          coxph(f1,tte_dat,subset = get_subset_val(tte_dat,strat_drug,is_use))  
        })
      mitml::testModels(mod1_list,mod0_list)$test[4]
    }) %>% unlist()  
    
    if(any(temp > 0.05)){
      covs_start <- covs_start[-which.max(temp)]
    } 
    else {
      done <- T
    }
  }
  return(lapply(tte_dat_list,function(tte_dat){
    rhs <- paste0(covs_start,collapse =  '+')
    f <- as.formula(paste0("Surv(time_to_event,event)",'~',rhs))
    coxph(f,tte_dat,subset = get_subset_val(tte_dat,strat_drug,is_use))  
  }) %>% mice::pool())
}

get_gt_tab <- function(mod,outcome){
  gtsummary::tbl_regression(mod,exponentiate=T) %>%
    gtsummary::modify_spanning_header(c(label,estimate,ci,p.value) ~ outcome) %>%
    gtsummary::bold_labels()
}

# get_biomarker_col_name <- function(x){
#   paste0(paste0(c("cv","avg","intercept","slope"),'_'),rep(x,each=4))  
# }

scale_cv <- function(x){
  str_replace(x,"cv_[a-z|0-9]*",paste0("scale(",x,')'))
}

get_table <- function(tab,outcome,stratified,add_overall){
  
  fields <- c("dummy","ethnic_self","EUR","pcp","SEX","age_init","BMI_male","BMI_female",
              "smoke_ever","packyears_smoker","CCI2",
              "MET_hours",
              "ISCED_level_gt2",
              "dm_type",
              "self_insulin","self_bpdrugs","self_cholesteroldrugs",
              "PGS000024_tertile",
              "PGS000036_tertile",
              "avg_sbp","avg_dbp",
              "avg_chol","avg_trig","avg_hdl","avg_ldl",
              "avg_hba1c","avg_glucose","avg_egfr","avg_log_uacr",
              "n_sbp","n_dbp",
              "n_chol","n_trig","n_hdl","n_ldl",
              "n_hba1c","n_glucose","n_egfr","n_log_uacr","n_all")
  
  label_params <- list(
    dummy ~ "n",
    ethnic_self ~ "Ethnicity (%)",
    EUR ~ "European (%)",
    pcp = pcp ~ "Primary care subjects (%)",
    SEX ~ "Sex (% males)",
    age_init ~ "Age (years)",
    BMI_male ~  "BMI males (kg/m^2)",
    BMI_female ~ "BMI females (kg/m^2)",
    smoke_ever ~ "Ever smoked (%)",
    packyears_smoker ~ "Pack years",
    CCI2 ~ "CCI (%)",
    MET_hours ~ "MET (hours/week)",
    ISCED_level_gt2 ~ "ISCED level > 2 (%)",
    dm_type ~ "DM Type (%)",
    self_insulin ~ "Insulin (%)",
    self_bpdrugs ~ "BP medication (%)",
    self_cholesteroldrugs ~ "Cholesterol medication (%)",
    PGS000024_tertile ~ "T1D GRS (tertile)",
    PGS000036_tertile ~ "T2D GRS (tertile)",
    avg_sbp ~ "SBP (mmHG)",
    avg_dbp ~ "DBP (mmHG)",
    avg_chol ~ "Total cholesterol (mg/dL)",
    avg_trig ~ "Triglycerides (mg/dL)",
    avg_hdl ~ "HDLc (mg/dL)",
    avg_ldl ~ "LDLc (mg/dl)",
    avg_hba1c ~ "HbA1c (%)",
    avg_glucose ~ "Glucose",
    avg_egfr ~ "eGFR",
    avg_log_uacr ~ "logUACR",
    n_sbp ~ "n SBP",
    n_dbp ~ "n DBP",
    n_chol ~ "n Total cholesterol",
    n_trig ~ "n Triglycerides",
    n_hdl ~ "n HDLc",
    n_ldl ~ "n LDLc",
    n_hba1c ~ "n HbA1c",
    n_glucose ~ "n Glucose",
    n_egfr ~ "n eGFR",
    n_log_uacr ~ "n logUACR",
    n_all ~ "n all biomarker"
  )
  
  if(stratified){
    stat_params <- list(
      dummy ~ "{n}",
      c(SEX,smoke_ever,ISCED_level_gt2,EUR,pcp,
        ethnic_self,PGS000024_tertile,PGS000036_tertile,dm_type,CCI2) ~ "{p}",
      starts_with("self_") ~ "{p}"
    )
  }
  else{
    stat_params <- list(
      dummy ~ "{n}",
      c(SEX,smoke_ever,ISCED_level_gt2,EUR,pcp,
        ethnic_self,PGS000024_tertile,PGS000036_tertile,dm_type,CCI2) ~ "{p}",
      starts_with("self_") ~ "{p}"
    )  
  }
  
  digit_params <- list(
    c(age_init,BMI_male,BMI_female,avg_dbp,ethnic_self,dm_type) ~ c(0,0,0),
    c(CCI2) ~ c(0,0,0),
    c(avg_chol,avg_trig,avg_hdl,avg_ldl,avg_glucose,avg_egfr,avg_log_uacr) ~ c(1,1,1)
  )
  
  if(stratified){
    if(add_overall){
      header_params <- list(
        label ~ "",
        stat_0 ~ "Overall",
        stat_1 ~ "No",
        stat_2 ~ "Yes",
        p.value ~ "P"
      )
    }
    else{
      header_params <- list(
        label ~ "",
        stat_1 ~ "No",
        stat_2 ~ "Yes",
        p.value ~ "P"
      )
    }
  }
  else{
    header_params <-
      list(
        label ~ "",
        stat_0 ~ outcome
      )
  } 
  
  if(stratified){
    tab <- tab %>% select(-f.eid) %>% select(all_of(fields),event)# %>% select(-pcp)
  }
  else {
    tab <- tab %>% select(-c(f.eid,event)) %>% select(all_of(fields))
  }
  
  if(stratified){
    #label_params["pcp"] <- NULL
    tab_gt <- tab %>%
      gtsummary::tbl_summary(by = event,
                                 label = label_params,
                                 statistic = stat_params,
                                 digits = digit_params,
                                 missing = "no") %>%
      gtsummary::add_p(pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = 3))
    
    if(add_overall){
      tab_gt <- tab_gt %>% 
        gtsummary::add_overall() %>%
        gtsummary::modify_spanning_header(c("stat_0","stat_1", "stat_2", "p.value") ~ outcome)
    } 
    else{
      tab_gt <- tab_gt %>% 
        gtsummary::modify_spanning_header(c("stat_1", "stat_2", "p.value") ~ outcome)  
    }
    
  }
  else {
      tab_gt <- tab %>%
        gtsummary::tbl_summary(label = label_params,
                               statistic = stat_params,
                               digits = digit_params,
                               missing = "no")
        
  }
  tab_gt %>% 
    gtsummary::modify_header(update = header_params) %>%
    gtsummary::modify_footnote(everything() ~ NA) %>%
    gtsummary::bold_labels()  
}

is_special_dates <- function(dates) {
  dates %in% as.Date(c("1900-01-01", "1901-01-01", "2037-07-07","1902-02-02", "1903-03-03"))
}

cleandates <- function(dates, DOBs) {
  dates[dates %in% as.Date(c("1900-01-01", "1901-01-01", "2037-07-07"))] <- NA 
  dates[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] <- DOBs[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] 
  return(dates)
}

