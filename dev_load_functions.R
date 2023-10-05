library(magrittr)
library(dplyr)
library(stringr)
library(rstatix)
library(data.table)
library(dplyr)

get_scored_fs <- function(dropbox_file_dir){
  output_file_dir = "~/Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/analysis_files/data/"
  load(paste0(output_file_dir,"/scored_data_w_demographics_for_healthy_env_analysis.RData"))
  data_path <- "../../../../files/data/"
  
  
  scored<-scored_with_demographics
  rm(scored_with_demographics)
  scored$score <- as.numeric(scored$score)
  scored$scale_name <- sub("-","_",scored$scale_name)
  
  
  
  #mark the first survey of each scale
  scored$survey_num <- as.numeric(str_trim(str_match(scored$survey_name,"[DEV Session \\s][\\d+][\\sSurveys]")))
  scored$survey_num[scored$survey_name=="DEV ADOPT Survey Flow"]<-0.5
  first_survey_for_scale<-scored %>% group_by(scale_name) %>% dplyr::summarise("first_survey"=min(survey_num))
  
  #then select it.
  scored_fs <- scored %>%
    merge(first_survey_for_scale,by="scale_name") %>%
    filter(survey_num==first_survey)
  
  return(scored_fs)
}




get_preprocessed_redcap_data <- function(){
  
  #Now get BMI from redcap
  stop("the file below is out of date; need to use a more recent file")
  
  load("../../../../files/data/ppt_list_w_data_2022_02.Rdata")
  
  ppt_list_w_data$date_0<-as.Date(ppt_list_w_data$date_0)
  ppt_list_w_data$dob<-as.Date(ppt_list_w_data$dob)
  ppt_list_w_data$age365<- as.numeric((ppt_list_w_data$date_0-ppt_list_w_data$dob)/365)
  race_cols <- colnames(ppt_list_w_data)[grepl("race__",colnames(ppt_list_w_data))]
  race_data <- ppt_list_w_data[,..race_cols]
  ppt_list_w_data$race<-apply(race_data, 1, function(x) {
    paste(sub("race___","r",names(which(x == 1))),collapse=", ")
  })
  
  #now tidy the race variable by making the largest one (white) the default, and 
  #combining all rare items
  race_items = table(ppt_list_w_data$race)
  majority_race <- names(race_items)[max(race_items)==race_items]
  small_minority_race <- names(race_items)[race_items<20]
  ppt_list_w_data$race[ppt_list_w_data$race %in% small_minority_race] <-"Other/mixed"
  other_race <- unique(ppt_list_w_data$race)[unique(ppt_list_w_data$race)!=majority_race]
  ppt_list_w_data$RaceCategorical <- factor(ppt_list_w_data$race,levels = c(majority_race,other_race))
  
  #our primary health outcome measure
  print("colnames(ppt_list_w_data)")
  print(colnames(ppt_list_w_data))
  ppt_health_outcomes <- ppt_list_w_data %>% dplyr::select(dev_id,bf_1,weight_0,height_0,birthsex,age365,bmi_0,bmi_1,RaceCategorical) %>% filter(!is.na(bf_1))
  
  ppt_health_outcomes$bmi <- ifelse(!is.na(ppt_health_outcomes$bmi_0),ppt_health_outcomes$bmi_0,ppt_health_outcomes$bmi_1)
  rm(ppt_list_w_data)
  
  #remove data of implausible bf_1 values
  ppt_health_outcomes$bf_1[ppt_health_outcomes$bf_1<13]<-NA
  
  
  bf_model <- lm(bf_1~birthsex+age365, ppt_health_outcomes)
  summary(bf_model)
  #ppt_health_outcomes$bf_1_controlled <-NULL <- bf_model$residuals
  ppt_health_outcomes$bf_1_controlled[as.numeric(rownames(bf_model$model))] <- bf_model$residuals
  
  #simple model just zscoring within sex
  #that allows for a more inuitive interpretation than errors from a linear model.
  ppt_health_outcomes %<>% group_by(birthsex) %>% mutate(
    bf_1_bsexnormedzs = (bf_1-mean(bf_1,na.rm = TRUE))/sd(bf_1,na.rm = TRUE),
    bmi_bsexnormedzs = (bmi-mean(bmi,na.rm = TRUE))/sd(bmi,na.rm = TRUE)
    
  )
  
  return(ppt_health_outcomes)
}



get_single_scaled_values <- function(scored_fs){
  
  single_scale_predictors_mean <- c("BSCS","EDM","cSES")
  
  single_scale_values_mean <- scored_fs %>% 
    #filter for FFQ
    filter(scale_name %in% single_scale_predictors_mean) %>%
    #filter for the two cols we're interested in
    #filter(scored_scale %in% c("mean")) %>%#unnecessary
    #throw it out wide
    select(SID,scored_scale,score,survey_name,scale_name) %>% tidyr::pivot_wider(names_from = scored_scale,values_from = score) %>%
    #wide across the surveys so that we can compare the surveys
    select(scale_name,survey_name,SID,mean) %>% 
    tidyr::pivot_wider(id_cols=c("SID","survey_name"),
                       names_from=scale_name,
                       values_from=c("mean")
    )
  
  
  
  
  
  single_scale_predictors_sum <- c("TRSQ","BIS_11","RS","PCS","PSS")
  
  single_scale_values_sum <- scored_fs %>% 
    #filter for FFQ
    filter(scale_name %in% single_scale_predictors_sum) %>%
    #throw it out wide
    select(SID,scored_scale,score,survey_name,scale_name) %>% tidyr::pivot_wider(names_from = scored_scale,values_from = score) %>%
    #wide across the surveys so that we can compare the surveys
    select(scale_name,survey_name,SID,sum) %>% 
    tidyr::pivot_wider(id_cols=c("SID","survey_name"),
                       names_from=scale_name,
                       values_from=c("sum")
    )
  single_scale_values<- merge(single_scale_values_mean,single_scale_values_sum,by=c("SID","survey_name"),all=TRUE)
  return(single_scale_values)
}



get_promote_minus_prevent <- function(scored_fs){
  
  #iterate through each of our scales, and test their correlation with FCI and FFQ
  
  #scales where there's a promote-prevent
  promote_minus_prevent <- scored_fs %>% 
    #filter for FFQ
    filter(scale_name %in% c("FFQ","FCI")) %>%
    #filter for the two cols we're interested in
    filter(scored_scale %in% c(
      "cancer_promoting","cancer_preventing","craved_cancer_promoting","liked_cancer_promoting","craved_cancer_preventing","liked_cancer_preventing")) %>%
    #throw it out wide
    select(SID,scored_scale,score,survey_name,scale_name) %>% tidyr::pivot_wider(names_from = scored_scale,values_from = score) %>%
    #create a composite measure
    mutate("cancer_promoting_minus_preventing"=cancer_promoting-cancer_preventing,
           "cancer_promoting_minus_preventing_craved"=craved_cancer_promoting-craved_cancer_preventing,
           "cancer_promoting_minus_preventing_liked"=liked_cancer_promoting-liked_cancer_preventing
    ) %>%
    #wide across the surveys so that we can compare the surveys
    select(scale_name,survey_name,SID,cancer_promoting_minus_preventing,
           cancer_promoting_minus_preventing_craved,cancer_promoting_minus_preventing_liked,
           cancer_promoting,cancer_preventing
    ) %>% 
    tidyr::pivot_wider(id_cols=c("SID","survey_name"),
                       names_from=scale_name,
                       values_from=c(
                         "cancer_promoting_minus_preventing",
                         "cancer_promoting_minus_preventing_craved",
                         "cancer_promoting_minus_preventing_liked",
                         "cancer_promoting","cancer_preventing"
                       )
    )
  return(promote_minus_prevent)
}


get_ses_summary_stats <- function(scored_fs){
  scored_fs %>% filter(scale_name=="DEMO") %>% .$scored_scale %>% unique
  
  scored_fs_demo_zscored <- scored_fs %>% filter(scale_name=="DEMO")
  ses_grouped<-c("zipcode_median_income_acs", "household_income_per_person","household_income_level_medamount","education_own")
  
  scored_fs_demo_zscored <- scored_fs_demo_zscored %>% data.frame %>% group_by(scored_scale) %>% 
    filter(scored_scale %in% ses_grouped) %>% 
    mutate(
      score_zscore=(score-mean(score,na.rm=TRUE))/sd(score,na.rm=TRUE),
      score_mean = mean(score,na.rm=TRUE),
      score_sd =sd(score,na.rm=TRUE)) %>% ungroup
  #get a zscore of each
  
  scored_fs_demo_zscored$score<- scored_fs_demo_zscored$score_zscore
  
  scored_fs_demo_zscored_wide <- scored_fs_demo_zscored %>% 
    select(SID,survey_name,score,scored_scale) %>% tidyr::pivot_wider(names_from = scored_scale,values_from = score) %>%
    rowwise() %>%
    mutate(ses_aggregate=mean(c(education_own,zipcode_median_income_acs,household_income_per_person,household_income_level_medamount)))
  
  return(scored_fs_demo_zscored_wide)
}

get_multiscale_values <- function(scored_fs){
  
  #Now do multi-scale predictors
  #Big5, Planfulness
  multi_scale_predictor_list <- list(BFI =c("agreeableness","conscientiousness","extraversion","neuroticism","openness"),
                                     PLAN = c("cognitive_strategies","mental_forecasting","temporal_orientation"),
                                     ACES = c("sum","abuse","divorced_separated","household_dysfunction","neglectful_parenting"),
                                     RTFS = c("factor_1", "factor_2"),
                                     SRHI = c("healthy","unhealthy","sum"),
                                     DEMO = c("mcarthur_social_standing"),
                                     IPAQ = c("sittinghours", "moderateminutes", "vigorousminutes", "walkingminutes"),
                                     RMQ = c("assessment", "lie",        "locomotion"),
                                     IMI = c("value_usefulness", "perceived_choice", "perceived_competence", "effort_importance", "interest_enjoyment"),
                                     NCS = c("get_job_done","intellectual_task", "abstract_thinking", "like_responsibility", "thinking_not_exciting",
                                             "think_minimally", "thinking_not_fun", "avoid_depth", "deliberating_issues", "prefer_little_thought",
                                             "relief_not_satisfaction", "satisfaction_in_deliberating", "thought_appealing", "total", "tasks_little_thought",
                                             "solve_puzzles", "small_daily_projects", "new_solutions_to_problems", "prefer_complex"
                                     ),
                                     TESQ_E = unique(scored_fs[scored_fs$scale_name=="TESQ_E","scored_scale"]),
                                     MESA = c("freshproduce", "healthyfoodavailability", "lowfat", "mean", "neighborhoodconnectedness", "safetyfromcrime"),
                                     FFQ = c('cancer_preventing', 'cancer_promoting', 'dairy', 'empty_calories', 'fried_food', 'fruits', 'grains', 'meat', 'processed_meats', 'protein', 'red_meat', 'soups', 'sugary_foods', 'vegetables')
  )
  
  #go through the table and label every subscale that we want to keep
  scored_fs$INCLUDE_SUBSCALE <- FALSE
  for (scale_x in names(multi_scale_predictor_list)){
    print(scale_x)
    for(subscale_x in multi_scale_predictor_list[[scale_x]]){
      cat(subscale_x)
      scored_fs[scored_fs$scale_name==scale_x & scored_fs$scored_scale==subscale_x,"INCLUDE_SUBSCALE"] <- TRUE
    }
  }
  
  scored_multi <- scored_fs[scored_fs$INCLUDE_SUBSCALE,]
  
  scored_multi$scale_subscale_name <- paste0(scored_multi$scale_name,"_",scored_multi$scored_scale)
  
  multi_scale_values <- scored_multi %>% 
    #throw it out wide
    select(SID,survey_name,score,scale_subscale_name) %>% tidyr::pivot_wider(names_from = scale_subscale_name,values_from = score)
  
  return(multi_scale_values)
}


get_data_by_ppt <- function(ppt_data_wide){
  
  #grabs the FIRST score for each item per participant.
  col_list <- lapply(colnames(ppt_data_wide %>% select(-survey_name,-SID)), function(col){
    return(ppt_data_wide %>% select(-survey_name) %>% .[,c("SID",col)] %>% filter(!is.na(.[col])) %>% group_by(SID) %>% summarise("{col}":=first(.data[[col]])))
  })
  data_by_ppt <- purrr::reduce(col_list, left_join,by="SID")
  return(data_by_ppt)
}

#I don't know why these are together. just run with it.
data_by_ppt_preprocessing <- function(data_by_ppt){
  
  
  #Craig CL,Marshall A , Sjostrom M et al. International Physical Activity Questionnaire: 12 country reliability and validity Med Sci Sports Exerc 2003;August
  #also available on the IPAQ scoring protocol in the DEV Dropbox, page 5
  #Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/Measures/Measures_Questionnaires/IPAQ Scoring Protocol.pdf
  data_by_ppt$IPAQ_walkingMETminutes <- data_by_ppt$IPAQ_walkingminutes* 3.3
  data_by_ppt$IPAQ_moderateMETminutes <- data_by_ppt$IPAQ_moderateminutes* 4.0
  data_by_ppt$IPAQ_vigorousMETminutes <- data_by_ppt$IPAQ_vigorousminutes* 8.0
  data_by_ppt$IPAQ_total_METminutes <-data_by_ppt$IPAQ_walkingMETminutes + data_by_ppt$IPAQ_moderateMETminutes+ data_by_ppt$IPAQ_vigorousMETminutes
  weight_in_kg <- data_by_ppt$weight_0* 0.453592 #convert from pounds to kg
  data_by_ppt$IPAQ_MET_kCal <- data_by_ppt$IPAQ_total_METminutes * weight_in_kg/60
  
  
  # data_by_ppt$birthsex_factor<-""
  # data_by_ppt$birthsex_factor[data_by_ppt$birthsex==1]<-"Female"
  # data_by_ppt$birthsex_factor[data_by_ppt$birthsex==2]<-"Male"
  # data_by_ppt$birthsex_factor<-factor(data_by_ppt$birthsex_factor,levels=c("Female","Male"))
  
  data_by_ppt <- data_by_ppt %>% mutate(
    birthsex_factor =
      factor(case_when(
        birthsex==1 ~ "Female",
        birthsex==2 ~ "Male"
      ),levels=c("Female","Male"))
  )
  
  zscore <- function(x){
    return((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
  }
  data_by_ppt$FFQ_fruit_and_vege <- zscore(data_by_ppt$FFQ_fruits)+zscore(data_by_ppt$FFQ_vegetables)/2
  
  return(data_by_ppt)
}


print_outliers <- function(data_by_ppt){
  for (coln in colnames(data_by_ppt)[2:ncol(data_by_ppt)]){
    class(data_by_ppt[[coln]])
    if(sum(is.na(data_by_ppt[[coln]]))<length(data_by_ppt[[coln]])){
      
      na_row <- is.na(unlist(data_by_ppt[coln]))
      col_vals <- unlist(data_by_ppt[!na_row,coln])
      if(class(col_vals)!="factor"){
        col_sids <- data_by_ppt[!na_row,"SID"]
        col_mean<-mean(col_vals)
        col_sd<-sd(col_vals)
        outliers_4sigma <- (col_vals < col_mean - col_sd * 4) | (col_vals > col_mean + col_sd * 4)
        outliers_5sigma <- (col_vals < col_mean - col_sd * 5) | (col_vals > col_mean + col_sd * 5)
        if(sum(outliers_4sigma)+sum(outliers_5sigma)>0){
          print(paste(coln, ": 4 sigma outliers are:",paste(data_by_ppt[outliers_4sigma,"SID"],collapse = ","), "; 5 sigma outliers are:",paste(data_by_ppt[outliers_5sigma,"SID"],collapse = ",")))
        }      
      }
    }
  }
}

#should extend this but this is all we have for now.
remove_outliers <- function(data_by_ppt){
  #remove implausible TRSQ score
  data_by_ppt$TRSQ[data_by_ppt$TRSQ<5]<-NA
  return(data_by_ppt)
}



get_complete_data_by_ppt <- function(dropbox_file_dir){
  
  scored_fs <- get_scored_fs(dropbox_file_dir)
  ppt_health_outcomes<-get_preprocessed_redcap_data()
  single_scale_values<-get_single_scaled_values(scored_fs)
  promote_minus_prevent<-get_promote_minus_prevent(scored_fs)
  scored_fs_demo_zscored_wide<-get_ses_summary_stats(scored_fs)
  multi_scale_values <- get_multiscale_values(scored_fs)
  
  #now merge these
  scales_wide<-merge(promote_minus_prevent,single_scale_values,by=c("SID","survey_name"),all=TRUE)
  ppt_data_wide_raw_1 <- merge(scales_wide,ppt_health_outcomes,by.x="SID",by.y="dev_id",all=TRUE)
  ppt_data_wide_raw_2 <- merge(ppt_data_wide_raw_1,multi_scale_values,by=c("SID","survey_name"),all=TRUE)
  ppt_data_wide <- merge(ppt_data_wide_raw_2,scored_fs_demo_zscored_wide,by=c("SID","survey_name"),all=TRUE)
  
  data_by_ppt <- get_data_by_ppt(ppt_data_wide)
  data_by_ppt <- data_by_ppt_preprocessing(data_by_ppt)
  
  
  load(paste0(dropbox_file_dir,"/raw_survey_data.RData"))
  #(2)  list of birthsex. this is done easily enough with:
  birthsex_key <- data_by_ppt[c("SID","birthsex_factor")]
  source("../ffq_revision/get_ffq_v2.R")
  
  ffq_v2_values <- score_ffqv2(surveys_long_clean,birthsex_key)
  colnames(ffq_v2_values) <- paste0("FFQ_v2_",colnames(ffq_v2_values))
  
  
  
  data_by_ppt <- merge(data_by_ppt,ffq_v2_values,by.x="SID",by.y="FFQ_v2_SID",all.x = TRUE,all.y=FALSE)
  
  print_outliers(data_by_ppt)
  
  data_by_ppt <- remove_outliers(data_by_ppt)
}


