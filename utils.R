get_session_from_survey_name <- function(survey_name){
  session_id <- as.integer(stringr::str_match(survey_name,"DEV Session (\\d) Surveys")[,2])
  #now some custom code for the oddd ones out.
  session_id[survey_name=="DEV ADOPT Survey Flow"]<-0
  session_id[survey_name=="DEV Session 0 Food Categories & Demographics"]<-0
  
  return(session_id)
  
  
}


do_misc_self_report_summary_measures <-function(data_by_ppt){
  data_by_ppt$SRHI_healthy_minus_unhealthy <- data_by_ppt$SRHI_healthy-data_by_ppt$SRHI_unhealthy
  data_by_ppt$RTFS_f1_minus_f2 <- data_by_ppt$RTFS_factor_1-data_by_ppt$RTFS_factor_2
  #Craig CL,Marshall A , Sjostrom M et al. International Physical Activity Questionnaire: 12 country reliability and validity Med Sci Sports Exerc 2003;August
  #also available on the IPAQ scoring protocol in the DEV Dropbox, page 5

  
  #Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/Measures/Measures_Questionnaires/IPAQ Scoring Protocol.pdf
  data_by_ppt$IPAQ_walkingMETminutes <- data_by_ppt$IPAQ_walkingminutes* 3.3
  data_by_ppt$IPAQ_moderateMETminutes <- data_by_ppt$IPAQ_moderateminutes* 4.0
  data_by_ppt$IPAQ_vigorousMETminutes <- data_by_ppt$IPAQ_vigorousminutes* 8.0
  data_by_ppt$IPAQ_total_METminutes <-data_by_ppt$IPAQ_walkingMETminutes + data_by_ppt$IPAQ_moderateMETminutes+ data_by_ppt$IPAQ_vigorousMETminutes
  if ("weight_0" %in% colnames(data_by_ppt)){
    weight_in_kg <- data_by_ppt$weight_0* 0.453592 #convert from pounds to kg
    data_by_ppt$IPAQ_MET_kCal <- data_by_ppt$IPAQ_total_METminutes * weight_in_kg/60
    
  }else{
    weight_in_kg <- data_by_ppt$weight* 0.453592 #convert from pounds to kg
    data_by_ppt$IPAQ_MET_kCal <- data_by_ppt$IPAQ_total_METminutes * weight_in_kg/60
  }
  
  
  # data_by_ppt$birthsex_factor<-""
  # data_by_ppt$birthsex_factor[data_by_ppt$birthsex==1]<-"Female"
  # data_by_ppt$birthsex_factor[data_by_ppt$birthsex==2]<-"Male"
  # data_by_ppt$birthsex_factor<-factor(data_by_ppt$birthsex_factor,levels=c("Female","Male"))
  if("birthsex" %in% colnames(data_by_ppt)){
    data_by_ppt <- data_by_ppt %>% mutate(
      birthsex_factor =
        factor(case_when(
          birthsex==1 ~ "Female",
          birthsex==2 ~ "Male"
        ),levels=c("Female","Male"))
    )
    
  }
  
  return(data_by_ppt)
}

do_aggregate_measures <- function(data_by_ppt){
  data_by_ppt$PLAN_mean <- rowMeans(data_by_ppt[,c("PLAN_cognitive_strategies","PLAN_mental_flexibility","PLAN_temporal_orientation")])
  data_by_ppt$Planning_aggregate <- rowMeans(data_by_ppt[,c("PLAN_mean","BSCS")],na.rm = FALSE)
  data_by_ppt$Restraint_aggregate<- rowMeans(data_by_ppt[,c("RS","TRSQ","RMQ_assessment")],na.rm = FALSE)
  data_by_ppt$IMI_effort_importance_aggregate <- rowMeans(data_by_ppt[,c("IMI_effort_importance", "IMI_interest_enjoyment")],na.rm = TRUE)
  
  return(data_by_ppt)
}

get_multiscale_values <- function(scored){
  #Now do multi-scale predictors
  #Big5, Planfulness
  multi_scale_predictor_list <- list(BFI =c("agreeableness","conscientiousness","extraversion","neuroticism","openness"),
                                     PLAN = c("cognitive_strategies","mental_flexibility","temporal_orientation"),
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
                                     TESQ_E = unique(scored[scored$scale_name=="TESQ_E","scored_scale"])
  )
  
  #go through the table and label every subscale that we want to keep
  scored$INCLUDE_SUBSCALE <- FALSE
  for (scale_x in names(multi_scale_predictor_list)){
    print(scale_x)
    for(subscale_x in multi_scale_predictor_list[[scale_x]]){
      cat(subscale_x)
      scored[scored$scale_name==scale_x & scored$scored_scale==subscale_x,"INCLUDE_SUBSCALE"] <- TRUE
    }
  }
  
  scored_multi <- scored[scored$INCLUDE_SUBSCALE,]
  
  scored_multi$scale_subscale_name <- paste0(scored_multi$scale_name,"_",scored_multi$scored_scale)
  
  multi_scale_values <- scored_multi %>% 
    #throw it out wide
    select(SID,survey_name,score,scale_subscale_name) %>% tidyr::pivot_wider(names_from = scale_subscale_name,values_from = score)
  
  return(multi_scale_values)
}


get_promote_minus_prevent <- function(scored){
  
  #iterate through each of our scales, and test their correlation with FCI and FFQ
  
  #scales where there's a promote-prevent
  promote_minus_prevent <- scored %>% 
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

get_single_scale_predictors <- function(scored){
  
  single_scale_predictors_mean <- c("BSCS","EDM","cSES")
  
  single_scale_values_mean <- scored %>% 
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
  
  single_scale_predictors_sum <- c("TRSQ","BIS_11","RS","PCS")
  
  single_scale_values_sum <- scored %>% 
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