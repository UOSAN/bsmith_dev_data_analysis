behavioral_neural_mappings <- list(
  "RTFS_factor_1" = list("CG_p_CS_wholebrain_pred_prob_CorrectGoFollowingCorrectStop","CG_p_FS_Frontal_Orbital_Cortex_pred_prob_CorrectGoFollowingFailedStop"),
  "RTFS_factor_2" = list("CG_p_FS_emotion_regulation_pred_prob_CorrectGoFollowingFailedStop"),
  "RTFS_f1_minus_f2" = list("CG_p_CS_wholebrain_pred_prob_CorrectGoFollowingCorrectStop", "CG_p_FS_wholebrain_pred_prob_CorrectGoFollowingFailedStop")
)




for (dataset_filepath in c(
  "/Users/benjaminsmith/Google Drive/oregon/grant-writing-workshop/files/data/analysis_v5_pes_test1.csv",
  "/Users/benjaminsmith/Google Drive/oregon/grant-writing-workshop/files/data/analysis_v5_pes_test1_validation.csv"
)){
  print(dataset_filepath)
  neural_behavioral <- readr::read_csv(dataset_filepath)
  
  #print(colnames(neural_behavioral))
  colnames(neural_behavioral)<-stringr::str_replace_all(colnames(neural_behavioral)," ","_")
  
  
  for (health_outcome_var in c("RTFS_factor_1","RTFS_factor_2","RTFS_f1_minus_f2"))
    for (neural_var in c(
      behavioral_neural_mappings[[health_outcome_var]]
    )){
      print(health_outcome_var)
      formula <- as.formula(paste0(health_outcome_var," ~ ",neural_var,""))
      print(summary(lm(formula,neural_behavioral)))
      formula <- as.formula(paste0(health_outcome_var," ~ ",neural_var," + SST_SSRT+TESQ_E_sum"))
      print(summary(lm(formula,neural_behavioral)))
    }
  
  
  
}
