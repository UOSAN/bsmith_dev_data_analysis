neural_behavioral <- readr::read_csv("/Users/benjaminsmith/Google Drive/oregon/grant-writing-workshop/files/data/analysis_v5_test1.csv")


colnames(neural_behavioral)

for (health_outcome_var in c("bf_1"))
for (neural_var in c(
  "linearSVC_cor_planning_CS_CSpred_prob_CorrectStop",
  "SVC_prob_planning_CS_CSpred_prob_CorrectStop",
  "SVC_prob_exec_function_CS_CSpred_prob_CorrectStop",
  "linearSVC_cor_wholebrain_CS_CSpred_prob_CorrectStop",
  "SVC_wholebrain_CS_CSpred_prob_CorrectStop"
)){
  formula <- as.formula(paste0(health_outcome_var," ~ ",neural_var," + SST_SSRT+TESQ_E_sum"))
  print(health_outcome_var)
  print(summary(lm(formula,neural_behavioral)))
}

print(summary(lm(cancer_promoting_minus_preventing_FFQ~SST_SSRT+TESQ_E_sum,neural_behavioral)))

for (health_outcome_var in c("cancer_promoting_minus_preventing_FFQ"))
  for (neural_var in c(
    "SVC_inhib_CS_CSpred_prob_CorrectStop",
    "SVC_IFG_PT_CS_CSpred_prob_CorrectStop"
  )){
    formula <- as.formula(paste0(health_outcome_var," ~ ",neural_var," + SST_SSRT + TESQ_E_sum"))
    print(health_outcome_var)
    print(summary(lm(formula,neural_behavioral)))
  }


