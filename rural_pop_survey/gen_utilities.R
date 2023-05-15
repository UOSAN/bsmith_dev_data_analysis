

summarize_calculated_effect <- function(effect_vector){
  effect_mean <- mean(effect_vector)
  effect_se <- sd(effect_vector)
  effect_ci_lower <- quantile(effect_vector,0.025)
  effect_ci_upper <- quantile(effect_vector,0.975)
  #the z-value here is exactly what lavaan displays.
  #it's going to be a bit different
  #because the mean and se I get are both a bit different for reasons that are unclear to me.
  effect_z_value <- effect_mean/effect_se
  effect_p_value <- get_two_tailed_score(effect_z_value)
  
  cat(paste0("Estimate=",signif(effect_mean,4),
            " [",signif(effect_ci_lower,4),", ", signif(effect_ci_upper,4),"]",
            ", Std. Err=",signif(effect_se,4),
            ", z-value=",signif(effect_z_value,4), 
            ", p-value=",signif(effect_p_value,4)))
  cat("\n")
  return_df <- data.frame(
    "est"=effect_mean,
    "se"=effect_se,
    "pvalue"=effect_p_value,
    "ci_lower" = effect_ci_lower,
    "ci_upper" = effect_ci_upper
  )
  #print(return_df)
  return(return_df)
  #print(t.test(hybrid_indirect_effect))
  #cat("\n")
}
