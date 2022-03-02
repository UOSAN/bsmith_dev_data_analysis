library(readr)



score_ffqv2 <- function(
  surveys_long_clean,
  birthsex_key,
  nutrient_density_score_path = "/Users/benjaminsmith/Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/Data/FFQ Nutrient Density Analysis/DEV_FFQ_Nutrient_Density_Scores.csv"){
  FFQ_df <- read_csv(nutrient_density_score_path)
  
  FFQ_df$Energy_Density <- (FFQ_df$ENERGY_KCAL_USDA/FFQ_df$GRAMWT_G_USDA)
  #surveys_long_clean<-scored
  #Filter the data to only view the completed FFQ for baseline session analysis (Session 1 only) 
  FFQ_data_long_clean <- surveys_long_clean %>% filter(Finished==1) %>% #filter(survey_name == c("DEV Session 1 Surveys")) %>% #Only looking at the baseline data (FFQ taken at Session 1)
    #filter(scale_name=="FFQ") %>%
    filter(substr(item,1,3)=="FFQ") %>% 
    mutate(value=as.numeric(value))
  
  #Load in data with gender identifier to reference for nutrient density scores
  # DEV_df_gender <- read.csv(file = "https://www.dropbox.com/s/qcz39xo62f3yahe/DEV%20Gender%20data%20.csv?dl=1", stringsAsFactors = FALSE, header = TRUE, row.names = NULL) %>%
  #   na_if("")
  
  #Merge datasets so that gender can be referenced 
  DEV_df_complete <- merge(FFQ_data_long_clean, birthsex_key, by = "SID")

  
  #Create a new variable that specifies the name of the FFQ item based on gender - and that matches with the item name in the FFQ_df 
  DEV_df_complete <- DEV_df_complete %>% mutate (Item_Sex =
                                                   case_when(
                                                     birthsex_factor == "Male" ~ paste(item, ".1", sep = ""),
                                                     birthsex_factor == "Female" ~ item
                                                   ))

  #Add the nutrient density scores to this dataframe 
  DEV_df_merged <- merge(DEV_df_complete, FFQ_df[,c("Nutrient_Density_Score","ITEM.NAME","percent_daily_value_Nutrient_Density_Score", "ENERGY_KCAL_USDA", "Nutrient_density_numerator","Energy_Density")], by.x="Item_Sex", by.y="ITEM.NAME") 
  
  #FFQ_df$percent_daily_value_Nutrient_Density_Score <- (FFQ_df$percent_daily_value_numerator/FFQ_df$ENERGY_KCAL_USDA)*100

  #Remove FFQ items 75 (Tea), which was problematic in our nutrient density algorithm
  DEV_df_merged <- DEV_df_merged[!(DEV_df_merged$Item_Sex=="FFQ_75" | DEV_df_merged$Item_Sex=="FFQ_75.1"),]

  #Create weighted nutrient density scores 
  DEV_df_merged$Weighted_Nutrient_Density <- DEV_df_merged$Nutrient_Density_Score*DEV_df_merged$value
  
  DEV_df_merged$Weighted_Percent_Nutrient_DV <- DEV_df_merged$percent_daily_value_Nutrient_Density_Score*DEV_df_merged$value
  
  #for dietary nutrient density
  DEV_df_merged$Weighted_Nutrient_density_numerator <- (DEV_df_merged$Nutrient_density_numerator*DEV_df_merged$value)
  DEV_df_merged$Weighted_ENERGY_KCAL_USDA <- (DEV_df_merged$ENERGY_KCAL_USDA*DEV_df_merged$value)

  #energy density = kCal/g, then weight by frequency
  DEV_df_merged$Weighted_Energy_Density <- (DEV_df_merged$Energy_Density*DEV_df_merged$value)
  
  #Calculate each participants Average Nutrient Density over the past two weeks. 
  DEV_Weighted_Nutrient_Density_Scores <- DEV_df_merged %>% 
    group_by(SID)%>%
    summarise(Mean_Weighted_Nutrient_Density = mean(Weighted_Nutrient_Density, na.rm = T),
              Mean_Weighted_percent_daily_value = mean(Weighted_Percent_Nutrient_DV, na.rm = T),
              Mean_Dietary_Nutrient_Density = mean(Weighted_Nutrient_density_numerator, na.rm = T)/mean(Weighted_ENERGY_KCAL_USDA,na.rm=TRUE)*100,
              Mean_Energy_Density = mean(Weighted_Energy_Density,na.rm=TRUE),
              Mean_Energy = mean(Weighted_ENERGY_KCAL_USDA,na.rm=TRUE)
              )
  

  
  #weighted diet score
  DEV_df_merged$Weighted_Diet_Nutrient_Density <- 
  return(DEV_Weighted_Nutrient_Density_Scores)
}
# #Get Descriptive Statistics for sample population 
# psych::describe(DEV_Weighted_Nutrient_Density_Scores$Mean_Weighted_percent_daily_value)
# psych::describe(DEV_Weighted_Nutrient_Density_Scores$Mean_Weighted_Nutrient_Density)
# ```
# 
# 
# #Export csv of participant scores 
# ```{r}
# 
# ```
# 
# 
# }
