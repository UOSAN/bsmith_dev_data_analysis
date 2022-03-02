library(foreign)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)

get_cycle5 <- function(location = "/Users/benjaminsmith/Google Drive/oregon/data/protected_HINTS_data/"){
  hines_locations <- Sys.glob(file.path(location,"*/hints5*.sav"))
  
  raw_file_list <- vector(mode="list",length<-length(hines_locations))
  
  for (i in 1:length(raw_file_list)){
    hines_filepath <- hines_locations[[i]]
    filename <- basename(hines_filepath)
    spss_rawfile <- read.spss(hines_filepath, use.value.label=TRUE,to.data.frame=TRUE)
    spss_rawfile[["filename"]] <- filename
    raw_file_list[[i]] <- spss_rawfile
  }
  
  
  detect_key_cols <- function(spss_item){
    print(spss_item[["filename"]][[1]])
    print("Race_Cat2" %in% colnames(spss_item))
    print(paste0("PR_RUCA_2010", "PR_RUCA_2010" %in% colnames(spss_item)))
    print(paste0("RUC2013", "RUC2013" %in% colnames(spss_item)))
    
    print("Age" %in% colnames(spss_item))
    print("BMI" %in% colnames(spss_item))
  }
  
  lapply(raw_file_list,detect_key_cols)
  
  shared_colnames <- colnames(raw_file_list[[1]])
  for (i in 2:length(raw_file_list)){
    shared_colnames <- intersect(colnames(raw_file_list[[i]]),shared_colnames)
  }
  
  #we want a few eextra important columns of interest
  cols_to_use <- c(shared_colnames,"Fruit","Vegetables")
  
  #then trim each file back to just these shared columns.
  #we've checked it will include the ones we want.
  file_list_trimmed <- lapply(
    raw_file_list,
    function(cycle_5_df){
      return(cycle_5_df[intersect(cols_to_use,colnames(cycle_5_df))])
    }
    
  )
  
  hines_cycle5 <- rbindlist(file_list_trimmed,fill=TRUE)
  # 
  # lapply(
  #   file_list_trimmed,
  #   function(cycle_5_df){
  #     print(table(cycle_5_df[["PR_RUCA_2010"]]))
  #     #colnames(cycle_5_df)
  #   }
  #   
  #preprocess
  hines_cycle5$Age <- as.numeric(as.character(hines_cycle5$Age))
  hines_cycle5$BMI <- as.numeric(as.character(hines_cycle5$BMI))
  
  hines_cycle5$Age_c <- hines_cycle5$Age-38 #using U.S. median age

  hines_cycle5$HINESIsRural <- grepl("Metropolitan",hines_cycle5$PR_RUCA_2010)==FALSE
  
  
  hines_cycle5$RacePreprocessed<- stringr::str_replace(hines_cycle5$Race_Cat2," only","")
  #replace small categories
  hines_cycle5$RacePreprocessed[hines_cycle5$RacePreprocessed %in% c("Guamanian or Chamorro","Native Hawaiian","Other Pacific Islander")]<-"Pacific Islander"
  
  race_pp_levels <- names(sort(table(hines_cycle5$RacePreprocessed),decreasing = TRUE))
  
  #make "white" category first, because it's the majority category
  
  hines_cycle5$HHInc[hines_cycle5$HHInc=="Missing Data (Not Ascertained)"]<-NA
  hines_cycle5$HHInc[hines_cycle5$HHInc=="Missing data (Web partial - Question Never Seen)"]<-NA
  median_hh_inc<-median(as.integer(hines_cycle5$HHInc),na.rm=TRUE)
  hines_cycle5$HHInc_r <- as.integer(hines_cycle5$HHInc)-median_hh_inc
  
  hines_cycle5$Fruit[
    hines_cycle5$Fruit %in% c("Missing data (Not Ascertained)","Multiple responses selected in error","Missing data (Web partial - Question Never Seen)")] <- NA
  
  #some recoding now
  hines_cycle5$Fruit[hines_cycle5$Fruit=="½ cup or less"]<-"1/2 cup or less"
  hines_cycle5$Fruit[hines_cycle5$Fruit==" ½ cup to 1 cup"]<-"1/2 cup to 1 cup"
  table(hines_cycle5$Fruit)
  #now code factors into integers.
  hines_cycle5$Fruit_i <- as.integer(NA)
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="None"]<-0
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="1/2 cup or less"]<-1
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="1/2 cup to 1 cup"]<-2
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="1 to 2 cups"]<-3
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="2 to 3 cups"]<-4
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="3 to 4 cups"]<-5
  hines_cycle5$Fruit_i[hines_cycle5$Fruit=="4 or more cups"]<-6
  table(hines_cycle5$Fruit_i,hines_cycle5$Fruit)
  hines_cycle5$Fruit_z <- (hines_cycle5$Fruit_i - mean(hines_cycle5$Fruit_i,na.rm=TRUE))/sd(hines_cycle5$Fruit_i,na.rm=TRUE)
  
  hines_cycle5$Vegetables[
    hines_cycle5$Vegetables %in% c("Missing data (Not Ascertained)","Multiple responses selected in error","Missing data (Web partial - Question Never Seen)")] <- NA
  
  hines_cycle5$Vegetables[hines_cycle5$Vegetables=="½ cup or less"]<-"1/2 cup or less"
  hines_cycle5$Vegetables[hines_cycle5$Vegetables==" ½ cup to 1 cup"]<-"1/2 cup to 1 cup"
  table(hines_cycle5$Vegetables)
  #now code factors into integers.
  hines_cycle5$Vegetables_i <- as.integer(NA)
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="None"]<-0
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="1/2 cup or less"]<-1
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="1/2 cup to 1 cup"]<-2
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="1 to 2 cups"]<-3
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="2 to 3 cups"]<-4
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="3 to 4 cups"]<-5
  hines_cycle5$Vegetables_i[hines_cycle5$Vegetables=="4 or more cups"]<-6
  table(hines_cycle5$Vegetables_i,hines_cycle5$Vegetables)
  hines_cycle5$Vegetables_z <- (hines_cycle5$Vegetables_i - mean(hines_cycle5$Vegetables_i,na.rm=TRUE))/sd(hines_cycle5$Vegetables_i,na.rm=TRUE)
  hines_cycle5$RacePreprocessed <- factor(hines_cycle5$RacePreprocessed,race_pp_levels)
  
  gender_text <- as.character(hines_cycle5$SelfGender)
  hines_cycle5$Gender3C <- factor(gender_text,levels = c("Female","Male","MissingOrMultiple"))
  hines_cycle5$Gender3C[is.na(hines_cycle5$Gender3C)]<- "MissingOrMultiple"
  
  return(hines_cycle5)
}