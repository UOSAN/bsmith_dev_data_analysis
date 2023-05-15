---
  title: "explore_orfcs_pilot_results"
author: "Ben Smith"
date: "10/19/2021"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(lubridate)

library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)
print(getwd())
source("orfcs_utilities.R")
Sys.setenv(R_CONFIG_ACTIVE = Sys.info()["nodename"])
data_dir <- config::get("rural_data_dir")
#data_dir <- "/Users/benjaminsmith/Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/analysis_files/data/"

data_key <- read_csv(paste0(data_dir, "cloudresearch_survey_results/ORFCS Bespoke CR2_November 2, 2021_11.47_key.csv")) %>% t %>% data.frame
data_key <- cbind("colkey" = rownames(data_key),data_key)
rownames(data_key) <- NULL
colnames(data_key) <- c("question_key","question_text","import_json")
```


### scorequaltrics formatted

To use the rubric scorer we really have to use the Scorequaltrics package from the start...
Next we need to 
(1) Exclude participants with too much missing data

(2) code up the scales. we can use the same scale coders that are used for the the mainstream DEV dataset. Maybe? 
  
  ```{r}

if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(knitr)) {install.packages('knitr')}
if (!require(devtools)) {install.packages('devtools')}
if (!require(scorequaltrics)) {devtools::install_github('dcosme/qualtrics', ref = "dev/enhance")}
if (!require(ggcorrplot)) {install.packages('ggcorrplot')}
```

```{r}

cred_file_location = config::get('qualtrics_credentials')
#keep_columns = '(Login|ResponseId|Finished|SID)'
survey_name_filter = '(Oregon Rural Food Consumption|ORFCS)'
#sid_pattern = 'DEV[0-9]{3}$'
#exclude_sid = '^99|DEV999|DEV000|DEV998|DEV737' # subject IDs to exclude
# identifiable_data = c('IPAddress', "RecipientEmail", "RecipientLastName", "RecipientFirstName",
#                       "LocationLatitude", "LocationLongitude") # exclude when printing duplicates

output_file_dir = "~/Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/analysis_files/data/"

rubric_dir = '/Users/benjaminsmith/Dropbox (University of Oregon)/UO-SAN Lab/Berkman Lab/Devaluation/DEV_scoring_rubrics'

```

```{r}
credentials = scorequaltrics::creds_from_file(cred_file_location)

# filter
surveysAvail = scorequaltrics::get_surveys(credentials)
surveysFiltered = filter(surveysAvail, grepl(survey_name_filter, SurveyName))

#knitr::kable(arrange(select(surveysFiltered, SurveyName), SurveyName))

```



```{r}

rubric_dir = config::get('rubric_dir')#

# specify rubric paths
scoring_rubrics = data.frame(file = dir(file.path(rubric_dir), 
                                        pattern = '.*scoring_rubric.*.csv',
                                        full.names = TRUE))

# read in rubrics
scoring_data_long = scorequaltrics::get_rubrics(scoring_rubrics, type = 'scoring')
# print the first 10 rows
head(scoring_data_long[, -1], 10)
```





```{r getsurveydata, results="hide"}
# get data
surveys_long = scorequaltrics::get_survey_data(surveysFiltered,
                                               pid_col = "aid") #%>%
#filter(!item %in% identifiable_data)# %>% #filter out identifiable data
#mutate(SID=coalesce(SID_1,Login)) %>% #combine the SID_1 and Login cols
#select(-SID_1,-Login) #remove them from the old columns

# print first 10 rows
head(surveys_long, 10)
```