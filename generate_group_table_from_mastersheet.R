
library(tidyverse)
Sys.setenv(R_CONFIG_ACTIVE = Sys.info()["nodename"])
dropbox_file_dir = config::get("dev_analysis_data_dir")

participant_mastersheet <- readxl::read_xlsx(paste0(dropbox_file_dir,"DEV Participant Mastersheet_copy.xlsx")) %>% select(-`First Name`,-`Last Name`,-`Email`,-`Initals`,-Group)

participant_mastersheet
