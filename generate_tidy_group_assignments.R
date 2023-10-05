
library(tidyverse)
Sys.setenv(R_CONFIG_ACTIVE = Sys.info()["nodename"])


dropbox_file_dir = config::get("dev_analysis_data_dir")

group_assignments<-readr::read_csv(paste0(dropbox_file_dir,"DEV-GroupAbridged_DATA_LABELS_2023-06-23_1859.csv"))

group_assignments$`Event Name`<-NULL

group_assignments <- rename(group_assignments,
  dev_id=`DEV ID`,
  randomized_assignment=`Randomized Arm`
  )


set.seed(202306)
group_assignments_shuffled$randomized_assignment <- sample(group_assignments$randomized_assignment,replace = FALSE)

