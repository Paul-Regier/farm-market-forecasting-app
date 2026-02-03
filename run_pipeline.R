# Script for running data pipeline
library(here)


path1 <- here("modeling","01_order_data_prep.R")
path2 <- here("modeling","02_calendar_generation.R")
path3 <- here("modeling","03_weather_data_processing.R")
path4 <- here("modeling","04_order_data_feature_engineering.R")
path5 <- here("modeling","05_baseline_model.R")

# NOTE: Public repository uses pre-randomized data generated in 01_order_data_prep.R
# RANDOMIZE should remain FALSE when rerunning the pipeline
RANDOMIZE = TRUE
if(!RANDOMIZE) {
  source(path1, echo = FALSE) 
}

source(path2, echo = FALSE)
source(path3, echo = FALSE)
source(path4, echo = FALSE)
source(path5, echo = FALSE)
