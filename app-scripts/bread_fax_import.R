library(googlesheets4)
library(dplyr)
library(janitor)

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1zcyP1m6vYmJB_p8qPh7W9jLeAV_owrbrQkUea-QsGIs/edit?usp=sharing"

load_google_ref <- function() {
  #gs4_deauth()  # public sheet: no login needed DO NOT UNCOMMENT!!
  
  bread_fax <- read_sheet(SHEET_URL)
  
  colnames(bread_fax) <- c(
    "timestamp",
    "staff_name",
    "report_date",
    
    "muffins_fresh",
    "muffins_discounted",
    
    "multigrain_fresh",
    "multigrain_discounted",
    
    "pilgrim_fresh",
    "pilgrim_discounted",
    
    "specialty_fresh",
    "specialty_discounted",
    
    "fruit_pies_fresh",
    "fruit_pies_discounted",
    
    "pumpkin_pies_fresh",
    "pumpkin_pies_discounted",
    
    "sold_out_notes",
    
    "max_temp_c",
    "precip_chance_pct",
    "max_wind_kmh",
    
    "additional_comments"
  ) 
  
  bread_fax %>% mutate(
    report_date = as.Date(report_date),
    pumpkin_pies_fresh = pumpkin_pies_fresh %>% as.numeric(),
    pumpkin_pies_discounted = pumpkin_pies_discounted %>% as.numeric()
  )
}
