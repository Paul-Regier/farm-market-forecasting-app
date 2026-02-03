# app_global.R
# Version: v8
# Last updated: 2026-01-08

# Reesor Farm - Bakery Dashboard (Shiny)
# Uses bslib navbar + cards
# Assumes the working directory is the project root that contains:
# - model-data/
# - summary-data/
# - dashboard-scripts/ (optional) OR scripts in project root
#
# Scripts used:
# - bread_fax_import.R
# - bakery_model_script.R
# - seasonal_market_projections.R
# - general_product_script.R

#Sys.setenv(
#  GS4_SERVICE_JSON = "C:/Users/paulr/Dropbox/R/Reesor-Farm/.secrets/gs_service.json"
#)
#Sys.getenv("GS4_SERVICE_JSON")

# message("APP WD: ", getwd())
# message("FILES: ", list.files())

# Google Auth
library(googlesheets4)
stopifnot(file.exists(".secrets/gs_service.json"))
gs4_auth(path = ".secrets/gs_service.json")
# file.exists("../.secrets/gs_service.json") doesn't work because project is at root



suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(gt)
})

# Helper: source scripts from either dashboard-scripts/ or roo
safe_source <- function(fname) {
  candidates <- c(
    file.path("dashboard-scripts", fname),
    fname
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("Could not find script: ", fname, " (looked in dashboard-scripts/ and project root)")
  source(hit, local = FALSE)
  invisible(hit)
}

# Load supporting scripts + precompute heavy objects once ----
library(here)
safe_source(here("app-scripts","bread_fax_import.R"))              # defines load_google_ref()
safe_source(here("app-scripts","bakery_model_script.R"))           # defines bakery_pred_2026()
safe_source(here("app-scripts","seasonal_market_projections.R"))   # defines build_dashboard_data()
safe_source(here("app-scripts","general_product_script.R"))        # defines general_product_analysis()

# Precompute bakery predictions + plots (heavy; do once at startup) ----
bakery_tmp <- bakery_pred_2026()
pred_2026_all <- bakery_tmp$pred_2026
plots_bakery  <- bakery_tmp$plots_bakery_items

# Precompute seasonal market data (light; do once) ----
season_tmp   <- build_dashboard_data()
week_labels  <- season_tmp$week_labels
data_comb    <- season_tmp$data_comb

# Precompute general item analysis ----
items_by_date <- here("data","processed","items_by_date.rds") %>% read_rds()
  
items_by_date_weekly <- items_by_date %>% group_by(season_week,year, item_name) %>% 
  summarize(
    total_items = sum(num_items),
    .groups = "drop"
  )

# Constants
DAY_CAT_LEVELS <- unique(items_by_date$day_category_both)

max_week <- max(week_labels$season_week, na.rm = TRUE)

Bakery_Types_Names <- c("Muffins","Multigrain","Pilgrim","Specialty","Pie","Pumpkin Pies*")

