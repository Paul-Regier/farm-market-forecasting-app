# app.R
# Version: v8
# Last updated: 2026-01-08

# Reesor Farm - Bakery Dashboard (Shiny)
# Uses bslib navbar + cards
# Assumes the working directory is the project root that contains:
# - model-data/
# - summary-data/
# - dashboard-scripts/ (optional) OR scripts in project root

library(here)
here("app","global.R") %>% source()
here("app","ui.R")     %>% source()
here("app","server.R") %>% source()

shinyApp(ui, server)
