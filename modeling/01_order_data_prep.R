library(tidyverse)
library(janitor)
library(lubridate)
library(here)

DEBUG <- FALSE

# Load Raw Data ----


# Note on reading files - I'm using read.csv instead of readr's read_csv because:
# - some columns are sparse and I will never use, e.g. warning on column 58, note

## 2026 File Names ----
# YEAR <- 2026

# TODO UPDate next year


## 2025 File Names ----
YEAR <- 2025
file_names = c(
  "FullOrdersDetails_for_period_2025-Jun-01_until_2025-Jul-31 - orders with items.csv",
  "FullOrdersDetails_for_period_2025-Aug-01_until_2025-Sep-30 - orders with items.csv",
  "FullOrdersDetails_for_period_2025-Oct-01_until_2025-Nov-30 - orders with items.csv"
) %>%  purrr::map(~ here("data", "raw", paste0("sales_", YEAR), .x))


d1 <-read.csv(file_names[[1]], na.strings = c("", "NA", "N/A"))
d2 <-read.csv(file_names[[2]], na.strings = c("", "NA", "N/A"))
d3 <-read.csv(file_names[[3]], na.strings = c("", "NA", "N/A"))

# Check names
if (!identical(colnames(d1), colnames(d2)) &
    !identical(colnames(d1), colnames(d3))) {
  stop("Order data schemas differ")
}


d2025 <- rbind(d1,d2,d3) %>%
  mutate(year = YEAR)

## 2024 File Names ----
YEAR <- 2024
file_names = c(
  "FullOrdersDetails_for_period_2024-Jun-01_until_2024-Jul-31 - orders with items.csv",
  "FullOrdersDetails_for_period_2024-Aug-01_until_2024-Sep-30 - orders with items.csv",
  "FullOrdersDetails_for_period_2024-Oct-01_until_2024-Nov-30 - orders with items.csv"
) %>% purrr::map(~ here("data", "raw", paste0("sales_", YEAR), .x))

d1 <-read.csv(file_names[[1]], na.strings = c("", "NA", "N/A"))
d2 <-read.csv(file_names[[2]], na.strings = c("", "NA", "N/A"))
d3 <-read.csv(file_names[[3]], na.strings = c("", "NA", "N/A"))

d2024 <- rbind(d1,d2,d3) %>%
  mutate(year = YEAR)



# 2026 Order Data - to add January 7027


# Order Data ----
od = bind_rows(d2024,d2025) %>% 
  clean_names() %>% 
  mutate(
  time = lubridate::hm(time),
  date = lubridate::ymd(date),
  year = factor(year)
) 


# Debugging decimal accuracy
if(DEBUG) {
  glimpse(od)
  
  options(digits = 22)
  ## These chould be equal - note some aren't
  head(od$order_net + od$order_cost_and_tax, 10)[6:10]
  head( od$order_amount_collected, 10)[6:10]
  
  # Tax rate precision  
  options(digits = 22)
  table(od$tax_rate)
  
  # Even some fields to not match like I think they should
  equal_to_cent <- function(x, y) {
    print(as.integer(round(x * 100)))
    print(as.integer(round(y * 100)))
    as.integer(round(x * 100)) == as.integer(round(y * 100))
  }
  x = od$order_net + od$order_cost_and_tax
  y = od$order_amount_collected
  equal_to_cent(x[20:25], y[20:25])
  
  # Reset precision
  options(digits = 7)
}

# Note: Rounding doesn't help floating point (num) data storage issue.
# - in future, model everything in cents to make modeling pipeline more reliable.
#- for now, include tolerance checks for values - SEE BELOW
# changes to something not $0.13, we would need to adjust this.

# Future - If/when pipeline grows for:
#   - reconciliation
#   - audits
#   - cross-system joins
# Revisit cents or decimals.



# Randomize Data
RANDOMIZE = TRUE

if(RANDOMIZE) {
 
  library(digest)  
  min_to_str <- function(x) {
    hours <- x %/% 60
    mins  <- x %% 60
    sprintf("%02d:%02d", hours, mins)
  }
  MAX_MIN = (24-1)*60+59
  #min_to_str(1121)
  set.seed(0)
  
  od_scrambled <- od %>% mutate(
    time_minutes = as.integer(str_sub(payment_time, 1, 2)) * 60 +
      as.integer(str_sub(payment_time, 4, 5)),
    min_offset = sample(c(-1,1),n(),replace = T)*round(runif(n())*10),
    keep = sample(c(TRUE, FALSE),n(),replace = T, prob = c(0.9,0.1))
    ) %>% filter(keep) %>%
    mutate(
      payment_time = pmin(MAX_MIN, pmax(0, time_minutes + min_offset)) %>% min_to_str()
    ) %>% dplyr::select(-time_minutes,-min_offset,-keep)
  od_2 = od %>% filter(sample(c(TRUE, FALSE),n(),replace = T, prob = c(0.95,0.05)))
  # Recombine mostly jittered data with a small random subset of original rows
  # (identifiers removed) to preserve aggregate distributions
  SALT <- "public-demo-2026"
  od <- rbind(od_scrambled, od_2) %>% arrange(date) %>% 
    select(-employee, -merchant_name, -device_name, -card_type) %>% 
    mutate(order = digest(paste0(order, SALT), algo = "sha256") %>% substr(1,12))
  stopifnot(!any(str_detect(od$order, "^\\d+$")))
}

# Write Data ----
write_rds(od, here("data","raw","od_raw.rds"))


# Extract Category Report ----
od %>% count(item_name, category, sort = TRUE) %>%
  writexl::write_xlsx(here("data","item_counts.xlsx"))
