library(tidyverse)
library(here)
library(janitor)

DEBUG <-  FALSE
#detach("package:MASS", unload = TRUE)

# Dependencies
#   1.0_Data_Pre-processing.R
#   1.2_Weather_Data_Processing.R



od <- here("data","raw","od_raw.rds") %>% read_rds() %>% 
  dplyr::select(where(~ !(is.numeric(.) && all(. == 0, na.rm = FALSE))))
category <- here("data","item_counts_categorized3.xlsx") %>% readxl::read_xlsx()


cal <- here("data","calendar","cal_with_weather.rds") %>% read_rds() %>% 
  dplyr::select(-year, -market_open) %>% 
  dplyr::select(-snow_on_ground_cm, -snow_flag, -spd_of_max_gust_km_h) %>% 
  mutate(
    #close_time_minutes = 18*60 + 30, #6:30 PM = 18:30
    # Weather flags
    extreme_temp_flag = (max_temp_c < 12 | max_temp_c > 32), # 53.6-89.6 degrees F
    # Final Weather Category
    weather_category = case_when(
      precip_flag ~ "bad_weather",
      (!high_wind_flag & !extreme_temp_flag) ~ "good_weather",
      TRUE ~ "neutral_weather"
    ) %>% factor(levels = c("bad_weather","neutral_weather","good_weather"))
  )

# Add close times ----

cal_with_close_time <- 
  here("data","calendar","cal_open_close_time_updated.xlsx") %>% 
  readxl::read_xlsx() %>% clean_names() %>% 
  dplyr::select(date, close_time)
cal_with_close_time <- cal_with_close_time %>% 
  mutate(
    close_time_minutes = as.integer(str_sub(close_time,1,2))*60 +
      as.integer(str_sub(close_time,4,5))
  )

cal <- cal %>% left_join( cal_with_close_time, by = "date")


if(DEBUG) {
  # Close times table
  cal_with_close_time %>% count(close_time)  
}



# Raw data cleaning ----

## Remove "trial" days ----
records_by_date <- od %>% count(date)
remove_dates <- records_by_date %>% filter( n<100)
# remove_dates

od <- od %>% filter( !(date %in% remove_dates$date) )


## Remove Refunds ----

od <- od %>% filter( !refunded )


# Data Exports ----

# Updates 2025-1-6
# - Added payment_time in minutes
# - time_minutes = time of sale in minutes after midnight
# - time_minutes_before_close = minutes before close


## Item data ----
item_data <- od %>% dplyr::select(
  year, date, week, day, hour, daypart, payment_time,
  order,item_name, category,
  item_price,
  item_gross_amount,   # gross
  item_total_discount, # deductions 
  item_net_amount,     # gross - deductions
  payment_type, payment_time,
) %>% arrange(date) %>%
  mutate( doy = yday(date),
          time_minutes = as.integer(str_sub(payment_time, 1, 2)) * 60 +
            as.integer(str_sub(payment_time, 4, 5)),
  ) %>%
  relocate(doy, .after = date) %>% 
  relocate(time_minutes, .after = payment_time)
write_rds(item_data, here("data","processed","item_data.rds"))




## Missing payment time ----
# 67 Records were missing payment time

if(DEBUG) {
  od %>%
    mutate(t = lubridate::hm(payment_time)) %>%
    filter(!is.na(payment_time) & is.na(t)) 
  
  # All have "" as payment time
  item_data %>% filter( payment_time == "")
  
}

## Open close time details ----

open_close_times <- item_data %>% group_by(date) %>% 
  summarize(
    missing_time = sum(payment_time == ""),
    min_time = min(payment_time[payment_time != ""]),
    max_time = max(payment_time)
  )
writexl::write_xlsx(open_close_times, here("data","calendar","cal_open_close_time.xlsx"))

### Check bakery goods

if(DEBUG) {
  open_close_times <- item_data %>% 
    left_join( category %>% dplyr::select(-n), by = "item_name") %>% # to be able to filter bakery goods
    filter(bakery == "T") %>% 
    group_by(date) %>% 
    summarize(
      missing_time = sum(payment_time == ""),
      min_time = min(payment_time[payment_time != ""]),
      max_time = max(payment_time)
    )
  open_close_times
  max(open_close_times$missing_time)
  #NO missing times on baked goods! WOOT!
  
  open_close_times %>% mutate(
    max_time_int = as.integer(str_sub(max_time, 1, 2)) * 60 +
      as.integer(str_sub(max_time, 4, 5)) - 16*60+30,
    year = lubridate::year(date) %>% as.factor()
  ) %>% ggplot(aes(max_time_int, fill = year)) +
    geom_histogram(color = "black") +
    facet_wrap(~year) +
    labs(
      title = "Time of last sale (in minutes)",
      x = "minutes before 18:30 "
    )
}




## Items by date ----
# **total items/sales by date and item**

items_by_date <- item_data %>% 
  group_by( year, date, item_name) %>% 
  summarise(
    num_items = n(),
    gross_sales = sum(item_price),
    last_sold_chr = max(payment_time),
    last_sold_minutes = max(time_minutes),
    .groups = "drop"
  ) %>% 
  left_join( category %>% dplyr::select(-n), by = "item_name") %>% 
  left_join( cal, by = "date") %>% 
  mutate(  
    month_day = as.Date(paste0("2000-", format(date, "%m-%d"))),
    bakery = coalesce(bakery, "F") == "T",
    last_sale_min_to_close = close_time_minutes - last_sold_minutes,
    last_sale_min_to_close_cat = case_when(
      last_sale_min_to_close < 100 ~ "<100",
      last_sale_min_to_close < 200 ~ "100-200",
      last_sale_min_to_close < 300 ~ "200-300",
      TRUE ~ ">300"
    ) %>% factor(levels = c("<100","100-200","200-300",">300")),
    sold_out_flag = last_sale_min_to_close >= 300
  ) %>% 
  relocate( month_day, category, item_name, 
            close_time, close_time_minutes, last_sale_min_to_close, sold_out_flag, .after = date )

write_rds(items_by_date, here("data","processed","items_by_date.rds"))
          
          
# Corn
if(DEBUG) {
  items_corn <- items_by_date %>% filter(item_name == "Reesor's Sweet Corn Dozen (13)")
  head(items_corn)
  # We got double entries of some items!
}

  
  
# Check
cal$date %>% unique() %>% length()
nrow(cal)

### Last Sold by Year ----

if(DEBUG) {
items_by_date %>% ggplot(aes(last_sale_min_to_close, fill = year)) +
  geom_histogram(color = "black") +
  facet_wrap(~year)
}

### Last Sold Minutes to Close by Category ----

if(DEBUG) {
items_by_date %>% ggplot(aes(date, last_sale_min_to_close, fill = category)) +
  geom_boxplot()
  items_by_date %>% count(year)
}


### Last Sold Bakery ----

if(DEBUG) {
items_by_date %>% filter(bakery) %>% 
  ggplot(aes(last_sale_min_to_close, fill = year)) +
  geom_histogram(color = "black") +
  facet_wrap(~year)

}

## Bakery Totals_by_date ----
# **total items/sales by date and item**
  
bakery_totals_by_date = items_by_date %>% 
  filter( bakery ) %>% 
  group_by( year, date, bakery_type) %>% 
  summarise(
    num_items = sum(num_items),
    gross_sales = sum(gross_sales),
    last_sale_min_to_close = min(last_sale_min_to_close),
    .groups = "drop"
  ) %>% 
  left_join( cal, by = "date")

write_rds(bakery_totals_by_date, here("data","processed","bakery_totals_by_date.rds"))




# Model Metrics ----

## Date Metrics (Model_data)
# For modeling:

date_metrics = item_data %>% group_by(year, date) %>% 
  summarize(
    num_items = n(),
    num_orders = n_distinct(order),
    ave_items_per_order = num_items/num_orders,
    gross = sum(item_gross_amount),
    net =   sum(item_net_amount),
    discounts = sum(item_total_discount),
    .groups = "drop"
  ) %>% 
  left_join( cal, by = "date") 

write_rds(date_metrics, here("data","processed","date_metrics.rds"))
# Write to xlsx below for owner's use

date_metrics_by_category = item_data %>% group_by(year, date, category) %>% 
  summarize(
    num_items = n(),
    num_orders = n_distinct(order),
    ave = num_items/num_orders,
    gross = sum(item_gross_amount),
    net =   sum(item_net_amount),
    discounts = sum(item_total_discount),
    .groups = "drop"
  ) %>% 
  left_join( cal, by = "date")

write_rds(date_metrics_by_category, here("data","processed","date_metrics_by_category.rds"))



# Key metrics by week ----

week_metrics = date_metrics %>% group_by(year, season_week) %>% 
  summarize(
    start_date = min(date),
    num_items = sum(num_items),
    num_orders = sum(num_orders),
    gross = sum(gross),
    net =   sum(net),
    gross_per_order = gross/num_orders,
    items_per_order = num_items/num_orders,
    discounts = sum(discounts),
    weather_mean = mean(weather_score),
    .groups = "drop"
  ) 

write_rds(week_metrics, here("data","processed","week_metrics.rds"))

week_metrics_by_category = date_metrics_by_category %>% group_by(year, season_week, category) %>% 
  summarize(
    start_date = min(date),
    num_items = sum(num_items),
    num_orders = sum(num_orders),
    gross = sum(gross),
    net =   sum(net),
    gross_per_order = gross/num_orders,
    items_per_order = num_items/num_orders,
    discounts = sum(discounts),
    weather_mean = mean(weather_score),
    .groups = "drop"
  )

write_rds(week_metrics_by_category, here("data","processed","week_metrics_by_category.rds"))





# Save Date Metrics as workbook ----

if(DEBUG) {
  levels(date_metrics$day_category)
  
  library(openxlsx)
  
  # build workbook ---
  wb <- createWorkbook()
  addWorksheet(wb, "date_metrics")
  
  # Write as a normal range (NOT an Excel Table)
  writeData(wb, "date_metrics", date_metrics, withFilter = TRUE)
  
  n <- nrow(date_metrics)
  p <- ncol(date_metrics)
  
  # Optional: make header bold + lightly filled (Google-safe)
  style_header <- createStyle(textDecoration = "bold", fgFill = "#E6E6E6")
  addStyle(wb, "date_metrics", style_header, rows = 1, cols = 1:p, gridExpand = TRUE)
  
  # Styles
  
  
  style_PA_day  <- createStyle(fgFill = "palegreen")
  style_saturday <- createStyle(fgFill = "steelblue1")
  style_holiday <- createStyle(fgFill = "#BF3EFF")
  style_near_holiday <- createStyle(fgFill = "plum")
  style_sat_holiday <- createStyle(fgFill = "steelblue")
  style_date    <- createStyle(numFmt = "yyyy-mm-dd")
  style_bold <- createStyle(textDecoration = "bold")
  
  # Excel rows are offset by 1 because of the header
  rows_PA_day  <- which(date_metrics$PA_day_York) + 1
  rows_saturday <- which(date_metrics$saturday) + 1
  rows_holiday <- which(date_metrics$holiday) + 1
  rows_near_holiday <- which(date_metrics$day_category_York == "Near_Holiday") + 1
  rows_sat_holiday <- which(date_metrics$day_category_York == "Sat_Holiday_Weekend") + 1
  
  # If weekend should "win" over market_open==FALSE, exclude weekend rows from grey
  # rows_closed <- setdiff(rows_closed, rows_weekend)
  
  # Apply styles to entire rows
  if (length(rows_PA_day)) {
    addStyle(wb, "date_metrics", style_PA_day, rows = rows_PA_day, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_saturday)) {
    addStyle(wb, "date_metrics", style_saturday, rows = rows_saturday, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_holiday)) {
    addStyle(wb, "date_metrics", style_holiday, rows = rows_holiday, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_near_holiday)) {
    addStyle(wb, "date_metrics", style_near_holiday, rows = rows_near_holiday, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_sat_holiday)) {
    addStyle(wb, "date_metrics", style_sat_holiday, rows = rows_sat_holiday, cols = 1:p, gridExpand = TRUE)
  }
  
  logical_cols <- which(sapply(date_metrics, is.logical))
  
  for (j in logical_cols) {
    rows_true <- which(date_metrics[[j]]) + 1  # +1 for header row
    
    if (length(rows_true)) {
      addStyle(
        wb,
        sheet = "date_metrics",
        style = style_bold,
        rows = rows_true,
        cols = j,
        stack = TRUE
      )
    }
  }
  # Date formatting for the date column (col 1), rows 2..n+1
  addStyle(wb, "date_metrics", style_date, rows = 2:(n + 1), cols = 2, gridExpand = TRUE, stack = TRUE)
  # Optional: make it nicer
  freezePane(wb, "date_metrics", firstRow = TRUE)
  setColWidths(wb, "date_metrics", cols = 1:p, widths = "auto")
  
  saveWorkbook(wb, here("data","calendar","Calendar_with_metrics.xlsx"), overwrite = TRUE)

}
