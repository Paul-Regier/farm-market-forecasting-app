# Dependencies
# 1.0_Data_Pre-processing.R

library(tidyverse)


d <-  here("data","raw","od_raw.rds") %>% read_rds()

# Label Season Weeks ----

# TODO future refactor calendar construction

# 2024 Season
d2024 <- d %>% filter( year == 2024)
d_min <- d2024$date %>% min()
d_max <- d2024$date %>% max()
all_dates <- seq(d_min, d_max, by = "day")

# 2024 Start Of Season 
START_OF_SEASON <- isoweek(date("2024-06-03"))
cal2024 <- data.frame(
  year = 2024,
  date = all_dates
) %>% mutate(
  season_week = isoweek(date) - START_OF_SEASON + 1 # Assumes season does not cross ISO year boundary
)

# 2025 Season
d2025 <- d %>% filter( year == 2025)
d_min <- d2025$date %>% min()
d_max <- d2025$date %>% max()
all_dates <- seq(d_min, d_max, by = "day")

# 2025 Start Of Season 
START_OF_SEASON <- isoweek(date("2025-6-2"))
cal2025 <- data.frame(
  year = 2025,
  date = all_dates
) %>% mutate(
  season_week = isoweek(date) - START_OF_SEASON +1 
)

# 2026 Season
d_min <- date("2026-06-04") # Projected Start date
d_max <- date("2026-11-01") # Projected end date
all_dates <- seq(d_min, d_max, by = "day")

START_OF_SEASON <- isoweek(date("2026-06-01"))
cal2026 <- data.frame(
  year = 2026,
  date = all_dates
) %>% mutate(
  season_week = isoweek(date) - START_OF_SEASON +1 
)

cal <- rbind(cal2024,cal2025, cal2026)

# Key dates ----

## Open ----
open_days <- d$date %>% unique() %>% sort()

## PA Days ----
PA_days_York <- as.Date(c(
  # 2024 (from 2023–24 calendar)
  "2024-06-07", # Elementary
  "2024-06-27", # Secondary
  "2024-06-28", # Elementary and Secondary
  
  # 2024 (from 2024–25 calendar)
  "2024-09-27", # Both
  "2024-10-21", # Both
  "2024-11-15", # Both
  
  # 2025 (from 2024–25 calendar)
  "2025-06-09", # Elementary
  "2025-06-26", # Secondary
  "2025-06-27", # Elementary and Secondary
  
  # 2025 (from 2025–26 calendar)
  "2025-09-26", # Both
  "2025-10-24", # Both
  "2025-11-21", # Both
  
  # 2025 (from 2025–2026 calendar)
  "2026-06-05", # Elementary
  "2026-06-25", # Secondary
  "2026-06-26" # Elementary and Secondary
  
)) %>%  unique() %>%  sort()

PA_days_Toronto <- as.Date(c(
  # 2024 (from 2023–2024 calendar)
  #"2024-01-19", "2024-02-16", 
  "2024-04-19", "2024-06-07",
  
  # 2024 (from 2024–2025 key dates)
  "2024-09-27", "2024-10-11", 
  #"2024-11-15", "2024-11-29",
  
  # 2025 (from 2024–2025 key dates)
  #"2025-01-17", "2025-01-30", "2025-02-14", 
  "2025-06-06", "2025-06-26", "2025-06-27",
  
  # 2025 (from 2025–2026 key dates)
  "2025-09-26", "2025-10-10", "2025-11-14",
  
  # 2026 (from 2025–2026 key dates)
  #"2026-01-16", "2026-01-29", "2026-02-13", 
  "2026-06-05", "2026-06-25", "2026-06-26",
  
  # 2026 (from 2026–2027 key dates not available yet
  "2026-09-25" # My guess
  
  # 2027 (update for 2027 season)
)) |> unique() |> sort()



## Holidays ----
olidays <- as.Date(c(
  # 2024
  "2024-01-01", # New Year's Day
  "2024-02-19", # Family Day
  "2024-03-29", # Good Friday
  "2024-04-01", # Easter Monday
  "2024-05-20", # Victoria Day
  "2024-07-01", # Canada Day - on Monday
  "2024-08-05", # Simcoe (Civic) Day
  "2024-09-02", # Labour Day
  "2024-10-14", # Thanksgiving Day
  "2024-11-11", # Remembrance Day
  "2024-12-25", # Christmas Day
  "2024-12-26", # Boxing Day
  
  # 2025
  "2025-01-01",
  "2025-02-17",
  "2025-04-18",
  "2025-04-21",
  "2025-05-19",
  "2025-07-01", # Canada Day - Tuesday
  "2025-08-04", # Civic Day
  "2025-09-01", # Labour Day
  "2025-10-13", # Canadian Thanksgiving
  "2025-11-11", # Remembrance Day
  #"2025-12-25",
  #"2025-12-26",
  
  # 2026
  "2026-01-01",
  "2026-02-16",
  "2026-04-03",
  "2026-04-06",
  "2026-05-18",
  "2026-07-01", # Canada Day
  "2026-08-03", # Civic Day
  "2026-09-07", # Labour Day
  "2026-10-12", # Canadian Thanksgiving
  "2026-11-11" # Remembrance Day
  #"2026-12-25",
  #"2026-12-28"  # Boxing Day (observed)
  
  # 2026 Holidays
  
)) %>%  unique() %>%  sort()

## Near Holidays ----
Near_Holiday_Week = as.Date(c(
  # 2024
  "2024-06-28", # Friday before Canada Day
  "2024-06-30", # Sunday before Canada Day
  "2024-08-30", # Friday Before Labor Day
  "2024-10-11", # Friday bore Canadian Thanksgiving - also PA day
  # 2025
  "2025-06-29", # Sunday before Canada Day
  "2025-06-30",  # Monday before Canada Day
  "2025-08-29", # Friday Before Labor Day
  "2025-10-10", # Friday bore Canadian Thanksgiving - also PA day
  # 2026
  "2026-06-28", # Sunday - may or may not be open - but leaving in in case
  "2026-06-29", # Monday before Canada Day
  "2026-06-30", # Tuesday before Canada Day -> on Wednesday
  "2026-07-31", # Friday Before Labor Day
  "2026-10-09" # Friday bore Canadian Thanksgiving 
))

Thanksgiving_weekend <- as.Date(c(
  # 2025
  "2024-10-11", # Fri
  "2024-10-12", # Sat
  "2024-10-14", # Mon
  # 2025
  "2025-10-10", # Fri
  "2025-10-11", # Sat
  "2025-10-13",  # Mon
  # 2026
  "2026-10-9",  # Fri
  "2026-10-10",  # Sat
  "2026-10-12"  # Mon
))

## PYO Strawberry days ----

PYO_strawberry_days <- as.Date(c(
  
  # ---- 2024 PYO Season ----
  seq(as.Date("2024-06-12"), as.Date("2024-06-25"), by = "day"),
  # ---- 2025 PYO Season ----
  seq(as.Date("2025-06-16"), as.Date("2025-06-18"), by = "day"),
  seq(as.Date("2025-06-21"), as.Date("2025-07-01"), by = "day"),
  seq(as.Date("2025-07-03"), as.Date("2025-07-05"), by = "day"),
  # ---- 2026 PYO Season ---- 
  # TO UPDATE **************
  seq(as.Date("2026-06-14"), as.Date("2026-07-05"), by = "day")
)) |> unique() |> sort()


# Category Levels ----

category_levels <- c(
  "Weekday",
  "Saturday",
  "PA_day",
  "Holiday",
  "Near_Holiday",
  "Sat_Holiday_Weekend",
  "CLOSED"
)
category_levels2 <- c(
  "Weekday",
  "Saturday",
  "Holiday_or_PA",
  "Near_Holiday",
  "Sat_Holiday_Weekend",
  "CLOSED"
)

cal <-  cal %>% arrange(date) %>% mutate(
  weekday = wday(date, label = T, abbr = T),
  market_open = date %in% d$date | (year == 2026 & weekday != "Sun"),
  saturday = weekday == "Sat",
  holiday = date %in% holidays,
  PA_day_Toronto = date %in% PA_days_Toronto,
  PA_day_York = date %in% PA_days_York,
  # Saturday after a long-weekend holiday (Fri holiday or next Mon or Tuesday holiday)
  sat_holiday_weekend = saturday & (lag(holiday, 1, default = FALSE) |
                                      lead(holiday, 2, default = FALSE) |
                                      lead(holiday, 3, default = FALSE)),
  # Without Holiday Weekends
  day_category = case_when(
    date %in% Near_Holiday_Week ~ "Near_Holiday",
    PA_day_Toronto ~ "PA_day",
    holiday ~ "Holiday",
    saturday ~ "Saturday",
    weekday == "Sun" ~ "CLOSED",
    TRUE ~ "Weekday"
  ) %>% factor( levels = category_levels ),
  # Adding PA days - Toronto school district
  day_category_Toronto = case_when(
    date %in% Near_Holiday_Week ~ "Near_Holiday",
    PA_day_Toronto ~ "PA_day",
    holiday ~ "Holiday",
    sat_holiday_weekend ~ "Sat_Holiday_Weekend",
    saturday ~ "Saturday",
    weekday == "Sun" ~ "CLOSED",
    TRUE ~ "Weekday"
  ) %>% factor( levels = category_levels),
  # Adding PA days - York school district
  day_category_York = case_when(
    date %in% Near_Holiday_Week ~ "Near_Holiday",
    PA_day_York ~ "PA_day",
    holiday ~ "Holiday",
    sat_holiday_weekend ~ "Sat_Holiday_Weekend",
    saturday ~ "Saturday",
    weekday == "Sun" ~ "CLOSED",
    TRUE ~ "Weekday"
  ) %>% factor( levels = category_levels),
  # PA days for both York and Toronto School Districts
  day_category_both = case_when(
    date %in% Near_Holiday_Week ~ "Near_Holiday",
    (PA_day_York | PA_day_Toronto) ~ "PA_day",
    holiday ~ "Holiday",
    sat_holiday_weekend ~ "Sat_Holiday_Weekend",
    saturday ~ "Saturday",
    weekday == "Sun" ~ "CLOSED",
    TRUE ~ "Weekday"
  ) %>% factor( levels = category_levels),
  # Combining holiday and PA days
  day_category_cleaned = case_when(
    date %in% Near_Holiday_Week ~ "Near_Holiday",
    (holiday | PA_day_York | PA_day_Toronto) ~ "Holiday_or_PA",
    sat_holiday_weekend ~ "Sat_Holiday_Weekend",
    saturday ~ "Saturday",
    weekday == "Sun" ~ "CLOSED",
    TRUE ~ "Weekday"
  ) %>% factor( levels = category_levels2),
  # Bakery Category - combining all day category for modeling with `weekday`
  day_category_bakery = case_when(
    day_category_cleaned == "Saturday" | day_category_cleaned == "Weekday" ~ "Normal",
    TRUE ~ day_category_cleaned
  ),
  # PYO_Strawberry Dates
  PYO_strawberry = date %in% PYO_strawberry_days,
  Thanksgiving_weekend = date %in% Thanksgiving_weekend
)

write_rds(cal, here("data","calendar","cal.rds"))

# Save as workbook function ----


write_calendar_workbook <- function(
    data_frame,
    cal_style_ref, # Where logic for colors come from, in case of saving reduced data
    path,
    sheet_name = "Calendar",
    table_style = "TableStyleMedium2"
) {
  
  library(openxlsx)
  
  # build workbook
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  
  writeDataTable(
    wb, sheet_name, data_frame,
    tableStyle = table_style,
    withFilter = TRUE
  )
  
  # styles
  style_closed   <- createStyle(fgFill = "#BFBFBF")
  style_PA_day   <- createStyle(fgFill = "#A9D18E")
  style_saturday <- createStyle(fgFill = "#9DC3E6")
  style_holiday  <- createStyle(fgFill = "#D9C2E9")
  style_date     <- createStyle(numFmt = "yyyy-mm-dd")
  style_bold     <- createStyle(textDecoration = "bold")
  
  n <- nrow(data_frame)
  p <- ncol(data_frame)
  
  # row indices (based on reference calendar)
  rows_closed   <- which(!cal_style_ref$market_open) + 1
  rows_PA_day   <- which(cal_style_ref$PA_day_York) + 1
  rows_saturday <- which(cal_style_ref$saturday) + 1
  rows_holiday  <- which(cal_style_ref$holiday) + 1
  
  # apply row styles 
  if (length(rows_closed)) {
    addStyle(wb, sheet_name, style_closed, rows = rows_closed, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_PA_day)) {
    addStyle(wb, sheet_name, style_PA_day, rows = rows_PA_day, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_saturday)) {
    addStyle(wb, sheet_name, style_saturday, rows = rows_saturday, cols = 1:p, gridExpand = TRUE)
  }
  if (length(rows_holiday)) {
    addStyle(wb, sheet_name, style_holiday, rows = rows_holiday, cols = 1:p, gridExpand = TRUE)
  }
  
  # bold logical TRUE cells (based on written data) 
  logical_cols <- which(vapply(data_frame, is.logical, logical(1)))
  
  for (j in logical_cols) {
    rows_true <- which(data_frame[[j]]) + 1
    if (length(rows_true)) {
      addStyle(
        wb,
        sheet = sheet_name,
        style = style_bold,
        rows = rows_true,
        cols = j,
        stack = TRUE
      )
    }
  }
  
  # date formatting (assumes date is column 2)
  addStyle(
    wb, sheet_name,
    style = style_date,
    rows = 2:(n + 1),
    cols = 2,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name, cols = 1:p, widths = "auto")
  
  saveWorkbook(wb, path, overwrite = TRUE)
  
  invisible(path)
}


# Save Calendar ----
write_calendar_workbook(
  data_frame     = cal,
  cal_style_ref = cal,
  path          = here("data", "calendar", "Calendar.xlsx")
)

# Save Reduced Calendar ----
cal_reduced <- cal %>% dplyr::select(
  year, date, season_week, weekday, day_category_both
)

write_calendar_workbook(
  data_frame     = cal_reduced,
  cal_style_ref = cal,
  path          = here("data", "calendar", "Calendar_reduced.xlsx")
)