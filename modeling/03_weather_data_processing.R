# Dependencies
# 1.1_Data_Pre-processing.R

DEBUG = FALSE

library(tidyverse)
library(janitor)
library(lubridate)

cal <- here("data","calendar","cal.rds") %>% read_rds()

# Toronto North York Data
w_TNY <- bind_rows(
  read_csv(here("data","raw","weather","en_climate_daily_TNY_2024.csv")),
  read_csv(here("data","raw","weather","en_climate_daily_TNY_2025.csv"))
) %>% clean_names() %>% 
  dplyr::select(-climate_id, -total_precip_flag, -data_quality)

# Uxbridge West Ontario
w_UWO <- bind_rows(
  read_csv(here("data","raw","weather","en_climate_daily_UWO_2024.csv")),
  read_csv(here("data","raw","weather","en_climate_daily_UWO_2025.csv"))
) %>% clean_names() %>% 
  dplyr::select(-climate_id, -total_precip_flag, -data_quality)

# Check names
if (!identical(colnames(w_TNY), colnames(w_UWO))) {
  stop("Weather schemas differ between TNY and UWO")
}


# glimpse(w_TNY)


#**Key variables:*
#  - date_time
# - year
# - max_temp_c
# - min_temp_c
# - mean_temp_c
# - total_precip_mm
# - snow_on_ground_cm
# - spd_of_max_gust_km_h (only available UWO)

#**Legend:**
# M = Missing
# E = Estimated


safe_max_na <- function(x) {
  if (all(is.na(x))) NA
  else max(x, na.rm = TRUE)
}


# Combining weather
weather_combined <-
  bind_rows(
    w_TNY %>% mutate(site = "TNY", date = mdy(date_time)),
    w_UWO %>% mutate(site = "UWO", date = mdy(date_time))
  ) %>%
  group_by(date) %>%
  summarise(
    max_temp_c = mean(max_temp_c, na.rm = TRUE),
    min_temp_c = mean(min_temp_c, na.rm = TRUE),
    mean_temp_c = mean(mean_temp_c, na.rm = TRUE),
    total_precip_mm = mean(total_precip_mm, na.rm = TRUE),
    precip_flag = any(total_precip_mm > 0, na.rm = TRUE),
    snow_on_ground_cm = mean(snow_on_grnd_cm, na.rm = TRUE),
    snow_flag = snow_on_ground_cm > 0,
    spd_of_max_gust_km_h = safe_max_na(spd_of_max_gust_km_h),
    high_wind_flag = any(spd_of_max_gust_km_h >= 40, na.rm = TRUE)
  ) %>% mutate(
    weather_score =
      -0.5 * (max_temp_c > 32) +
      -0.5 * (max_temp_c > 28) +
      -0.5 * (mean_temp_c < 5) + # 5 degrees C = 39.2 F
      -0.5 * (mean_temp_c < 10) + # 10 degrees C = 50 F
      -1.0 * precip_flag +
      -1.0 * snow_flag +
      -1.0 * high_wind_flag,
    weather_index = case_when(
      weather_score > -1 ~ "Good",
      weather_score < -1 ~ "Bad",
      TRUE ~ "Neutral"
    ) %>% factor(levels = c("Bad", "Neutral","Good"))
  )


if(DEBUG) {
  weather_combined %>% ggplot(aes(date, weather_score)) +
    geom_line()
}


# Write weather data ----
# data/calendar
write_rds(weather_combined, here("data","calendar", "weather_daily.rds"))


# Calendar with weather ----
cal2 <- cal %>% left_join( weather_combined, by = "date")

write_rds(cal2, here("data","calendar","cal_with_weather.rds"))
writexl::write_xlsx(cal2, here("data","calendar","calendar_with_weather.xlsx"))






