library(tidyverse)
library(here)

# Baseline model of traffic ----
model_data <-  here("data","processed","date_metrics.rds") %>% read_rds()

library(splines)
baseline_model <-
  lm(
    num_items ~ ns(season_week, df = 3) + day_category_both + PYO_strawberry + Thanksgiving_weekend:day_category_both,
    data = model_data
  )
#summary(baseline_model)

model_data <- model_data %>% mutate(
  num_items_pred = predict(baseline_model, newdata = model_data),
  num_items_residual = num_items - num_items_pred
)

write_rds(model_data, here("data","model","baseline_model.rds"))



# Predication data for next year ---

category_levels <- c(
  "Weekday",
  "Saturday",
  "PA_day",
  "Holiday",
  "Near_Holiday",
  "Sat_Holiday_Weekend"
)

cal = here("data","calendar","cal.rds") %>% read_rds()
pred_2026 <- cal %>% 
  dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, Thanksgiving_weekend) %>% 
  mutate(
    day_category_both = case_when(
      day_category_both == "CLOSED" ~ NA,
      TRUE ~ day_category_both
    ) %>% factor(levels = category_levels)
  )


pred_2026 <- pred_2026 %>% mutate(
  num_items_pred = predict(baseline_model, newdata = pred_2026)
)
write_rds(pred_2026, here("data","model","pred_2026.rds"))

# for next step
# pred_2026 <- here("data","model","pred_2026.rds") %>% read_rds()
