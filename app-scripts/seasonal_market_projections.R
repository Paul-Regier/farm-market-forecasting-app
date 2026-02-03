build_dashboard_data <- function() {

model_data <- here("data","model","model_data_2.3.rds") %>% read_rds()
pred_2026 <- here("data","model","pred_2026.rds") %>%  read_rds() %>% filter( year == "2026")
  

week_labels = pred_2026 %>%  group_by(season_week) %>% 
  summarize( 
    start_date = min(date),
    .groups = "drop"
  ) %>% mutate(
    date_md = str_sub(start_date, 6),
    week_label = paste("week", season_week, "-", date_md)
  )

years = paste(2024:2026)
#pred_2026 %>% glimpse()
data_2026_pred <- pred_2026 %>% rename(
  num_items = num_items_pred
) %>% mutate(
  data_type = "Pred" %>% factor(levels = c("Actual","Pred")),
  year = year %>% factor(levels = years)
)

data_pre2026_actual <- model_data %>%
  dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, Thanksgiving_weekend, num_items_pred) %>%
  rename(
    num_items = num_items_pred
  ) %>% mutate(
    data_type = "Pred" %>% factor(levels = c("Actual","Pred")),
    year = year %>% factor(levels = years)
  )
data_pred2026_pred <- model_data %>%
  dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, Thanksgiving_weekend, num_items) %>% 
  mutate(
    data_type = "Actual" %>% factor(levels = c("Actual","Pred")),
    year = year %>% factor(levels = years)
  )

data_comb <- rbind(
  data_2026_pred, data_pre2026_actual, data_pred2026_pred
) %>% mutate(
  month_day = as.Date(paste0("2000-", format(date, "%m-%d"))),
  percent_rank = percent_rank(num_items),
  items_rel_to_median = num_items/1180.00
)

list(week_labels = week_labels, data_comb = data_comb)
}