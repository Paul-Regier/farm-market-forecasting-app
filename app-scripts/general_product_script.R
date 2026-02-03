library(tidyverse)


# Debugging
if (FALSE) {
  #items <- items_by_date %>% count(item_name)
  #cat = sort(unique(items_by_date$category))[1]
  #item_name_select = "Reheating Pumpkin Pies"
  #name = "Reheating Pumpkin Pies"
  name = "Reesor's Sweet Corn Dozen (13)"
  YEAR = "2026"
  cat = "Produce"
}

general_product_analysis = function(items_by_date, name, cat, YEAR = 2026) {
  
  YEAR = as.character(YEAR)
  YEARS = 2024:as.numeric(YEAR)
  item_data <- items_by_date %>% filter(item_name == name & 
                                        category == cat) %>% 
    mutate(
      day_category_both = day_category_both %>% factor(levels = DAY_CAT_LEVELS)
    )
  
  if (nrow(item_data) < 150) {
    return(NULL)
  }
  # Else
  tryCatch(
    {
      library(MASS)
      library(splines)
      item_model <-
          glm.nb(
            num_items ~ 
              ns(season_week, df = 3) +
              day_category_both +
              PYO_strawberry +
              Thanksgiving_weekend +
              Thanksgiving_weekend:day_category_both +
              sold_out_flag,
            data = item_data
          )
      model_summary = summary(item_model)
      
      item_data <- item_data %>%  mutate(
        num_items_pred = predict(
          item_model,      # replace with your model object name
          newdata = item_data,
          type = "response"  # IMPORTANT: mean count, not log-scale
        ),
        resid_nb = residuals(item_model, type = "pearson"), # Variance-Standardized
        num_items_residual = num_items - num_items_pred # raw residuals
      ) %>% relocate( num_items, .before = num_items_pred)
      
      # residuals
      residuals_plot <- ggplot(item_data,
                               aes(num_items_pred, resid_nb, color = last_sale_min_to_close)) +
        geom_point(alpha = 0.5) +
        geom_smooth() +
        labs(
          title = paste0(name, " - Standardized Residuals vs. Fitted Values")
        ) +
        scale_y_continuous(breaks = scales::pretty_breaks())
    
      
      plot_data <-   item_data %>% pivot_longer(
        cols = c(num_items_pred, num_items),
        names_to = "model",
        values_to = "num_items"
      )
      
      model_plot <- plot_data %>%
        ggplot(aes(x = date, y = num_items, color = model, 
                   shape =  day_category_both, 
                   alpha = last_sale_min_to_close_cat )) +
        scale_alpha_manual(values = c("<100" = 0.3, "100-200" = 0.5, "200-300" = 0.70, ">300" = 1)) +
        geom_point() +
        facet_wrap(~year, scales = "free_x") +
        labs(
          title = paste0(name," - Prediction vs Actual Sales"),
          y = "number of items",
          x = "date"
        )
      
      season_weeks <- item_data$season_week %>% unique()
      
      # Update Pre_2026 data
      data_next_temp  <- here("data","model","pred_2026.rds") %>% read_rds() %>% 
        dplyr::select(-num_items_pred) %>% 
        filter(year == YEAR,
               season_week %in% season_weeks) %>% 
        mutate(
          sold_out_flag = FALSE,
          day_category_both = day_category_both %>% factor(levels = item_model$xlevels$day_category_both)
        )
      
      #data_next_temp$day_category_both %>% levels()
      #item_model$xlevels$day_category_both
    
      data_next_pred <- data_next_temp %>% 
        filter( season_week %in% season_weeks) %>% 
        mutate(
          num_items = predict(item_model, newdata = data_next_temp, type = "response") %>% 
            unname() %>% as.numeric(),
          year = year %>% factor(levels = YEARS),
          data_type = "Pred" %>% factor(levels = c("Actual","Pred"))
        )
    
    # DEBUG
    # writexl::write_xlsx(data_2026_temp, "model-data/pred_2026.xlsx")
    
    # For operational recommendation:
    #- round() if symmetric costs
    #- ceiling() if stockouts are worse than waste
    #- floor() if waste is worse than stockouts
    
      # Pre-2026 raw data
      data_prior_actual <- item_data  %>% 
        dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, 
                      Thanksgiving_weekend, num_items, sold_out_flag) %>% 
        mutate(
          num_items = num_items %>% as.numeric(),
          data_type = "Actual" %>% factor(levels = c("Actual","Pred")),
          year = year %>% factor(levels = YEARS)
        ) %>% relocate(num_items, .before = data_type)
      
      # Pre-2026 predictions
      data_prior_pred <- item_data %>% 
        dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, 
                      Thanksgiving_weekend, num_items_pred, sold_out_flag) %>% 
        rename(
          num_items = num_items_pred
        ) %>% mutate(
          data_type = "Pred" %>% factor(levels = c("Actual","Pred")),
          year = year %>% factor(levels = YEARS),
          sold_out_flag = FALSE,
          num_items = num_items %>%  unname() %>% as.numeric()
        ) %>% relocate(num_items, .before = data_type)
    
      #colnames(data_prior_actual)
      #colnames(data_prior_pred)
      #colnames(data_next_pred)
      data_comb <- rbind(data_prior_actual, data_prior_pred, data_next_pred) %>%
        mutate(
          month_day = as.Date(paste0("2000-", format(date, "%m-%d"))),
          percent_rank = percent_rank(num_items)
        ) %>%  tidyr::drop_na()
      
      ## DEBUG Up to here
      #data_comb$data_type %>% levels()
      
      pred_plot <- data_comb %>%
        ggplot(aes(x = date, 
                   y = num_items, 
                   color = data_type, 
                   shape = day_category_both, 
                   alpha = sold_out_flag)) +
        geom_point() +
        scale_alpha_manual(values = c("FALSE" = 0.4, "TRUE" = 1)) +
        scale_color_manual(
          values = c(Actual = "black", Pred = "blue"),
          labels = c(Actual = "Actual", Pred = "Predicted")
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~year, scales = "free_x") +
        labs(
          title = paste0(name," - Predicted vs Actual Sales"),
          y = "number of items",
          x = "date",
          alpha = "Sold Out Flag",
          color = "",
          shape = "Day Category"
        )
      weekly_totals <- data_comb %>% group_by(year, data_type, season_week) %>% 
        summarize(
          total_sold = sum(num_items),
          .groups = "drop"
        )
      df_wide <- weekly_totals %>%
        mutate(year_type = paste(year, data_type, sep = "_")) %>%
        dplyr::select(season_week, year_type, total_sold) %>%
        tidyr::pivot_wider(
          names_from  = year_type,
          values_from = total_sold
        ) %>%
        arrange(season_week)
      
      out = list(model_available = TRUE,
                 item_data = item_data,
                 item_model = item_model,
                 residuals_plot = residuals_plot,
                 model_plot = model_plot,
                 pred_plot = pred_plot,
                 weekly_totals = df_wide)
    }, 
    error = function() {
      out = NULL
    }
  )
  out
}



