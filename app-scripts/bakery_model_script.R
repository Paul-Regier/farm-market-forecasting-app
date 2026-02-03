bakery_pred_2026 = function() {
  
  library(tidyverse)
  library(MASS)
  library(splines)
    
  bakery_data <- here("data","processed","bakery_totals_by_date.rds") %>% read_rds() %>%  
    mutate(
      month_day = as.Date(paste0("2000-", format(date, "%m-%d"))),
      last_sale_min_to_close_cat = case_when(
        last_sale_min_to_close < 100 ~ "<100",
        last_sale_min_to_close < 200 ~ "100-200",
        last_sale_min_to_close < 300 ~ "200-300",
        TRUE ~ ">300"
      ) %>% factor(levels = c("<100","100-200","200-300",">300")),
      sold_out_flag = last_sale_min_to_close >= 300
    )
  
  
  
  #type_names = bakery_data$bakery_type %>% unique()
  type_names <- c(
    "Muffins",     # 1
    "Multigrain",  # 2
    "Pilgrim",     # 3
    "Specialty",   # 4
    "Pie"          # 5
  )
  
  plots     <- vector("list",5)
  data_ind  <- vector("list",5)
  models    <- vector("list",5)
  summaries <- vector("list",5)
  residuals <- vector("list",5)
  
  ## DEBUG
  #i = 1
  for (i in 1:5) {
    type <- type_names[i]
    
    data_ind[[i]] <- bakery_data %>% filter( bakery_type == type )
    models[[i]] <- final_model_nb_ind <-
      glm.nb(
        num_items ~ 
          ns(season_week, df = 3) +
          day_category_both +
          PYO_strawberry +
          Thanksgiving_weekend +
          Thanksgiving_weekend:day_category_both +
          sold_out_flag,
        data = data_ind[[i]]
      )
    summaries[[i]] <- summary(models[[i]])
    
    data_ind[[i]] <- data_ind[[i]] %>%  mutate(
      num_items_pred = predict(
        models[[i]],      # replace with your model object name
        newdata = data_ind[[i]],
        type = "response"  # IMPORTANT: mean count, not log-scale
      ),
      resid_nb = residuals(models[[i]], type = "pearson"), # Variance-Standardized
      num_items_residual = num_items - num_items_pred # raw residuals
    ) %>% relocate( num_items, .before = num_items_pred)
    
    # residuals
    residuals[[i]] <- ggplot(data_ind[[i]],
                             aes(num_items_pred, resid_nb, color = last_sale_min_to_close)) +
      geom_point(alpha = 0.5) +
      geom_smooth() +
      labs(
        title = paste0(type, " - Standardized Residuals vs. Fitted Values")
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks())
    
    plot_data <-   data_ind[[i]] %>% pivot_longer(
      cols = c(num_items_pred, num_items),
      names_to = "model",
      values_to = "num_items"
    )
    
    plots[[i]] <- plot_data %>%
      ggplot(aes(x = date, y = num_items, color = model, 
                 shape =  day_category_both, 
                 alpha = last_sale_min_to_close_cat )) +
      scale_alpha_manual(values = c("<100" = 0.3, "100-200" = 0.5, "200-300" = 0.70, ">300" = 1)) +
      geom_point() +
      facet_wrap(~year, scales = "free_x") +
      labs(
        title = paste0(type," - Prediction vs Actual Sales"),
        y = "number of items",
        x = "date"
      ) 
  }
  
  # Order
  ## 1 = "Muffins"
  ## 2 = "Multigrain"
  ## 3 = "Pilgrim"
  ## 4 = "Specialty"
  ## 5 = "Pie"
  
  data_2026_temp  <- here("data","model","pred_2026.rds") %>% read_rds() %>% 
    dplyr::select(-num_items_pred) %>% 
    filter(year == "2026") %>% 
    mutate(sold_out_flag = FALSE)

  pred_2026_all <- data_2026_temp %>% mutate(
    Muffins     = predict(models[[1]], newdata = data_2026_temp, type = "response") %>% round(),
    Multigrain  = predict(models[[2]], newdata = data_2026_temp, type = "response") %>% round(),
    Pilgrim     = predict(models[[3]], newdata = data_2026_temp, type = "response") %>% round(),
    Specialty   = predict(models[[4]], newdata = data_2026_temp, type = "response") %>% round(),
    Pie         = predict(models[[5]], newdata = data_2026_temp, type = "response") %>% round()
  ) 
  
  # For operational recommendation:
  #- round() if symmetric costs
  #- ceiling() if stockouts are worse than waste
  #- floor() if waste is worse than stockouts
  
  pred_2026_all_select <- pred_2026_all %>% dplyr::select(
    date, Muffins, Multigrain, Pilgrim, Specialty, Pie
  )
  
  plots_bakery_items <- vector("list",5)
  years = paste(2024:2026)
  
  for (i in 1:5) {
    
    type = type_names[i]
    
    # Pre-2026 raw data
    data_pre2026_actual <- data_ind[[i]]  %>% 
      dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, 
                    Thanksgiving_weekend, num_items, sold_out_flag) %>% 
      mutate(
        num_items = num_items %>% as.numeric(),
        data_type = "Actual" %>% factor(levels = c("Actual","Pred")),
        year = year %>% factor(levels = years)
      )
    
    # Pre-2026 predictions
    data_pre2026_pred <- data_ind[[i]]  %>% 
      dplyr::select(year, date, season_week, day_category_both, PYO_strawberry, 
                    Thanksgiving_weekend, num_items_pred, sold_out_flag) %>% 
      rename(
        num_items = num_items_pred
      ) %>% mutate(
        data_type = "Pred" %>% factor(levels = c("Actual","Pred")),
        year = year %>% factor(levels = years),
        sold_out_flag = FALSE
      )
    
    # 2026 Predictions
    data_2026_pred <- data_2026_temp %>%
      mutate(
        num_items = predict(models[[i]], newdata = data_2026_temp, type = "response") %>%
          unname() %>% as.numeric(),
        data_type = "Pred" %>% factor(levels = c("Actual","Pred")),
        year = year %>% factor(levels = years)
      )
    

    
    data_comb <- rbind(data_pre2026_actual, data_pre2026_pred, data_2026_pred) %>%
      mutate(
        month_day = as.Date(paste0("2000-", format(date, "%m-%d"))),
        percent_rank = percent_rank(num_items)
      ) %>%  tidyr::drop_na()
    
    #data_comb$data_type %>% levels()
    
    plots_bakery_items[[i]] <- data_comb %>%
      ggplot(aes(x = date, 
                 y = num_items, 
                 color = data_type, 
                 shape = day_category_both, 
                 alpha = sold_out_flag)) +
      geom_point() +
      scale_size_manual(
        values = c("Actual" = 2, "Predicted" = 1),
        guide  = "none"
      ) +
      scale_alpha_manual(values = c("FALSE" = 0.4, "TRUE" = 1)) +
      scale_color_manual(
        values = c(Actual = "#D55E00",Pred = "black"),
        labels = c(Actual = "Actual", Pred = "Predicted")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 10),
        legend.text  = element_text(size = 8)) +
      facet_wrap(~year, scales = "free_x") +
      labs(
        title = paste0(type," - Predicted vs Actual Sales"),
        y = "number of items",
        x = "date",
        alpha = "Sold Out Flag",
        color = "",
        shape = "Day Category"
      )
  }
  
  list(pred_2026 = pred_2026_all_select, plots_bakery_items = plots_bakery_items)
}


