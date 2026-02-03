# ===
# --- Server ----
# ===
server <- function(input, output, session) {

  # 1. Bakery Orders Reactives ----
  # Bread Fax Reactive
  bread_fax_all_reactive <- reactiveVal(tibble())
  bread_fax_last_updated <- reactiveVal(NA)
  
  refresh_bread_fax <- function() {
    bf <- tryCatch(load_google_ref(), error = function(e) tibble())
    bread_fax_all_reactive(bf)
    bread_fax_last_updated(Sys.time())
    invisible(bf)
  }
  
  output$bread_fax_timestamp_ui <- renderUI({
    ts <- bread_fax_last_updated()
    
    if (is.na(ts)) {
      tags$div(style = "margin-top: 0.25rem; font-size: 0.85em; color: #6c757d;",
               "This page last refreshed: (not yet loaded)")
    } else {
      tags$div(style = "margin-top: 0.25rem; font-size: 0.85em; color: #6c757d;",
               paste0("This page last refreshed: ", format(ts, "%a %b %d, %I:%M %p")))
    }
  })
  
  
  # Delivery Date
  delivery_date <- reactive({
    d <- as.Date(input$order_date)
    
    if (weekdays(d) == "Saturday") {
      d + 2   # skip Sunday ??? deliver Monday
    } else {
      d + 1
    }
  })
  
  # load once on page open
  observeEvent(TRUE, { refresh_bread_fax() }, once = TRUE)
  
  # auto-refresh every 5 minutes
  observe({
    invalidateLater(5 * 60 * 1000, session)
    refresh_bread_fax()
  })
  
  
  output$order_delivery_dates <- renderUI({
    tags$div(
      tags$p(
        tags$strong("Order date: "), tags$br(),
        as.character(as.Date(input$order_date))," - ",tags$strong(weekdays(as.Date(input$order_date)))),
      tags$p(tags$strong("Delivery date: "), tags$br(),
             as.character(delivery_date()), " - ",tags$strong(weekdays(delivery_date())))
    )
  })
  
  bread_fax_day <- reactive({
    bf_all <- bread_fax_all_reactive()
    req(nrow(bf_all) > 0)
    bf_all %>% filter(report_date == as.Date(input$order_date))
  })
  
  weather_status <- reactive({
    bf <- bread_fax_day()
    if (nrow(bf) == 0) return(NA_character_)
    
    case_when(
      bf$precip_chance_pct > 50 ~ "Bad",
      (bf$max_temp_c < 12 | bf$max_temp_c > 32) | (bf$max_wind_kmh >= 50) ~ "Neutral",
      TRUE ~ "Good"
    )
  })
  
  output$weather_summary_ui <- renderUI({
    bf <- bread_fax_day()
    ws <- weather_status()
    
    if (nrow(bf) == 0) {
      return(tags$div(
        tags$p(tags$strong("No Bread Fax / weather row found for this date.")),
        tags$p("Check the Google Sheet for the selected order date.")
      ))
    }
    
    owner_paragraph <- base::switch(
      ws,
      "Bad"     = "Forecast suggests higher weather risk (precipitation is likely). Plan for lower traffic. Weather adjusted prediction advises conservative ordering.",
      "Neutral" = "Forecast suggests mild weather risk (wind or temperature may affect traffic). Weather adjusted prediction advises near baseline ordering.",
      "Good"    = "Forecast looks favorable. Plan for higher traffic.  Weather adjusted prediction advises above baseline ordering.",
      "Forecast unavailable."
    )
    
    tags$div(
      tags$p(tags$strong("Weather Report:"), paste0("(Submitted by ", bf$staff_name, ")")),
      tags$p(owner_paragraph),
      tags$ul(
        tags$li(paste0("Max temp (C): ", bf$max_temp_c)),
        tags$li(paste0("Chance of precip (%): ", bf$precip_chance_pct)),
        tags$li(paste0("Max wind (km/h): ", bf$max_wind_kmh))
      ),
      tags$p(tags$strong("Weather Status Prediction: "), ws),
      tags$p(tags$strong("Sold-out notes: "), tags$br(), bf$sold_out_notes),
      tags$p(tags$strong("Additional notes: "), tags$br(), bf$additional_comments)
    )
  })
  
  # Bakery predictions table (delivery_date-based)
  bakery_pred_df <- reactive({
    dd <- delivery_date()
    
    # baseline predictions for that delivery date
    pred_2026 <- pred_2026_all %>%
      filter(date == dd)
    
    if (nrow(pred_2026) == 0) {
      return(tibble(
        Type = Bakery_Types_Names,
        Baseline = NA_real_,
        Pred_bad = NA_real_,
        Pred_neutral = NA_real_,
        Pred_good = NA_real_,
        Suggested = NA_real_,
        Unsold_Fresh = NA_real_,
        Unsold_Discount = NA_real_,
        Unsold_Total = NA_real_,
        Suggested_Order = NA_real_
      ))
    }
    
    # pivot to Type/Baseline
    pred_2026_t <- pred_2026 %>%
      dplyr::select(date, Muffins, Multigrain, Pilgrim, Specialty, Pie) %>%
      t() %>%
      as.data.frame() %>%
      slice(-1) %>%
      rownames_to_column("Type")
    names(pred_2026_t)[2] <- "Baseline"
    
    pred_2026_t <- pred_2026_t %>%
      mutate(
        Baseline     = as.numeric(Baseline),
        Pred_bad     = ceiling(0.90 * Baseline),
        Pred_neutral = ceiling(1.04 * Baseline),
        Pred_good    = ceiling(1.07 * Baseline),
        Suggested = case_when(
          weather_status() == "Good"    ~ Pred_good,
          weather_status() == "Neutral" ~ Pred_neutral,
          TRUE                          ~ Pred_bad
        )
      )
    
    # Unsold inventory from Bread Fax
    bf <- bread_fax_day()
    
    order_wide <- if (nrow(bf) == 0) {
      tibble(
        Type = Bakery_Types_Names,
        Unsold_Fresh = NA_real_,
        Unsold_Discount = NA_real_,
        Unsold_Total = NA_real_
      )
    } else {
      ow <- bf %>%
        dplyr::select(ends_with("_fresh"), ends_with("_discounted")) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("bakery_type", "status"),
          names_pattern = "^(.*)_(fresh|discounted)$",
          values_to = "qty"
        ) %>%
        mutate(qty = as.numeric(qty)) %>%
        pivot_wider(names_from = status, values_from = qty) %>%
        mutate(sum = fresh + discounted)
      
      names(ow) <- c("Type", "Unsold_Fresh", "Unsold_Discount", "Unsold_Total")
      
      # Preserve required order + add manual pumpkin row
      ow$Type <- Bakery_Types_Names
      ow
    }
    
    pred_out <- pred_2026_t %>%
      full_join(order_wide, by = "Type") %>%
      mutate(
        Estimated_Discount_Retained = floor(Unsold_Discount / 2),
        Estimated_Total_Inventory_Retained = Unsold_Fresh + Estimated_Discount_Retained,
        Suggested_Order = pmax(0, Suggested - Estimated_Total_Inventory_Retained)
      )
    
    pred_out
  })
  
  output$reprint_delivery_date_ui <- renderUI({
    tags$p(tags$strong("Delivery date: "), 
          as.character(delivery_date()), " - ",tags$strong(weekdays(delivery_date())))
  })
  
  output$bakery_pred_table <- render_gt({
    df <- bakery_pred_df()
    
    # Create transposed display like in Rmd
    gt_df <- df %>%
      dplyr::select(
        Type,
        Baseline, Pred_bad, Pred_neutral, Pred_good, Suggested,
        Unsold_Fresh, Unsold_Discount,
        Estimated_Discount_Retained,
        Estimated_Total_Inventory_Retained,
        Suggested_Order
      ) %>%
      column_to_rownames("Type") %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Type")
    
    
    # Rename the first rows to match your Rmd style
    if (nrow(gt_df) >= 10) {
      gt_df$Type[2:10] <- c(
        "Predicted - Bad Weather",
        "Predicted - Neutral Weather",
        "Predicted - Good Weather",
        "Weather Adjusted Prediction",
        "Unsold Fresh",
        "Unsold Discount",
        "Estimated Discount Retained",
        "Estimated Total Inventory Retained",
        "Recommended Order"
      )
    }
    
    gt(gt_df) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = c(1, 5, 9, 10))
      ) %>%
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(rows = nrow(gt_df))
      )
  })
  
  # Helper for recommended order values - 1/30/2026
  recommended_order_vals <- reactive({
    df <- bakery_pred_df()
    
    # Pull recommended order by type
    rec <- df %>%
      dplyr::select(Type, Suggested_Order) %>%
      dplyr::mutate(Suggested_Order = as.numeric(Suggested_Order))
    
    # Named vector (Type -> value)
    setNames(rec$Suggested_Order, rec$Type)
  })
  
  # Add actual order row - 1/30/2026
  output$actual_order_ui <- renderUI({
    rec <- recommended_order_vals()
    
    # defaults (fall back to 0 if NA)
    def <- function(x) ifelse(is.na(x), 0, x)
    
    tagList(
      tags$h5("Final Order"),
      tags$p(
        style = "margin-top: -0.5rem; color: #6c757d;",
        "Defaults to Recommended Order. Adjust as needed, then submit to \"Bread Fax Receipts\" Google Sheet."
      ),
      
      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        
        numericInput("actual_muffins",     "Muffins",     value = def(rec[["Muffins"]]),     min = 0, step = 1),
        numericInput("actual_multigrain",  "Multigrain",  value = def(rec[["Multigrain"]]),  min = 0, step = 1),
        numericInput("actual_pilgrim",     "Pilgrim",     value = def(rec[["Pilgrim"]]),     min = 0, step = 1),
        
        numericInput("actual_specialty",   "Specialty",   value = def(rec[["Specialty"]]),   min = 0, step = 1),
        numericInput("actual_pie",         "Pie",         value = def(rec[["Pie"]]),         min = 0, step = 1),
        numericInput("actual_pumpkinpies", "Pumpkin Pies*", value = 0, min = 0, step = 1)
      ),
      
      actionButton("submit_actual_order", "Submit"),
      tags$span(style = "margin-left: 0.75rem;"),
      textOutput("actual_order_status")
    )
  })
  
  # Auto-update defaults - 1/30/2026
  observeEvent(recommended_order_vals(), {
    rec <- recommended_order_vals()
    def <- function(x) ifelse(is.na(x), 0, x)
    
    updateNumericInput(session, "actual_muffins",     value = def(rec[["Muffins"]]))
    updateNumericInput(session, "actual_multigrain",  value = def(rec[["Multigrain"]]))
    updateNumericInput(session, "actual_pilgrim",     value = def(rec[["Pilgrim"]]))
    updateNumericInput(session, "actual_specialty",   value = def(rec[["Specialty"]]))
    updateNumericInput(session, "actual_pie",         value = def(rec[["Pie"]]))
    # pumpkin pies stays manual; leave as-is
  }, ignoreInit = TRUE)
  
  actual_order_status <- reactiveVal("")
  
  output$actual_order_status <- renderText(actual_order_status())
  
  observeEvent(input$submit_actual_order, {
    #  build row
    new_row <- tibble::tibble(
      `Timestamp`     = as.POSIXct(Sys.time(), tz = "America/Chicago"),
      `Delivery Date` = as.character(delivery_date()),
      `Muffins`       = as.integer(input$actual_muffins),
      `Multigrain`    = as.integer(input$actual_multigrain),
      `Pilgrim`       = as.integer(input$actual_pilgrim),
      `Specialty`     = as.integer(input$actual_specialty),
      `Pie`           = as.integer(input$actual_pie),
      `Pumpkin Pies*` = as.integer(input$actual_pumpkinpies)
    )
    
    # append to Google Sheet
    SHEET_ID <- "1mPhrj-K3EcQd408sD4dL7I-pDWL7NR-3ubBxPLoti6I"
    SHEET_TAB <- "Order Receipts"  # set to "Sheet1" (or your tab name) if needed
    
    ok <- tryCatch({
      if (is.null(SHEET_TAB)) {
        googlesheets4::sheet_append(ss = SHEET_ID, data = new_row)
      } else {
        googlesheets4::sheet_append(ss = SHEET_ID, sheet = SHEET_TAB, data = new_row)
      }
      
      TRUE
    }, error = function(e) {
      actual_order_status(paste0("Submit failed: ", conditionMessage(e)))
      FALSE
    })
    
    if (ok) {
      actual_order_status("Submitted ✓")
    }
  })
  
  

  # 2. Seasonal Bakery Plots nav_panel -----

  output$bakery_plot_selected <- renderPlot({
    req(input$bakery_type_plot)
    
    # Map the UI choice to the plot index (assumes plots_bakery is in this order)
    plot_index <- match(input$bakery_type_plot, c("Muffins","Multigrain","Pilgrim","Specialty","Pie"))
    
    # Pumpkin pies are manual ??? no model plot
    validate(
      need(!is.na(plot_index), "Pumpkin pies are ordered manually; no projection plot available.")
    )
    
    plots_bakery[[plot_index]]
  }, res = 110)
  
  
  # 3. Seasonal Market Projections nav_panel -----
  data_comb_filter <- reactive({
    rng <- input$week_range
    data_comb %>%
      filter(season_week >= rng[1], season_week <= rng[2])
  })
  
  output$week_range_label <- renderUI({
    rng <- input$week_range
    wl_min <- week_labels %>% filter(season_week == rng[1]) %>% pull(week_label)
    wl_max <- week_labels %>% filter(season_week == rng[2]) %>% pull(week_label)
    
    tags$p(
      style = "margin: 0; font-size: 0.9em;",
      tags$strong("Showing: "),
      paste0(ifelse(length(wl_min) == 0, paste0("week ", rng[1]), wl_min),
             "  to  ",
             ifelse(length(wl_max) == 0, paste0("week ", rng[2]), wl_max))
    )
  })
  
  output$seasonal_plot <- renderPlot({
    df <- data_comb_filter()
    
    ggplot(df, aes(
      x = date,
      y = items_rel_to_median,
      color = data_type,
      shape = day_category_both,
      alpha = data_type
    )) +
      geom_point() +
      scale_color_manual(
        values = c(Actual = "#D55E00",Pred = "#4D4D4D"),
        labels = c(Actual = "Actual", Pred = "Predicted")
      ) +
      scale_shape_discrete(na.translate = FALSE) +
      scale_alpha_manual(values = c(Actual = 0.3, Pred = 0.9)) +
      facet_wrap(~year, scales = "free_x") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "",
        title = "Actual and Predicted Sales by Year",
        y = "traffic relative to median",
        color = "",
        shape = "Day Category"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 10)
      ) +
      guides(alpha = "none")
  }, res = 100)
  
  
  # 4. General Product Projections nav_panel -----
  # Reactive "filtered products" list based on selected input$cat
  items_in_cat <- reactive({
    req(input$cat)
    items_by_date %>%
      filter(category == input$cat) %>%
      distinct(item_name) %>%
      arrange(item_name) %>%
      pull(item_name)
  })
  
  # update product dropdown whenever category changes
  observeEvent(input$cat, {
    rp <- item_totals_ranked()
    
    updateSelectizeInput(session, "prod",
                      choices  = rp$item_name,
                      selected = rp$item_name[1], 
                      server = TRUE
    )
  })
  
 
  output$category_header_ui <- renderUI({
    req(input$cat)
    
    tags$p(
      tags$strong(input$cat),
      " – items ranked by total gross sales (2024-2025)"
    )
  })
  
  item_totals_ranked <- eventReactive(input$cat, {
    req(input$cat)
    
    items_by_date %>%
      filter(category == input$cat) %>%
      group_by(item_name) %>%
      summarise(
        total_gross_sales = sum(gross_sales, na.rm = TRUE),
        total_items       = sum(num_items,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_gross_sales))
  })
  
  output$category_ranked_ui <- render_gt({

    gt(item_totals_ranked()) %>%
      fmt_currency(
        columns = total_gross_sales,
        currency = "USD",
        decimals = 0
      ) %>%
      fmt_number(
        columns = total_items,
        decimals = 0
      ) %>%
      cols_label(
        item_name = "Product",
        total_gross_sales = "Total Gross Sales",
        total_items = "Total Items Sold"
      )
  })
  
  # General Product Out Function ----
  general_product_out <- reactive({
    req(input$cat, input$prod)
    
    # Returns null if model didn't run
    general_product_analysis(items_by_date, input$prod, input$cat, 2026)
  })
  
  output$general_product_main_ui <- renderUI({
    req(input$general_view)
    
    switch(
      input$general_view,
      weekly_totals     = gt_output("general_product_weekly_actual_tbl"),
      projected_totals  = gt_output("general_product_weekly_proj_tbl"),
      pred_plot         = plotOutput("general_product_plot", height = "450px"),
      residuals_plot    = plotOutput("general_product_resid_plot", height = "450px"),
      model_summary     = verbatimTextOutput("general_product_model_summary")
    )
  })
  
  # Historical Weekly Totals
  output$general_product_weekly_actual_tbl <- render_gt({
    req(input$prod)
    
    weekly_totals <- items_by_date_weekly %>%
      filter(item_name == input$prod) %>%
      pivot_wider(
        names_from = year,
        values_from = total_items,
        values_fill = 0
      ) %>%
      mutate(season_week = as.character(season_week))
    
    total_row <- weekly_totals %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
      mutate(season_week = "Total", item_name = "Total") %>%
      relocate(season_week)
    
    weekly_totals <- bind_rows(weekly_totals, total_row)
    last_row <- nrow(weekly_totals)
    
    gt(weekly_totals, rowname_col = "season_week") %>%
      tab_stubhead(label = "Week") %>%
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(rows = last_row)
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = last_row)
      )  %>%
      cols_label(
        item_name = "Product"
      )
  })
  
  # Historical With next year's predicted totals
  output$general_product_weekly_proj_tbl <- render_gt({
    out <- general_product_out()
    
    validate(
      need(!is.null(out) && isTRUE(out$model_available),
           "Model not available for this product (insufficient data or fit failure).")
    )
    
    tbl <- out$weekly_totals
    
    total_row <- tbl %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
      mutate(season_week = "Total") %>%
      relocate(season_week)
    
    tbl2 <- bind_rows(
      tbl %>% mutate(season_week = as.character(season_week)),
      total_row
    )
    
    last_row <- nrow(tbl2)
    
    gt(tbl2, rowname_col = "season_week") %>%
      tab_stubhead(label = "Season Week") %>%
      fmt_number(columns = where(is.numeric), decimals = 0) %>%
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(rows = last_row)
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = last_row)
      ) %>%
      tab_source_note(md("Projected totals shown where model is available."))
  })
  
  
  output$general_product_plot <- renderPlot({
    out <- general_product_out()
    validate(need(!is.null(out) && isTRUE(out$model_available), "Model Not Available"))
    out$pred_plot
  })
  
  output$general_product_resid_plot <- renderPlot({
    out <- general_product_out()
    validate(need(!is.null(out) && isTRUE(out$model_available), "Model Not Available"))
    out$residuals_plot
  })

  
  output$general_product_model_summary <- renderPrint({
    out <- general_product_out()
    validate(need(!is.null(out) && isTRUE(out$model_available), "Model Not Available"))
    summary(out$item_model)
  })

}
