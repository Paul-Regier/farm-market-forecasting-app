# at top of ui.R and server.R (if you insist)
#if (!exists(".GLOBAL_LOADED", envir = .GlobalEnv)) {
#  source("global.R", local = FALSE)
#  assign(".GLOBAL_LOADED", TRUE, envir = .GlobalEnv)
#}
# ===
# --- UI ----
# ===
tags$style(HTML("
  .irs {
    margin-bottom: 0 !important;
    margin-top: 2px !important;
  }
"))

ui <- page_navbar(
  title = "Farm Market Dashboard - Public Demo",
  theme = bs_theme(version = 5),
  
  # 1. Bakery Orders -----
  nav_panel(
    "Bakery Orders",
    layout_sidebar(
      sidebar = sidebar(
        width = "220px",   # try 220???260px
        dateInput(
          inputId = "order_date",
          label   = "Order date", # add note "default today",
          value   = as.Date("2026-06-04") # change to Sys.Date()
        ),
        helpText("Please verify order and delivery dates are correct."),
        helpText("Note: Orders are for the following day, except on Saturday the delivery date will be the following Monday."),
        uiOutput("bread_fax_timestamp_ui")
      ),
      
      # main content (vertical stack)
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          card_header("Weather prediction summary"),
          uiOutput("order_delivery_dates"),
          uiOutput("weather_summary_ui"),
          hr(),
          tags$div(
            tags$strong("Note on Weather Categories:"),
            tags$ul(
              tags$li(tags$strong("Bad:"), " precip chance > 50%"),
              tags$li(tags$strong("Neutral:"), " max wind ≥ 50 km/h OR max temp < 12 (C) OR > 32 (C)"),
              tags$li(tags$strong("Good:"), " otherwise")
            )
          ),
          hr(),
          tags$strong("Predictions Label Key:"),
          tags$ul(
            tags$li("Estimated Discount Retained = Unsold Discount / 2 (rounding down)"),
            tags$li("Estimated Total Inventory Retained = Unsold Fresh + Estimated Discount Retained."),
            tags$li("Recommended Order = Weather Adjusted Prediction - Estimated Total Inventory Retained.")
          ),
          tags$strong("Holiday Note:"),
          tags$ul(
            tags$li("Some product estimates are likely too low on holidays due to lower holiday sales in 2024."),
            tags$li("For holidays, use the seasonal Bakery Projections plots in the next tab to cross-check and revise orders.")
          )
        ),
        
        card(
          card_header("Bakery Predictions"),
          uiOutput("reprint_delivery_date_ui"),
          tags$div(
            style = "
                max-height: 70vh;
                overflow-y: auto;
                padding-right: 0.5rem;
              ",
            gt_output("bakery_pred_table"),
            tags$div(
              style = "margin-top: 0.5rem;",
              tags$em("* Pumpkin pies are ordered manually; model not available.")
            ),
            hr(),
            
            uiOutput("actual_order_ui")
          )
        )
      )
    )
  ),
  
  # 2. Bakery Projections -----
  nav_panel(
    "Bakery Projections",
    card(
      card_header("Historical and Predicted Bakery Sales"),
      #tags$div(
      #  style = "margin-top: 0.5rem;",
      #  tags$em("Projected sales indicated in blue. Actual sales indicated in black")
      #),
      #uiOutput("bakery_plots_ui")
      selectInput(
        inputId = "bakery_type_plot",
        label = "Select bakery category:",
        choices = Bakery_Types_Names,
        selected = "Muffins"
      ),
      card(
        full_screen = TRUE,
        plotOutput("bakery_plot_selected", height = "360px")
      )
    )
  ),
  
  # 3. Seasonal Market Projections -----
  nav_panel(
    "Overall Traffic Projections",
    card(
      tags$p(
        tags$strong("Overall Traffic Projections:"),
        " using ",
        tags$em("total number of items sold"),
        " as proxy for traffic, relative to 2024–2025 median sales (1180 items)."
      ),
      
      # Label + slider side-by-side
      fluidRow(
        column(
          width = 4,
          tags$strong("Season week range:"),
          tags$div(
            style = "margin-top: 4px;",
            uiOutput("week_range_label")
          )
        ),
        column(
          width = 4,
          sliderInput(
            inputId = "week_range",
            label   = NULL,
            min     = 1,
            max     = max_week,
            value   = c(1, min(22, max_week)),
            step    = 1,
            sep     = "",
            width   = "80%"
          )
        )
      ),
      plotOutput("seasonal_plot", height = "450px")
    )
  ),
  
  # 4. General Product Projections -----
  nav_panel(
    "General Product Projections",
    
    # ---- Left pane: selectors + ranked list ----
    layout_columns(
      col_widths = c(4, 8),
      
      # ---- Left pane: selectors + ranked list ----
      card(
        card_header("Select Category"),
          selectInput(
            "cat", "Category",
            choices = sort(unique(items_by_date$category)),
            selected = sort(unique(items_by_date$category))[1]
          ),
        uiOutput("category_header_ui"),
        uiOutput("category_ranked_ui")
      ),
      # ---- Right pane: product outputs ----
      card(
        card_header("Select a Product"),
        fluidRow(
          column(
            width = 4,
            selectizeInput(
              "prod", "Product",
              choices = NULL, selected = NULL,
              options = list(
                placeholder = "Type to search…"
              )
            )
          ),
          column(
            width = 8,
            radioButtons(
              "general_view",
              "Show:",
              choices = c(
                "Weekly totals"        = "weekly_totals",
                "Projected totals"    = "projected_totals",
                "Predicted vs Actual" = "pred_plot",
                "Model summary"       = "model_summary",
                "Residuals"           = "residuals_plot"
              ),
              selected = "weekly_totals",
              inline = TRUE
            )
          )
        ),
        uiOutput("general_product_main_ui")
      )
      
    )
  )
)
