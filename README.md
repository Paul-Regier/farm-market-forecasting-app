# Small-Scale Farmers Market Forecasting and Ordering Dashboard
This project began after a conversation with a friend who purchased a small, on-site farmers market business (~40 employees). At the time, the business had accumulated two years of point-of-sale data that was used primarily for tax reporting, with little impact on day-to-day operational decisions.

We identified an immediate opportunity: improving ordering and tracking for perishable bakery goods. Bakery orders were based largely on guesswork, leading to frequent stockouts of high-demand items and over-ordering of others—both of which created avoidable cost and operational friction.

What started as a focused effort to improve bakery ordering evolved into a broader decision-support system for forecasting, ordering, staffing, and longer-term planning.


---

## Live Dashboard and Operational Data

### Public Dashboard

**Live Shiny dashboard:**  
https://paulsshinyapps.shinyapps.io/R-market-forecasting-app/

This public-facing dashboard provides interactive forecasts, historical summaries, and decision-support visualizations for small-scale farmers market operations. It is intended for exploratory analysis and daily, weekly, and seasonal operational planning.

---

### Operational Google Sheets

The dashboard integrates with Google Sheets used for daily operations and record-keeping. These Sheets serve as lightweight, human-readable interfaces between staff workflows and the forecasting pipeline.

**Bread Fax Report**  
Used for importing prior-day stock levels and weather-informed predictions into the ordering workflow.  
https://docs.google.com/spreadsheets/d/1WHhSdAPJeDIjq34lDkJ3hY_z7_5Sonxis6bb2uj2TkY/edit?gid=1028908163#gid=1028908163

**Bread Fax Receipts**  
Used for documenting finalized bakery orders and maintaining a historical record of order decisions.  
https://docs.google.com/spreadsheets/d/1mPhrj-K3EcQd408sD4dL7I-pDWL7NR-3ubBxPLoti6I

> Note: These reports are maintained as separate tabs within a single Google Sheet to simplify access control, versioning, and operational handoff between staff.


---

## Decision Supports Implemented

### Decision Support 1: Daily Bakery Ordering Dashboard

My initial objective was to build a centralized dashboard to support **daily bakery ordering**, based on predicted demand and carryover inventory.

This supports two core business decisions:
- Improving the bottom line on bakery sales by reducing stockouts and waste
- Freeing up owner and staff time -- previously split between manual tracking or fully delegating ordering to the bakery supplier -- so attention could be focused on higher-level business decisions

---

### Decision Support 2: Forecast Transparency and Model Awareness

As I began presenting model results to the business owner, I began providing **intuitive visualizations of historical and projected bakery sales**. These help the user understand:
- Key drivers of demand
- Where models perform well
- Where uncertainty is higher

Sales patterns exhibit strong variability due to seasonality, holidays, school professional days, and special market events (e.g., pick-your-own (PYO) strawberry, PYO pumpkins, corn maze). Accurately gathering this data and making it visible was essential for building trust in the forecasts and supporting informed overrides when needed.

---

### Decision Support 3: Store Traffic and Staffing Planning

Given the strong seasonal and event-driven variation in sales, and the project expanded to include **modeling total store traffic**, using total items sold as a proxy.

This supports:
- Staffing decisions during peak and low-demand periods
- Planning around special events and seasonal transitions

---

### Decision Support 4: Item-Level Sales Tracking and Planning

Finally, the modeling framework developed for bakery forecasting was generalized to support **historical and (where data allows) predicted sales for individual items**.

This capability is particularly useful for:
- Planning weekly produce orders for items grown off-site
- Informing succession planting strategies for on-site crops (e.g., corn, green beans, strawberries)

The application code supporting these decision supports is located in `app/`.

---

## The Decisions Being Supported

At its core, this project supports **better prediction and ordering of market goods**, with particular emphasis on perishable bakery and produce items.

Once early versions of the dashboard were deployed, the business owner identified additional value: the system also supports lead bakery staff -- both in-house and external partners -- in anticipating demand and planning raw ingredient needs for upcoming seasonal shifts.

---

## Data & Signals

The forecasting and decision-support pipeline integrates:

- Point-of-sale transaction data  
- Historical weather data  
- Public holiday and school professional-day calendars  
- Market-specific special events calendar  

The data pipeline was designed to allow easy extension as new years of data become available. Relevant scripts can be found in `modeling/`.

**Note on Deployment and Data Privacy**  
The production version of this project operates on proprietary business data and cannot be shared publicly. This repository includes randomized data that preserves the original statistical structure. To make the project suitable for public sharing, the raw transactional data has been lightly anonymized: direct identifiers (employee, device, merchant, and card-related fields) were removed and order numbers cryptographically hashed using a one-way function.; a small proportion of rows were randomly dropped; and payment timestamps were jittered by a small random offset (on the order of minutes) to preserve realistic temporal patterns while preventing exact record reconstruction. These steps are implemented at the end of `01_data_preparation.R`, after which the full analysis pipeline is rerun end-to-end on the anonymized dataset.

---

## Analytical Approach

The analytical workflow is documented in a series of exploratory notebooks located in `analysis/`.

These notebooks are intentionally left in a relatively raw form. They reflect:
- A short turnaround development cycle (several days over a winter break)
- Iterative model refinement driven by business feedback
- Rapid validation and deployment priorities

Next January, I plan to refit models with the data from next market season, revisiting assumptions, and improving feature engineering as additional data become available (e.g., better stock tracking, explicit sell-out flags, and more detailed special event indicators).

The modeling process followed these steps:

1. Build a baseline model for total store traffic to support downstream feature engineering  
2. Construct order multipliers based on key weather features and validate them against baseline indicators  
3. Develop and compare a range of bakery-order models incorporating seasonality, date indicators, and market events, identifying interaction terms that substantially improved accuracy  
4. Validate model assumptions through residual diagnostics and comparative performance analysis  
5. Deploy models within the Shiny application for operational use  

---

## Outputs That Drive Decisions

The primary outputs of this project are:
- Daily bakery order recommendations  
- Visual diagnostics supporting forecast interpretation  
- Store traffic estimates for staffing decisions  
- Item-level historical and projected sales summaries  

Together, these outputs are designed to support **timely, high-impact operational decisions under real-world uncertainty**.

---

## Project Characteristics

This project reflects a realistic applied analytics context:

- Messy, incomplete operational data  
- Fast turnaround requirements  
- Limited technical and staffing resources  

While this is project clearly not a polished, enterprise-scale system, it demonstrates:
- Future-proof data pipelines  
- Rigorous, assumption-aware modeling  
- Rapid development of decision-relevant analytics tools  

I had a blast working on it.

---

## Projected Impact

We anticipate that this dashboard will improve operational efficiency and reduce waste, ultimately contributing to improved margins and better allocation of staff time. Longer-term impact will be evaluated after additional seasons of deployment and model refinement.

---

## Tech Stack

- R (tidyverse)  
- Shiny  
- Google Data APIs  

### Google Sheets Integration

This app uses a Google service account for authentication.  
When deployed, a shinyapps.io service account is granted access directly to required Google Sheets.

---

## Repository Structure


market-forecasting-dashboard/
|
├── data/
│ ├── raw/
│ │ ├── sales_2024/
│ │ ├── sales_2025/
│ │ ├── weather/
│ │ └── calendar/
│ ├── processed/
│ └── model/
│
├── modeling-pipeline/
│ ├── 01_order_data_prep.R
│ ├── 02_calendar_generation.R
│ ├── 03_weather_data_processing.R
│ ├── 04_order_data_feature_engineering.R
│ └── README.md
│
├── analysis/
│ ├── Whole mess of files ... this will be cleaned up next on iteration.
│
├── scripts/
│ ├── bakery_model_script.R
│ ├── bread_fax_import.R
│ ├── general_product_script.R
│ └── seasonal_market_projections.R
|
├── app.R
├── global.R
├── ui.R
├── server.R
│
├── run_pipeline.R
├── paths.R
└── README.md

