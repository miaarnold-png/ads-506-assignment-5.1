library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tsibble)
library(fable)
library(feasts)
library(gt)

# ==============================================================================
# 1. DATA IMPORT & PREPARATION (Runs once when the app starts)
# ==============================================================================
aus_wine <- read_csv(
  "data/AustralianWines.csv",
  na = "*",
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, "-", "-01-")) |> yearmonth())

wine <- aus_wine |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

min_date <- as.Date(min(wine$Month))
max_date <- as.Date(max(wine$Month))
varietal_choices <- sort(unique(wine$Varietal))
default_varietals <- if (all(c("Red", "sparkling") %in% varietal_choices)) {
  c("Red", "sparkling")
} else {
  varietal_choices[1:min(2, length(varietal_choices))]
}

# ==============================================================================
# 2. USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("Australian Wine Sales: Comparative Forecasting"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data & Split Controls"),

      # Multiselect for Varietal
      selectizeInput(
        inputId = "varietals",
        label = "Select Varietal(s):",
        choices = varietal_choices,
        selected = default_varietals,
        multiple = TRUE,
        options = list(placeholder = "Choose one or more varietals...")
      ),
      helpText("Select two or more varietals to compare side-by-side."),

      # Historical date range filter
      dateRangeInput(
        inputId = "hist_range",
        label = "Historical Data Range:",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date,
        format = "yyyy-mm",
        startview = "year"
      ),

      # Date range: Start = Train cutoff, End = End of forecast horizon
      dateRangeInput(
        inputId = "split_horizon",
        label = "Train Cutoff (Start) & Forecast End (End):",
        start = as.Date("1993-12-01"),
        end = max_date,
        min = min_date,
        max = as.Date("2000-12-01"),
        format = "yyyy-mm",
        startview = "year"
      ),
      helpText(
        "Start date sets the end of the training window (dashed red line). ",
        "End date sets the end of the validation/forecast horizon."
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # --- TAB 1: VISUALIZATION ---
        tabPanel(
          title = "1. Visualization",
          br(),
          checkboxInput(
            inputId = "show_stl",
            label = "Show STL Trend & Seasonality Decomposition",
            value = FALSE
          ),
          plotOutput("overview_plot", height = "480px"),
          conditionalPanel(
            condition = "input.show_stl == true",
            hr(),
            h4("STL Decomposition (Trend & Seasonality)"),
            plotOutput("stl_plot", height = "520px")
          )
        ),

        # --- TAB 2: MODEL BUILDING & ACCURACY ---
        tabPanel(
          title = "2. Model Building",
          br(),
          fluidRow(
            column(
              width = 6,
              checkboxInput(
                inputId = "show_specs",
                label = "Show Model Specifications (ETS & ARIMA Orders)",
                value = FALSE
              )
            ),
            column(
              width = 6,
              checkboxInput(
                inputId = "show_trn_acc",
                label = "Show Training Accuracy Table",
                value = FALSE
              )
            )
          ),

          conditionalPanel(
            condition = "input.show_specs == true",
            uiOutput("spec_table"),
            br()
          ),

          h4("Recommended Model per Varietal (Lowest Validation RMSE)"),
          uiOutput("best_model_table"),
          br(),

          h4("Validation Window Accuracy"),
          uiOutput("val_acc_table"),
          br(),

          conditionalPanel(
            condition = "input.show_trn_acc == true",
            h4("Training Window Accuracy"),
            uiOutput("trn_acc_table")
          )
        ),

        # --- TAB 3: FORECASTS ---
        tabPanel(
          title = "3. Forecast",
          br(),
          fluidRow(
            column(
              width = 5,
              sliderInput(
                inputId = "context_months",
                label = "Months of Historical Actuals to Overlay:",
                min = 6,
                max = 60,
                value = 24,
                step = 6
              )
            ),
            column(
              width = 4,
              radioButtons(
                inputId = "pi_level",
                label = "Prediction Interval Level:",
                choices = c("80%" = "80", "95%" = "95", "Both (80% & 95%)" = "both"),
                selected = "80",
                inline = TRUE
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "fc_model",
                label = "Forecast Table Model:",
                choices = c("TSLM", "ETS", "ARIMA"),
                selected = "ARIMA"
              )
            )
          ),

          plotOutput("forecast_plot", height = "520px"),
          hr(),
          uiOutput("forecast_table")
        ),

        # --- TAB 4: ABOUT & REPRODUCIBILITY ---
        tabPanel(
          title = "4. About",
          br(),
          h4("Dataset & Modeling Overview"),
          p(
            "This application analyzes monthly Australian wine sales (in thousands of liters) ",
            "across varietals using a monthly tsibble keyed by Varietal. Missing values in ",
            "the Rose series ('*') are imputed downward prior to modeling."
          ),
          tags$ul(
            tags$li(tags$strong("TSLM:"), " Time series linear model with deterministic trend and monthly seasonality."),
            tags$li(tags$strong("ETS:"), " Automated exponential smoothing state-space selection (error, trend, seasonality)."),
            tags$li(tags$strong("ARIMA:"), " Automated seasonal ARIMA order selection via step-wise AICc minimization.")
          ),
          h4("How to Reproduce Figures"),
          p(
            "Select the target varietal(s), historical date window, training cutoff date, and ",
            "forecast horizon end date in the left sidebar. Active parameter choices dynamically ",
            "update all plots, specifications, accuracy metrics, and monthly point forecasts."
          ),
          h4("Attribution"),
          p(
            "Built for ADS-506 Module 5 using R, Shiny, tidyverse, fpp3, and gt. ",
            "AI assistance was used to help structure reactive Shiny components and formatting."
          )
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {

  # Parse date inputs into yearmonth objects
  train_cutoff <- reactive({
    req(input$split_horizon)
    yearmonth(input$split_horizon[1])
  })

  forecast_end <- reactive({
    req(input$split_horizon)
    yearmonth(input$split_horizon[2])
  })

  # Filter tsibble by selected varietals and historical date range
  wine_filtered <- reactive({
    req(input$varietals, input$hist_range)
    wine |>
      filter(
        Varietal %in% input$varietals,
        Month >= yearmonth(input$hist_range[1]),
        Month <= yearmonth(input$hist_range[2])
      )
  })

  # Training set (<= train_cutoff) with friendly validation guards
  wine_trn <- reactive({
    validate(
      need(length(input$varietals) >= 1, "Please select at least one varietal."),
      need(forecast_end() > train_cutoff(), "Forecast end date must be after the training cutoff date.")
    )

    trn <- wine_filtered() |>
      filter(Month <= train_cutoff())

    validate(
      need(nrow(trn) >= 24, "Please include at least 24 months of training data before the cutoff.")
    )
    trn
  })

  # Validation set (> train_cutoff up to forecast_end)
  wine_tst <- reactive({
    wine_filtered() |>
      filter(Month > train_cutoff(), Month <= forecast_end())
  })

  # Forecast horizon in months
  fc_horizon <- reactive({
    as.integer(forecast_end() - train_cutoff())
  })

  # Fit TSLM, ETS, and ARIMA models per varietal
  wine_fit <- reactive({
    wine_trn() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  # Generate forecasts over the user-defined horizon
  wine_fc <- reactive({
    wine_fit() |>
      forecast(h = fc_horizon())
  })

  # --- TAB 1 OUTPUTS ---
  output$overview_plot <- renderPlot({
    wine_filtered() |>
      ggplot(aes(x = Month, y = Sales)) +
      geom_line(color = "#2C3E50", linewidth = 0.7) +
      geom_vline(
        xintercept = train_cutoff(),
        color = "red",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(
        title = "Historical Monthly Wine Sales by Varietal",
        subtitle = paste("Dashed red line indicates training cutoff:", format(train_cutoff())),
        x = "Month",
        y = "Sales"
      ) +
      theme_classic()
  })

  output$stl_plot <- renderPlot({
    wine_filtered() |>
      model(STL(Sales ~ season(window = "periodic"))) |>
      components() |>
      autoplot() +
      theme_classic()
  })

  # --- TAB 2 OUTPUTS ---
  output$spec_table <- renderUI({
    wine_fit() |>
      as_tibble() |>
      mutate(across(c(TSLM, ETS, ARIMA), ~ str_remove_all(format(.x), "[<>]"))) |>
      gt() |>
      tab_header(
        title = "Auto-Selected Model Specifications",
        subtitle = "ETS component structure and ARIMA seasonal orders"
      )
  })

  val_metrics <- reactive({
    tst <- wine_tst()
    validate(
      need(nrow(tst) > 0, "No actual observations fall inside the validation window to score accuracy.")
    )

    wine_fc() |>
      accuracy(tst) |>
      select(Varietal, .model, RMSE, MAE, MAPE)
  })

  output$best_model_table <- renderUI({
    val_metrics() |>
      group_by(Varietal) |>
      slice_min(order_by = RMSE, n = 1, with_ties = FALSE) |>
      ungroup() |>
      gt() |>
      tab_header(title = "Top Performing Model per Varietal (Validation Window)") |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2)
  })

  output$val_acc_table <- renderUI({
    val_metrics() |>
      arrange(Varietal, RMSE) |>
      gt() |>
      tab_header(
        title = "Validation Accuracy",
        subtitle = paste("Evaluated from", format(train_cutoff() + 1), "to", format(min(forecast_end(), max(wine_filtered()$Month))))
      ) |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2)
  })

  output$trn_acc_table <- renderUI({
    wine_fit() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE) |>
      gt() |>
      tab_header(
        title = "Training Accuracy",
        subtitle = paste("Evaluated through", format(train_cutoff()))
      ) |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2)
  })

  # --- TAB 3 OUTPUTS ---
  output$forecast_plot <- renderPlot({
    overlay_start <- train_cutoff() - input$context_months
    actual_overlay <- wine_filtered() |>
      filter(Month >= overlay_start, Month <= forecast_end())

    pi_levels <- if (input$pi_level == "both") {
      c(80, 95)
    } else {
      as.numeric(input$pi_level)
    }

    wine_fc() |>
      autoplot(actual_overlay, level = pi_levels) +
      labs(
        title = "Comparative Model Forecasts vs. Actual Sales",
        subtitle = paste("Cutoff:", format(train_cutoff()), "| Horizon:", fc_horizon(), "months"),
        x = "Month",
        y = "Sales"
      ) +
      facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
      theme_classic()
  })

  output$forecast_table <- renderUI({
    wine_fc() |>
      filter(.model == input$fc_model) |>
      as_tibble() |>
      select(Varietal, .model, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean) |>
      gt() |>
      tab_header(
        title = paste(input$fc_model, "Monthly Point Forecasts"),
        subtitle = "Predicted sales across the selected forecast horizon"
      ) |>
      fmt_number(columns = where(is.numeric), decimals = 2)
  })
}

# ==============================================================================
# 4. RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)