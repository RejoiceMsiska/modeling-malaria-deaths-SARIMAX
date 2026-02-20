# train.R
options(warn=1)

train_chap <- function(csv_fn, model_fn) {
  library(forecast)
  library(lubridate)
  library(dplyr)

  # Read the combined data CSV
  # Assume columns: time_period (YYYYMM), disease_cases, mean_temperature, rainfall
  df <- read.csv(csv_fn)

  # Handle NA in disease_cases
  df$disease_cases[is.na(df$disease_cases)] <- 0

  # Convert time_period to Date (add day 01)
  df <- df %>%
    mutate(Date = ymd(paste0(time_period, "01"))) %>%
    arrange(Date)

  # Create lags for rainfall
  df <- df %>%
    mutate(rainfall_lag1 = lag(rainfall, 1),
           rainfall_lag2 = lag(rainfall, 2))

  # Remove rows with NA lags
  df_valid <- df %>%
    filter(!is.na(rainfall_lag1) & !is.na(rainfall_lag2))

  # Create time series for disease_cases
  start_year <- year(min(df_valid$Date))
  start_month <- month(min(df_valid$Date))
  ts_cases <- ts(df_valid$disease_cases, start = c(start_year, start_month), frequency = 12)

  # Create xreg matrix
  xreg <- as.matrix(df_valid[, c("mean_temperature", "rainfall_lag1", "rainfall_lag2")])

  # Fit SARIMAX model
  model <- auto.arima(ts_cases, seasonal = TRUE, xreg = xreg, stepwise = TRUE, approximation = TRUE)

  # Save the model
  saveRDS(model, file = model_fn)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  csv_fn <- args[1]
  model_fn <- args[2]
  train_chap(csv_fn, model_fn)
}
