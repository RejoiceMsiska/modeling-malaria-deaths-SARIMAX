# predict.R
options(warn=1)

predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  library(forecast)
  library(lubridate)
  library(dplyr)

  # Load the model
  model <- readRDS(model_fn)

  # Read historic data to get last values for lags
  historic <- read.csv(historic_data_fn)
  historic <- historic %>%
    mutate(Date = ymd(paste0(time_period, "01"))) %>%
    arrange(Date)

  # Read future climate data
  # Assume columns: time_period (YYYYMM), mean_temperature, rainfall
  future <- read.csv(future_climatedata_fn)
  future <- future %>%
    mutate(Date = ymd(paste0(time_period, "01"))) %>%
    arrange(Date)

  # To create lags for future, append future to last 2 historic rows
  last_two_historic <- tail(historic, 2)
  append_for_lags <- rbind(last_two_historic[, c("time_period", "mean_temperature", "rainfall", "Date")], future[, c("time_period", "mean_temperature", "rainfall", "Date")])

  # Create lags
  append_for_lags <- append_for_lags %>%
    mutate(rainfall_lag1 = lag(rainfall, 1),
           rainfall_lag2 = lag(rainfall, 2))

  # Filter to future periods only
  future_with_lags <- append_for_lags %>%
    filter(time_period %in% future$time_period)

  # Create future xreg
  future_xreg <- as.matrix(future_with_lags[, c("mean_temperature", "rainfall_lag1", "rainfall_lag2")])

  # Forecast
  fc <- forecast(model, h = nrow(future), xreg = future_xreg)

  # Prepare output DF
  output <- future %>%
    mutate(sample_0 = fc$mean)  # Using point forecast as sample_0

  # Add confidence intervals if needed, but for now, just sample_0
  # Optionally add more samples for uncertainty, but CHAP may expect probabilistic if possible

  # Write to CSV
  write.csv(output[, c("time_period", "sample_0")], predictions_fn, row.names = FALSE)  # Assume minimal columns; add location if present
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 4) {
  model_fn <- args[1]
  historic_data_fn <- args[2]
  future_climatedata_fn <- args[3]
  predictions_fn <- args[4]
  predict_chap(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn)
}
