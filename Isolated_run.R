source("train.R")
source("predict.R")

train_chap("input/train_data.csv", "output/model.bin")
predict_chap("output/model.bin", "input/train_data.csv", "input/future_climate_data.csv", "output/predictions.csv")
