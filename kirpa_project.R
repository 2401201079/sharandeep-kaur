library(ggplot2)
library(readr)
# Load CSV
stock_data <- read_csv("stock_kirpa.csv")
# Convert Date column to Date type
stock_data$Date <- as.Date(stock_data$Date)
# Preview data
head(stock_data)
# Plot opening prices
ggplot(stock_data, aes(x = Date, y = Open)) +
  geom_line(color = "darkgreen") +
  labs(title = "Stock Opening Prices Over Time", x = "Date", y = "Opening Price")
library(tsibble)
stock_ts <- stock_data %>%
  as_tsibble(index = Date)
library(tseries)
# Augmented Dickey-Fuller Test
adf.test(stock_data$Open)
# If not stationary, difference it
stock_data$Open_diff <- diff(stock_data$Open)
# Plot differenced series
ggplot(stock_data[-1, ], aes(x = Date, y = Open_diff)) +
  geom_line(color = "red") +
  labs(title = "Differenced Opening Prices", x = "Date", y = "Price Change")

library(forecast)
# ARIMA model
arima_model <- auto.arima(stock_data$Open)
summary(arima_model)

# ETS model
ets_model <- ets(stock_data$Open)
summary(ets_model)

# Forecast 30 days ahead
arima_forecast <- forecast(arima_model, h = 30)
ets_forecast <- forecast(ets_model, h = 30)

# Plot
autoplot(arima_forecast) + labs(title = "ARIMA Forecast - Opening Prices")
autoplot(ets_forecast) + labs(title = "ETS Forecast - Opening Prices")

# Train-test split
n <- nrow(stock_data)
train_data <- ts(stock_data$Open[1:(0.8 * n)], frequency = 252)
test_data <- ts(stock_data$Open[(0.8 * n + 1):n], frequency = 252)

# Fit model on training data
model <- auto.arima(train_data)

# Forecast and evaluate
pred <- forecast(model, h = length(test_data))
accuracy(pred, test_data)




