library("fpp3")
library(dplyr)
library(tibble)
library(fable)
library(lubridate)

setwd("/home/ismail/Documents/projects/time-series")
getwd()
data <- read.csv2("monthly_values.csv", header = FALSE, skip = 4, col.names = c("date", "value", "status"))
head(data)


# Step 2: Clean and convert columns
data$date <- as.Date(paste0(data$date, "-01"))           # convert "YYYY-MM" to proper Date
data$value <- as.numeric(as.character(data$value))       # convert index values to numeric
#Step 3: Sort chronologically (INSEE data is usually descending)
data <- data[order(data$date), ]

data <- as_tsibble(data, index = date)
data <- data |>
  mutate(date = yearmonth(date)) |>
  as_tsibble(index = date) |>
  fill_gaps()
data




# check data size
nrow(data)

data <- data[1:(nrow(data) - 20), ]
#data$log_value <- log(data$value)  # Avoid log(0) by adding 1
# Check the time range
range(as.Date(data_diff$date))  # Assuming the date is in 'date' column

# Check summary statistics to understand the scale and distribution
summary(data_diff$diff_value)

# Visualize the distribution of the index values
hist(data_diff$diff_value, main = "Distribution of Retail Sales Volume Index", xlab = "Index Value")


plot(data$value, type = "l", main = "Time Series Plot")
acf(data$value, lag.max = 40)
mean(data$value, na.rm= TRUE)






# first differnece
data_diff <- data %>%
  mutate(diff_value = difference(value))



plot(data_diff$diff_value, type = "l", main = "Time Series Plot")

# mean after differencing
mean(data_diff$diff_value, na.rm= TRUE)

#acf pacf
par(mfrow = c(2, 1))  # 1 row, 2 columns
acf(na.omit(data_diff$diff_value), lag.max = 30)
pacf(na.omit(data_diff$diff_value), lag.max = 30)
data_diff

# Set max lags
max_lag <- 20

# Remove NAs from the data
x <- na.omit(data_diff$diff_value)
n <- length(x)

# ACF
acf_res <- acf(x, plot = FALSE, lag.max = max_lag)
acf_vals <- acf_res$acf[-1]
acf_lags <- acf_res$lag[-1]
acf_se <- 1 / sqrt(n)  # standard error for white noise
acf_z <- acf_vals / acf_se
acf_p <- 2 * (1 - pnorm(abs(acf_z)))

# PACF
pacf_res <- pacf(x, plot = FALSE, lag.max = max_lag)
pacf_vals <- pacf_res$acf
pacf_lags <- 1:max_lag
pacf_se <- 1 / sqrt(n)
pacf_z <- pacf_vals / pacf_se
pacf_p <- 2 * (1 - pnorm(abs(pacf_z)))

# Combine into a table
acf_pacf_stats <- tibble(
  Lag = acf_lags,
  ACF = round(acf_vals, 3),
  `ACF P-Value` = round(acf_p, 4),
  PACF = round(pacf_vals, 3),
  `PACF P-Value` = round(pacf_p, 4)
)

print(acf_pacf_stats)


# non-stationary variance
library(zoo)

# Rolling window variance
rolling_var <- rollapply(data_diff$diff_value, width = 50, FUN = var, fill = NA)

# Plotting
plot(rolling_var, type = "l", main = "Rolling Variance (Window = 50)")

library(car)
data_diff <- na.omit(data_diff)  # Remove rows with NA values

n <- length(data_diff$diff_value)
first_half <- na.omit(data_diff$diff_value)[1:(n/2)]
second_half <- data_diff$diff_value[(n/2 + 1):n]

# Compare variances of the two halves
var_first_half <- var(first_half)
var_second_half <- var(second_half)

# Compute the variance ratio
var_ratio <- var_first_half / var_second_half
var_ratio

library(car)

# Split into two halves
n <- nrow(data_diff)
first_half <- data_diff$diff_value[1:(n/2)]
second_half <- data_diff$diff_value[((n/2)+1):n]

# Create grouping factor
group <- factor(c(rep(1, length(first_half)), rep(2, length(second_half))))
values <- c(first_half, second_half)

# Levene's test
leveneTest(values ~ group)



# stationarity
# dickey fuller
library(tseries)
adf.test(data_diff$diff_value)

##pp
library(urca)

pp_test <- ur.pp(ts_data, type = "Z-tau", model = "constant", lags = "short")
summary(pp_test)



# estimation


library(fable)
model_fit <- data_diff %>%
  model(ARIMA(diff_value))
report(model_fit)

model_ma1 <- data_diff %>%
  model(MA1 = ARIMA(diff_value ~ pdq(3, 0, 1)))
report(model_ma1)
data_diff

library(forecast)


# Convert to ts object
ts_data <- ts(data_diff$diff_value, start = c(1999, 2), frequency = 12)

# ---- Fit Model: ARMA ----
fit_arma31 <- Arima(ts_data, order = c(3, 0, 1), include.mean = TRUE)
fit_arma21 <- Arima(ts_data, order = c(2, 0, 1), include.mean = TRUE)
fit_arma11 <- Arima(ts_data, order = c(1, 0, 1), include.mean = TRUE)

# ---- Forecasts for in-sample RMSE/MAE ----
fc_arma31 <- fitted(fit_arma31)
fc_arma21 <- fitted(fit_arma21)
fc_arma11 <- fitted(fit_arma11)

# Align fitted and actual values
align_rmse_mae <- function(actual, fitted) {
  idx <- !is.na(fitted)
  list(
    rmse = sqrt(mean((actual[idx] - fitted[idx])^2)),
    mae = mean(abs(actual[idx] - fitted[idx]))
  )
}

m31 <- align_rmse_mae(ts_data, fc_arma31)
m21 <- align_rmse_mae(ts_data, fc_arma21)
m11 <- align_rmse_mae(ts_data, fc_arma11)

# ---- Extract criteria with complete metrics ----
results <- data.frame(
  Model = c("ARMA(3,1)", "ARMA(2,1)", "ARMA(1,1)"),
  AIC = c(AIC(fit_arma31), AIC(fit_arma21), AIC(fit_arma11)),
  BIC = c(BIC(fit_arma31), BIC(fit_arma21), BIC(fit_arma11)),
  HQC = c(
    AIC(fit_arma31, k = log(log(length(ts_data)))),
    AIC(fit_arma21, k = log(log(length(ts_data)))),
    AIC(fit_arma11, k = log(log(length(ts_data))))
  ),
  LogLikelihood = c(logLik(fit_arma31), logLik(fit_arma21), logLik(fit_arma11)),
  RMSE = c(m31$rmse, m21$rmse, m11$rmse),
  MAE = c(m31$mae, m21$mae, m11$mae)
)

print(results)


fit_arma31
fit_arma21
fit_arma11










# Function to compute additional statistics
extended_metrics <- function(model, ts_data) {
  y_hat <- fitted(model)
  residuals <- residuals(model)
  idx <- !is.na(y_hat)
  y <- ts_data[idx]
  
  # Sum of Squares
  ss_res <- sum((y - y_hat[idx])^2)
  ss_tot <- sum((y - mean(y))^2)
  
  # Metrics
  n <- length(y)
  k <- length(model$coef)
  
  r_squared <- 1 - (ss_res / ss_tot)
  adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - k))
  ser <- sqrt(ss_res / (n - k))
  dw <- sum(diff(residuals)^2) / ss_res
  
  return(c(
    R_squared = r_squared,
    Adjusted_R_squared = adj_r_squared,
    SER = ser,
    Durbin_Watson = dw
  ))
}



# Calculate additional stats
extra_31 <- extended_metrics(fit_arma31, ts_data)
extra_21 <- extended_metrics(fit_arma21, ts_data)
extra_11 <- extended_metrics(fit_arma11, ts_data)

# ---- Full Results Table ----
results_ext <- cbind(
  results,
  R_Squared = c(extra_31[1], extra_21[1], extra_11[1]),
  Adjusted_R_Squared = c(extra_31[2], extra_21[2], extra_11[2]),
  SER = c(extra_31[3], extra_21[3], extra_11[3]),
  Durbin_Watson = c(extra_31[4], extra_21[4], extra_11[4])
)

print(results_ext, digits = 4)










# Extract coefficients and standard errors
coefs <- coef(fit_arma11)
se <- sqrt(diag(fit_arma11$var.coef))

# Compute z-values and p-values
z_values <- coefs / se
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Combine into a data frame
coef_summary <- data.frame(
  Coefficient = names(coefs),
  Estimate = coefs,
  StdError = se,
  Z = z_values,
  PValue = p_values
)

print(coef_summary)



# Get variance-covariance matrix
vcov_mat <- vcov(fit_arma11)

# Convert to correlation matrix
cor_mat <- cov2cor(vcov_mat)

# Print correlation matrix
print(round(cor_mat, 3))


# DIAGNOSTIC CHECKING STAGE

# Plot residuals for each model
par(mfrow = c(2, 1))  # Arrange plots in 3 rows, 2 columns


# ARMA(2,1)
plot(residuals(fit_arma21), main = "Residuals - ARMA(2,1)", ylab = "Residuals")
acf(na.omit(residuals(fit_arma21)), main = "ACF - ARMA(2,1) Residuals")


# ARMA(1,1)
plot(residuals(fit_arma11), main = "Residuals - ARMA(1,1)", ylab = "Residuals")
acf(na.omit(residuals(fit_arma11)), main = "ACF - ARMA(1,1) Residuals")





# Set max lags
max_lag <- 20

# Remove NAs from the data
x <- na.omit(residuals(fit_arma21))
n <- length(x)

# ACF
acf_res <- acf(x, plot = FALSE, lag.max = max_lag)
acf_vals <- acf_res$acf[-1]
acf_lags <- acf_res$lag[-1]
acf_se <- 1 / sqrt(n)  # standard error for white noise
acf_z <- acf_vals / acf_se
acf_p <- 2 * (1 - pnorm(abs(acf_z)))

# PACF
pacf_res <- pacf(x, plot = FALSE, lag.max = max_lag)
pacf_vals <- pacf_res$acf
pacf_lags <- 1:max_lag
pacf_se <- 1 / sqrt(n)
pacf_z <- pacf_vals / pacf_se
pacf_p <- 2 * (1 - pnorm(abs(pacf_z)))

# Combine into a table
acf_pacf_stats <- tibble(
  Lag = acf_lags,
  ACF = round(acf_vals, 3),
  `ACF P-Value` = round(acf_p, 4),
  PACF = round(pacf_vals, 3),
  `PACF P-Value` = round(pacf_p, 4)
)

print(acf_pacf_stats)

# Ljung-Box test
Box.test(residuals(fit_arma21), lag = 20, type = "Ljung-Box")
Box.test(residuals(fit_arma11), lag = 20, type = "Ljung-Box")
#Arch effects
Box.test(residuals(fit_arma21)^2, lag = 20, type = "Ljung-Box")
Box.test(residuals(fit_arma11)^2, lag = 20, type = "Ljung-Box")




# Residuals from the model
res <- residuals(fit_arma11)

# Plot residuals
par(mfrow = c(2, 1))  # 2x2 layout

# 1. Time series plot
plot(res, main = "Residuals over Time", ylab = "Residuals", col = "blue")

# 2. Histogram
hist(res, main = "ARMA(1,1) - Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 20)

# 3. ACF of residuals
acf(res, main = "ACF of Residuals")

# 4. Q-Q plot
qqnorm(res, main = "Q-Q Plot of Residuals")
qqline(res, col = "red")

par(mfrow = c(1,1))  # reset layout


# Required packages
library(zoo)

# Get residuals and create a time index
res <- residuals(fit_arma21)
time_index <- time(ts_data)
date_index <- as.Date(as.yearmon(time_index))  # Convert to date

# Plot residuals with month labels
plot(date_index, res, type = "l", col = "blue",
     main = "ARMA(2,1) - Residuals with Monthly Time Axis",
     xlab = "Date", ylab = "Residuals", xaxt = "n")
axis.Date(1, at = seq(min(date_index), max(date_index), by = "6 months"), format = "%b %Y", las = 2)

# Add horizontal line for zero
abline(h = 0, col = "red", lty = 2)

# Highlight potential outliers (e.g., abs(residual) > 2*sd)
outlier_idx <- which(abs(res) > 2 * sd(res))
points(date_index[outlier_idx], res[outlier_idx], col = "red", pch = 19)

# Optional: print dates of outliers after 2015
outlier_dates <- date_index[outlier_idx]
outlier_after_2015 <- outlier_dates[outlier_dates > as.Date("2015-01-01")]
print(outlier_after_2015)


# Get residuals and create a time index
res <- residuals(fit_arma11)
time_index <- time(ts_data)
date_index <- as.Date(as.yearmon(time_index))  # Convert to date

# Plot residuals with month labels
plot(date_index, res, type = "l", col = "blue",
     main = "ARMA(1,1) - Residuals with Monthly Time Axis",
     xlab = "Date", ylab = "Residuals", xaxt = "n")
axis.Date(1, at = seq(min(date_index), max(date_index), by = "6 months"), format = "%b %Y", las = 2)

# Add horizontal line for zero
abline(h = 0, col = "red", lty = 2)

# Highlight potential outliers (e.g., abs(residual) > 2*sd)
outlier_idx <- which(abs(res) > 2 * sd(res))
points(date_index[outlier_idx], res[outlier_idx], col = "red", pch = 19)

# Optional: print dates of outliers after 2015
outlier_dates <- date_index[outlier_idx]
outlier_after_2015 <- outlier_dates[outlier_dates > as.Date("2015-01-01")]
print(outlier_after_2015)

# Fitted values
fitted_values <- fitted(fit_arma11)

# Residuals
residuals <- residuals(fit_arma11)

# Plot residuals vs fitted values
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")



library(forecast)
# Fit a SARIMA model to your data 
#(example: 'data' is your time series data)
data_ts <- as.ts(data_diff$value)  # Adjust the column name if necessary
fit_sarima <- auto.arima(data_ts)

# Print the model summary
summary(fit_sarima)

# Check residuals of the SARIMA model
checkresiduals(fit_arma21)

# Forecast future values (e.g., for 12 periods ahead)
forecast_sarima <- forecast(fit_arma11, h = 12)

# Plot the forecast
plot(fit_arma11)





# CHECKING NORMALITY
shapiro.test(residuals(fit_arma21))        # Shapiro-Wilk Test
tseries::jarque.bera.test(residuals(fit_arma21))  # Jarque-Bera Test


# MODEL REFINEMENT
# Calculate the Z-scores of data_diff$diff_value
z_scores <- scale(data_diff$diff_value)

# Identify outliers (Z-scores greater than 3 or less than -3)
outliers <- which(abs(z_scores) > 3)

# Print the indices of outliers
print(outliers)


# Calculate the IQR for data_diff$diff_value
Q1 <- quantile(data_diff$diff_value, 0.25)
Q3 <- quantile(data_diff$diff_value, 0.75)
IQR_value <- IQR(data_diff$diff_value)

# Identify outliers
outliers_iqr <- which(data_diff$diff_value < (Q1 - 1.5 * IQR_value) | data_diff$diff_value > (Q3 + 1.5 * IQR_value))


# Impute outliers (from both Z-score and IQR methods) with the mean
data_diff$diff_value[outliers] <- mean(data_diff$diff_value, na.rm = TRUE)
data_diff$diff_value[outliers_iqr] <- mean(data_diff$diff_value, na.rm = TRUE)

# Check the updated data
head(data_diff$diff_value)


data_diff |> autoplot(diff_value)
# Check the distribution of the updated data
par(mfrow = c(2, 2))  # 1 row, 2 columns

hist(data_diff$diff_value, main = "Hist After Imputation of Outliers")

# Boxplot to check for remaining outliers
boxplot(data_diff$diff_value, main = "Boxplot After Imputation of Outliers")

# Fit the ARIMA model again
fit_arma11 <- arima(data_diff$diff_value, order = c(1, 0, 1))
fit_arma21 <- arima(data_diff$diff_value, order = c(2, 0, 1))

checkresiduals(fit_arma11)



# Ljung-Box test
Box.test(residuals(fit_arma21), lag = 20, type = "Ljung-Box")
Box.test(residuals(fit_arma11), lag = 20, type = "Ljung-Box")
#Arch effects
Box.test(residuals(fit_arma21)^2, lag = 20, type = "Ljung-Box")
Box.test(residuals(fit_arma11)^2, lag = 20, type = "Ljung-Box")

shapiro.test(residuals(fit_arma21))        # Shapiro-Wilk Test
tseries::jarque.bera.test(residuals(fit_arma21))  # Jarque-Bera Test

par(mfrow = c(1, 1))  # Arrange plots in 3 rows, 2 columns

library(ggplot2)

# Add a column to indicate the split
data$set <- ifelse(1:nrow(data) <= nrow(train_data), "Train", "Test")

# Add a time index if needed
data$time <- 1:nrow(data)

# Plot
ggplot(data, aes(x = time, y = value, color = set)) +
  geom_line(size = 1) +
  labs(title = "Train vs Test Data Split",
       x = "Time",
       y = "Value") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()




# Set the proportion of data to use for training
train_ratio <- 0.8

# Calculate the index at which to split
train_size <- floor(nrow(data) * train_ratio)

# Split the data
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Check
cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")
# Forecasting on the differenced data
forecast_arma11_diff <- forecast(fit_arma11, h = nrow(test_data))
forecast_arma21_diff <- forecast(fit_arma21, h = nrow(test_data))

# Reversing the differencing to bring the forecast back to the original scale

# The last observed value in the original data (before differencing)
last_value <- tail(train_data$value, 1)

# Forecasting on differenced data, reverse the differencing
forecast_arma11_original <- last_value + cumsum(forecast_arma11_diff$mean)
forecast_arma21_original <- last_value + cumsum(forecast_arma21_diff$mean)

# Compare the forecasted values to the actual values in the test set
actual_values <- test_data$value

# Calculate accuracy metrics for ARMA(1,1) forecast
rmse_arma11 <- sqrt(mean((actual_values - forecast_arma11_original)^2))
mae_arma11 <- mean(abs(actual_values - forecast_arma11_original))
mape_arma11 <- mean(abs((actual_values - forecast_arma11_original) / actual_values)) * 100

# Calculate accuracy metrics for ARMA(2,1) forecast
rmse_arma21 <- sqrt(mean((actual_values - forecast_arma21_original)^2))
mae_arma21 <- mean(abs(actual_values - forecast_arma21_original))
mape_arma21 <- mean(abs((actual_values - forecast_arma21_original) / actual_values)) * 100

# Print forecast accuracy metrics
cat("ARMA(1,1) Forecast Accuracy Metrics:\n")
cat("RMSE:", rmse_arma11, "\n")
cat("MAE:", mae_arma11, "\n")
cat("MAPE:", mape_arma11, "%\n\n")

cat("ARMA(2,1) Forecast Accuracy Metrics:\n")
cat("RMSE:", rmse_arma21, "\n")
cat("MAE:", mae_arma21, "\n")
cat("MAPE:", mape_arma21, "%\n")








# Create the time index only for the test period
time_index <- (nrow(train_data) + 1):nrow(data)

# Plot only test set (actual vs forecasts)
plot(time_index, actual_values, type = "l", col = "darkgreen", lwd = 2,
     main = "Zoomed Forecast vs Actual (Test Set)",
     xlab = "Time Index", ylab = "Value", ylim = range(c(actual_values, forecast_arma11_original, forecast_arma21_original)))

lines(time_index, forecast_arma11_original, col = "blue", lwd = 2)
lines(time_index, forecast_arma21_original, col = "red", lwd = 2)

legend("topleft",
       legend = c("Actual", "ARMA(1,1) Forecast", "ARMA(2,1) Forecast"),
       col = c("darkgreen", "blue", "red"),
       lty = 1, lwd = 2)







# Assuming you have your fitted ARIMA models named 'fit_arma11' and 'fit_arma21'
# and your differenced data is in 'data_diff$diff_value'
# Also assuming your original data frame and value column have appropriate names
# (replace 'your_original_data$value' with the actual names)

library(forecast)
library(lubridate) # For easier date manipulation

# 1. Forecasting with ARIMA(1,1)
forecast_arma11 <- forecast(fit_arma11, h = 12) # Forecast 12 periods into the future

# 2. Forecasting with ARIMA(2,1)
forecast_arma21 <- forecast(fit_arma21, h = 12)

# 3. Extracting Forecast Values and Confidence Intervals (Differenced Scale)
forecast_values_arma11 <- forecast_arma11$mean
confidence_intervals_arma11_80 <- forecast_arma11$lower[, 1]
confidence_intervals_arma11_95 <- forecast_arma11$lower[, 2]
upper_confidence_intervals_arma11_80 <- forecast_arma11$upper[, 1]
upper_confidence_intervals_arma11_95 <- forecast_arma11$upper[, 2]

forecast_values_arma21 <- forecast_arma21$mean
confidence_intervals_arma21_80 <- forecast_arma21$lower[, 1]
confidence_intervals_arma21_95 <- forecast_arma21$lower[, 2]
upper_confidence_intervals_arma21_80 <- forecast_arma21$upper[, 1]
upper_confidence_intervals_arma21_95 <- forecast_arma21$upper[, 2]

# 4. Reversing the Differencing to Get Forecasts in the Original Scale
# Assuming the last value of your original 'value' series before differencing is 'last_original_value'
last_original_value <- tail(data$value, 1) # Replace 'your_original_data$value'

# Reverse differencing for ARIMA(1,1) forecasts
forecasts_original_scale_arma11 <- last_original_value + cumsum(forecast_values_arma11)
lower_80_original_arma11 <- last_original_value + cumsum(confidence_intervals_arma11_80)
lower_95_original_arma11 <- last_original_value + cumsum(confidence_intervals_arma11_95)
upper_80_original_arma11 <- last_original_value + cumsum(upper_confidence_intervals_arma11_80)
upper_95_original_arma11 <- last_original_value + cumsum(upper_confidence_intervals_arma11_95)

# Reverse differencing for ARIMA(2,1) forecasts
forecasts_original_scale_arma21 <- last_original_value + cumsum(forecast_values_arma21)
lower_80_original_arma21 <- last_original_value + cumsum(confidence_intervals_arma21_80)
lower_95_original_arma21 <- last_original_value + cumsum(confidence_intervals_arma21_95)
upper_80_original_arma21 <- last_original_value + cumsum(upper_confidence_intervals_arma21_80)
upper_95_original_arma21 <- last_original_value + cumsum(upper_confidence_intervals_arma21_95)

# 5. Print the forecasts in the original scale
cat("ARIMA(1,1) Forecasts (Original Scale):\n")
print(forecasts_original_scale_arma11)
cat("\nARIMA(2,1) Forecasts (Original Scale):\n")
print(forecasts_original_scale_arma21)

# 6. Visualizing the Forecasts (Initial Plot)
n_original <- length(data$value) # Replace 'your_original_data$value'
n_forecast <- 12
time_index <- seq(as.Date("1999-02-01"), by = "month", length.out = n_original) # Adjust start date if needed
forecast_index <- seq(tail(time_index, 1) + months(1), by = "month", length.out = n_forecast)

plot(time_index, data$value, type = "l", # Replace 'your_original_data$value'
     main = "Forecasts from ARIMA(1,1) vs ARIMA(2,1)",
     xlab = "Time",
     ylab = "Turnover Volume Index")
lines(forecast_index, forecasts_original_scale_arma11, col = "red", lwd = 2)
lines(forecast_index, forecasts_original_scale_arma21, col = "blue", lwd = 2)
legend("topright", legend = c("Original Data", "ARIMA(1,1) Forecast", "ARIMA(2,1) Forecast"),
       col = c("black", "red", "blue"), lty = 1, bty = "n")

# 7. Zoomed-in Plots
# Zoomed-in Plot of ARIMA(1,1) Forecasts
plot(time_index, data$value, type = "l", # Replace 'your_original_data$value'
     xlim = c(tail(time_index, 1) - years(2), tail(forecast_index, 1) + months(1)),
     main = "Zoomed-in Forecasts from ARIMA(1,1)",
     xlab = "Time",
     ylab = "Turnover Volume Index")
lines(forecast_index, forecasts_original_scale_arma11, col = "red", lwd = 2)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_95_original_arma11, rev(upper_95_original_arma11)),
        col = rgb(1, 0, 0, 0.2), border = NA)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_80_original_arma11, rev(upper_80_original_arma11)),
        col = rgb(1, 0, 0, 0.4), border = NA)
legend("topright", legend = c("Original Data", "Forecast", "80% CI", "95% CI"),
       col = c("black", "red", rgb(1, 0, 0, 0.4), rgb(1, 0, 0, 0.2)),
       lty = c(1, 1, NA, NA), pch = c(NA, NA, 15, 15), pt.cex = 1.2, bty = "n")

# Zoomed-in Plot of ARIMA(2,1) Forecasts
plot(time_index, data$value, type = "l", # Replace 'your_original_data$value'
     xlim = c(tail(time_index, 1) - years(2), tail(forecast_index, 1) + months(1)),
     main = "Zoomed-in Forecasts from ARIMA(2,1)",
     xlab = "Time",
     ylab = "Turnover Volume Index")
lines(forecast_index, forecasts_original_scale_arma21, col = "blue", lwd = 2)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_95_original_arma21, rev(upper_95_original_arma21)),
        col = rgb(0, 0, 1, 0.2), border = NA)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_80_original_arma21, rev(upper_80_original_arma21)),
        col = rgb(0, 0, 1, 0.4), border = NA)
legend("topright", legend = c("Original Data", "Forecast", "80% CI", "95% CI"),
       col = c("black", "blue", rgb(0, 0, 1, 0.4), rgb(0, 0, 1, 0.2)),
       lty = c(1, 1, NA, NA), pch = c(NA, NA, 15, 15), pt.cex = 1.2, bty = "n")

# Combined Zoomed-in Plot
plot(time_index, data$value, type = "l", # Replace 'your_original_data$value'
     xlim = c(tail(time_index, 1) - years(2), tail(forecast_index, 1) + months(1)),
     main = "Zoomed-in Forecasts: ARIMA(1,1) vs ARIMA(2,1)",
     xlab = "Time",
     ylab = "Turnover Volume Index")
lines(forecast_index, forecasts_original_scale_arma11, col = "red", lwd = 2)
lines(forecast_index, forecasts_original_scale_arma21, col = "blue", lwd = 2)
legend("topright", legend = c("Original Data", "ARIMA(1,1) Forecast", "ARIMA(2,1) Forecast"),
       col = c("black", "red", "blue"), lty = 1, bty = "n")










library(forecast)
library(lubridate)

# Assuming 'insee_data$value' is your original time series
last_original_value <- tail(data$value, 1)

# Forecast from ARIMA(1,1)
forecast_arma11 <- forecast(fit_arma11, h = 12)
point_forecasts_diff_arma11 <- forecast_arma11$mean

# Extract 80% confidence intervals
lower_80_diff_arma11 <- forecast_arma11$lower[, 1]
upper_80_diff_arma11 <- forecast_arma11$upper[, 1]

# Extract 95% confidence intervals
lower_95_diff_arma11 <- forecast_arma11$lower[, 2]
upper_95_diff_arma11 <- forecast_arma11$upper[, 2]

forecasts_original_scale_arma11 <- last_original_value + cumsum(point_forecasts_diff_arma11)
lower_80_original_arma11 <- last_original_value + cumsum(lower_80_diff_arma11)
upper_80_original_arma11 <- last_original_value + cumsum(upper_80_diff_arma11)
lower_95_original_arma11 <- last_original_value + cumsum(lower_95_diff_arma11)
upper_95_original_arma11 <- last_original_value + cumsum(upper_95_diff_arma11)

# Forecast from ARIMA(2,1)
forecast_arma21 <- forecast(fit_arma21, h = 12)
point_forecasts_diff_arma21 <- forecast_arma21$mean

# Extract 80% confidence intervals
lower_80_diff_arma21 <- forecast_arma21$lower[, 1]
upper_80_diff_arma21 <- forecast_arma21$upper[, 1]

# Extract 95% confidence intervals
lower_95_diff_arma21 <- forecast_arma21$lower[, 2]
upper_95_diff_arma21 <- forecast_arma21$upper[, 2]

forecasts_original_scale_arma21 <- last_original_value + cumsum(point_forecasts_diff_arma21)
lower_80_original_arma21 <- last_original_value + cumsum(lower_80_diff_arma21)
upper_80_original_arma21 <- last_original_value + cumsum(upper_80_diff_arma21)
lower_95_original_arma21 <- last_original_value + cumsum(lower_95_diff_arma21)
upper_95_original_arma21 <- last_original_value + cumsum(upper_95_diff_arma21)

# Create time index for forecasts
n_original <- length(data$value)
n_forecast <- 12
time_index <- seq(as.Date("1999-02-01"), by = "month", length.out = n_original) # Adjust start date if needed
forecast_index <- seq(tail(time_index, 1) + months(1), by = "month", length.out = n_forecast)





# Assuming your original data frame is 'insee_data' and the value column is 'value'
n_total <- length(insee_data$value)
n_test <- 12 # Hold out the last 12 months for testing
n_train <- n_total - n_test

train_data <- insee_data$value[1:n_train]
test_data <- insee_data$value[(n_train + 1):n_total]

# Fit your ARIMA models to the training data
fit_arma11_train <- arima(train_data, order = c(1, 1, 1))
fit_arma21_train <- arima(train_data, order = c(2, 1, 1))








# Plot of Forecasts
plot(time_index, data$value, type = "l",
     xlim = c(tail(time_index, 1) - years(2), tail(forecast_index, 1) + months(1)),
     main = "Forecasts of Turnover Volume Index",
     xlab = "Time",
     ylab = "Turnover Volume Index (2010 = 100)")
lines(forecast_index, forecasts_original_scale_arma11, col = "red", lwd = 2, lty = 1)
lines(forecast_index, forecasts_original_scale_arma21, col = "blue", lwd = 2, lty = 2)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_95_original_arma11, rev(upper_95_original_arma11)),
        col = rgb(1, 0, 0, 0.2), border = NA)
polygon(c(forecast_index, rev(forecast_index)),
        c(lower_95_original_arma21, rev(upper_95_original_arma21)),
        col = rgb(0, 0, 1, 0.2), border = NA)
legend("topright",
       legend = c("Original Data", "ARIMA(1,1) Forecast", "ARIMA(2,1) Forecast", "95% CI (ARIMA(1,1))", "95% CI (ARIMA(2,1))"),
       col = c("black", "red", "blue", rgb(1, 0, 0, 0.2), rgb(0, 0, 1, 0.2)),
       lty = c(1, 1, 2, NA, NA),
       pch = c(NA, NA, NA, 15, 15), pt.cex = 1.2, bty = "n")










# Assuming your original data frame is 'insee_data' and the value column is 'value'
library(forecast)

# Use 'insee_data' consistently
n_total <- length(data$value)
n_test <- 12 # Hold out the last 12 months for testing
n_train <- n_total - n_test

train_data <- data$value[1:n_train]
test_data <- data$value[(n_train + 1):n_total]

# Fit ARIMA models to the training data using arima()
fit_arma11_train <- arima(train_data, order = c(1, 1, 1))
fit_arma21_train <- arima(train_data, order = c(2, 1, 1))

# Generate forecasts on the testing set using forecast()
forecast_arma11_test <- forecast(fit_arma11_train, h = n_test)
forecast_arma21_test <- forecast(fit_arma21_train, h = n_test)

# Function to calculate accuracy metrics
calculate_accuracy <- function(forecast_values, actual_values) {
  n <- length(actual_values)
  residuals <- as.numeric(forecast_values) - as.numeric(actual_values)
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals))
  mape <- mean(abs(residuals / actual_values)) * 100
  me <- mean(residuals)
  
  return(data.frame(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    ME = me
  ))
}

# Extract point forecasts as numeric vectors
forecasts_arma11 <- as.numeric(forecast_arma11_test$mean)
forecasts_arma21 <- as.numeric(forecast_arma21_test$mean)

# Ensure test_data is a numeric vector
test_data_numeric <- as.numeric(test_data)

# Calculate accuracy for ARIMA(1,1)
accuracy_arma11 <- calculate_accuracy(forecasts_arma11, test_data_numeric)
print("Accuracy of ARIMA(1,1) on Testing Set:")
print(accuracy_arma11)

# Calculate accuracy for ARIMA(2,1)
accuracy_arma21 <- calculate_accuracy(forecasts_arma21, test_data_numeric)
print("Accuracy of ARIMA(2,1) on Testing Set:")
print(accuracy_arma21)









n_total <- length(data$value)
n_test <- 12 # Hold out the last 12 months for testing
n_train <- n_total - n_test

train_data <- insee_data$value[1:n_train]
test_data <- data$value[(n_train + 1):n_total]



fit_arma11_train <- arima(train_data, order = c(1, 1, 1))
fit_arma21_train <- arima(train_data, order = c(2, 1, 1))



forecast_arma11_test <- forecast(fit_arma11_train, h = n_test)
forecast_arma21_test <- forecast(fit_arma21_train, h = n_test)






calculate_accuracy <- function(forecast_values, actual_values) {
  n <- length(actual_values)
  residuals <- as.numeric(forecast_values) - as.numeric(actual_values)
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(residuals))
  mape <- mean(abs(residuals / actual_values)) * 100
  me <- mean(residuals)
  
  return(data.frame(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    ME = me
  ))
}

forecasts_arma11 <- as.numeric(forecast_arma11_test$mean)
forecasts_arma21 <- as.numeric(forecast_arma21_test$mean)
test_data_numeric <- as.numeric(coredata(test_data))

accuracy_arma11 <- calculate_accuracy(forecasts_arma11, test_data_numeric)
accuracy_arma21 <- calculate_accuracy(forecasts_arma21, test_data_numeric)

print("Accuracy of ARIMA(1,1) on Testing Set:")
print(accuracy_arma11)

print("Accuracy of ARIMA(2,1) on Testing Set:")
print(accuracy_arma21)









# FORCASTING
library(forecast)
par(mfrow = c(2, 1))  # 1 row, 2 columns


split_ratio <- 0.8
n <- nrow(data_diff)
train_size <- floor(split_ratio * n)

train_data <- data_diff$diff_value[1:train_size]
test_data  <- data_diff$diff_value[(train_size + 1):n]




fit_arma11 <- arima(train_data, order = c(1,0,1))
fit_arma21 <- arima(train_data, order = c(2,0,1))

h <- length(test_data)

forecast_arma11 <- forecast(fit_arma11, h = h)
forecast_arma21 <- forecast(fit_arma21, h = h)



# Define start point of forecast on the x-axis
forecast_start <- (train_size + 1):(train_size + h)

# Plot actual test data
plot(test_data, type = 'l', col = 'black', lwd = 2,
     main = 'Forecast vs Actual (Differenced Data)',
     ylab = 'diff_value', xlab = 'Time Index')

# Add forecast lines with correct x-index
lines(forecast_start, forecast_arma11$mean, col = 'red', lwd = 2)
lines(forecast_start, forecast_arma21$mean, col = 'blue', lwd = 2)

# Add legend
legend("topright", legend = c("Actual", "ARMA(1,1)", "ARMA(2,1)"),
       col = c("black", "red", "blue"), lty = 1, lwd = 2)

print(forecast_arma11$mean)
print(length(forecast_arma11$mean))
print(length(test_data))

plot(1:length(test_data), test_data, type = 'l', col = 'black', lwd = 2,
     main = 'Forecast vs Actual (Differenced Data)',
     ylab = 'diff_value', xlab = 'Time Index')

lines(1:length(test_data), forecast_arma11$mean, col = 'red', lwd = 2)
lines(1:length(test_data), forecast_arma21$mean, col = 'blue', lwd = 2)



actual


# Assuming your data is a time series object (e.g., 'actual' is your time series data)
train <- window(ts_data, end = c(2014, 12))  # Train up to December 2014
test <- window(ts_data, start = c(2015, 1))  # Test from January 2015 onwards

# Fit an ARIMA model to the training data
fit <- auto.arima(train)
fit <- auto.arima(train, max.p=2, max.q=2, seasonal=FALSE,ic="bic", stepwise=TRUE,approximation=FALSE)  # or use your fit_arma11

summary(fit)


# Forecast the same number of periods as the test set length
forecast_vals <- forecast(fit, h = length(test))

# Plot the forecast with the actual values
plot(forecast_vals)
lines(test, col = 'blue')  # Add the test data to the plot for comparison




accuracy(forecast_vals, test)








fit <- arima(data_diff$diff_value, order = c(1, 0, 1))  # or use your fit_arma11
forecast_arma <- forecast(fit, h = 10)

plot(forecast_arma)



# Suppose your original last value (before differencing) was:
last_value <- tail(data$value, 1)

# Invert differencing for ARMA(1,1)
forecast_original_arma11 <- cumsum(forecast_arma11$mean) + last_value

# Plot it
plot(forecast_original_arma11, type = 'l', col = 'red', lwd = 2,
     main = 'Forecasted Original Values (ARMA(1,1))', ylab = 'Original Value', xlab = 'Time')



# Forecast on test set horizon
h <- length(test_data)
fc <- forecast(fit_arma11, h = h)

# Compare with actual test data
accuracy(fc, test_data)







ts_data2 <- ts(data$value, start = c(1999, 2), frequency = 12)


# Step 1: Forecast for the next 3 months
h <- 3  # Forecast horizon = 3 months
fc <- forecast(fit_arma11, h = h)

# Step 2: Inverse differencing to get forecasted values in the original scale
# Assuming your last value from the original series is the last observed value.
last_original_value <- tail(ts_data2, 1)  # Last value of the original time series
forecasted_original <- last_original_value + cumsum(fc$mean)  # Forecast values on original scale

# Step 3: Extract actual values for the first 3 months from test_data (adjust accordingly)
# Example: Assuming the first 3 months in the test set are Jan, Feb, and Mar
actual_values <- test_data[1:3]  # Adjust if your test data is for different months
# Extracting forecasted values for Jan 2020, Feb 2020, and Mar 2020
forecast_dates <- seq(from = as.Date("2017-01-01"), by = "month", length.out = length(forecasted_original))

# Create a time series object with these dates
forecasted_original_ts <- ts(forecasted_original, start = c(2017, 1), frequency = 12)
forecasted_original_ts
# Now, let's extract the forecasted values for the 3 months in 2020
forecasted_2020 <- window(forecasted_original_ts, start = c(2020, 1), end = c(2020, 3))

# Now, you can create the actual values for the same months (replace with your actual data)
# Extract actual values from a time series object (for Jan, Feb, Mar 2020)
# Correct way to extract values for Jan to Mar 2020

actual_values <- window(ts_data, start = c(2020, 1), end = c(2020, 3))
ts_data2
print(actual_values)
print(actual_values)

# Create a data frame for easy visualization
forecast_results <- data.frame(
  Month = c("Jan 2020", "Feb 2020", "Mar 2020"),
  Predicted = forecasted_2020,
  Actual = actual_values,
  Error = actual_values - forecasted_2020
)
# Print the results
print(forecast_results)


















# Forcast stage

# Load libraries
library(forecast)
library(ggplot2)

# Forecast next 24 months
forecast_arma21 <- forecast(fit_arma21, h = 12)

# Improved ggplot2-based forecast plot
autoplot(forecast_arma21) +
  ggtitle("Forecast from ARMA(2,1) Model") +
  xlab("Year") +
  ylab("Differenced Series") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )







library(ggplot2)
library(forecast)
library(scales)

# Forecast next 24 months
forecast_arma21 <- forecast(fit_arma21, h = 10)
forecast_arma11 <- forecast(fit_arma11, h = 10)

# Enhanced forecast plot with both ARMA(2,1) and ARMA(1,1)
autoplot(forecast_arma21, series = "ARMA(2,1)") +
  autolayer(forecast_arma11, series = "ARMA(1,1)") +
  ggtitle("📈 Forecast — ARMA(2,1) vs ARMA(1,1), Next 24 Months") +
  xlab("Year") +
  ylab("Differenced Time Series") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA)
  ) +
  geom_vline(xintercept = max(time(fitted(fit_arma21))), linetype = "dotted", color = "red") +
  annotate("text",
           x = max(time(fitted(fit_arma21))) + 0.5,
           y = Inf, label = "Forecast Begins →",
           vjust = 2, hjust = 0, color = "red", fontface = "italic", size = 4)




library(ggplot2)
library(forecast)
library(scales)

# Forecast next 24 months
forecast_arma21 <- forecast(fit_arma21, h = 24)
forecast_arma11 <- forecast(fit_arma11, h = 24)

# Prepare CI ribbon data for ARMA(2,1)
df_ci <- data.frame(
  Time = as.numeric(time(forecast_arma21$mean)),
  Lo95 = forecast_arma21$lower[, 2],
  Hi95 = forecast_arma21$upper[, 2]
)

# Plot
autoplot(forecast_arma21, series = "ARMA(2,1)", PI = FALSE) +
  autolayer(forecast_arma11, series = "ARMA(1,1)", PI = FALSE) +
  autolayer(fitted(fit_arma21), series = "Actual", color = "blue") +
  geom_ribbon(
    data = df_ci,
    aes(x = Time, ymin = Lo95, ymax = Hi95),
    fill = "yellow", alpha = 0.2, inherit.aes = FALSE
  ) +
  ggtitle("📈 Forecast — ARMA(2,1) vs ARMA(1,1), Next 24 Months") +
  xlab("Year") +
  ylab("Differenced Time Series") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(values = c("Actual" = "blue", "ARMA(2,1)" = "#E4572E", "ARMA(1,1)" = "#4C78A8")) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.title = element_blank()
  ) +
  geom_vline(xintercept = max(time(fitted(fit_arma21))), linetype = "dotted", color = "red") +
  annotate(
    "text",
    x = max(time(fitted(fit_arma21))) + 0.5,
    y = Inf,
    label = "Forecast Begins →",
    vjust = 2,
    hjust = 0,
    color = "red",
    fontface = "italic",
    size = 4
  )




library(ggplot2)
library(forecast)
library(scales)

# Forecast next 24 months for ARMA(2,1)
forecast_arma11 <- forecast(fit_arma11, h = 24)

# Prepare CI ribbon data for ARMA(2,1)
df_ci <- data.frame(
  Time = as.numeric(time(forecast_arma11$mean)),
  Lo95 = forecast_arma11$lower[, 2],
  Hi95 = forecast_arma11$upper[, 2]
)

# Plot
autoplot(forecast_arma11, series = "ARMA(1,1)", PI = FALSE) +
  autolayer(fitted(fit_arma11), series = "Actual", color = "blue") +
  geom_ribbon(
    data = df_ci,
    aes(x = Time, ymin = Lo95, ymax = Hi95),
    fill = "yellow", alpha = 0.2, inherit.aes = FALSE
  ) +
  ggtitle("📈 Forecast — ARMA(1,1), Next 24 Months") +
  xlab("Year") +
  ylab("Differenced Time Series") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(values = c("Actual" = "blue", "ARMA(1,1)" = "#E4572E")) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.title = element_blank()
  ) +
  geom_vline(xintercept = max(time(fitted(fit_arma11))), linetype = "dotted", color = "red") +
  annotate(
    "text",
    x = max(time(fitted(fit_arma11))) + 0.5,
    y = Inf,
    label = "Forecast Begins →",
    vjust = 2,
    hjust = 0,
    color = "red",
    fontface = "italic",
    size = 4
  )











# Load necessary libraries
library(forecast)
library(ggplot2)
library(scales)
library(ggfortify)

# Forecasts
forecast_arma21 <- forecast(fit_arma21, h = 24)
forecast_arma11 <- forecast(fit_arma11, h = 24)

# Create a common forecast date sequence
forecast_dates <- seq(max(ts_dates) + 1, by = "month", length.out = 24)

# Convert to data frame manually to avoid ts issues
forecast_df_21 <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(forecast_arma21$mean),
  Lo80 = as.numeric(forecast_arma21$lower[,1]),
  Hi80 = as.numeric(forecast_arma21$upper[,1]),
  Lo95 = as.numeric(forecast_arma21$lower[,2]),
  Hi95 = as.numeric(forecast_arma21$upper[,2]),
  Model = "ARMA(2,1)"
)

forecast_df_11 <- data.frame(
  Date = forecast_dates,
  Forecast = as.numeric(forecast_arma11$mean),
  Lo80 = as.numeric(forecast_arma11$lower[,1]),
  Hi80 = as.numeric(forecast_arma11$upper[,1]),
  Lo95 = as.numeric(forecast_arma11$lower[,2]),
  Hi95 = as.numeric(forecast_arma11$upper[,2]),
  Model = "ARMA(1,1)"
)

# Now they can safely be combined
combined_forecast_df <- rbind(forecast_df_21, forecast_df_11)

# Optional: outlier region
outlier_zone <- data.frame(
  xmin = as.Date("2015-01-01"),
  xmax = max(ts_dates),
  ymin = -Inf, ymax = Inf
)

# Plot
ggplot() +
  # Outlier region
  geom_rect(data = outlier_zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "lightpink", alpha = 0.3) +
  # Historical data
  geom_line(data = observed_df, aes(x = Date, y = Value), color = "#1f77b4", size = 1) +
  
  # Forecast intervals (95%)
  geom_ribbon(data = combined_forecast_df, aes(x = Date, ymin = Lo95, ymax = Hi95, fill = Model),
              alpha = 0.25) +
  
  # Forecast lines
  geom_line(data = combined_forecast_df, aes(x = Date, y = Forecast, color = Model), size = 1) +
  
  # Titles and theme
  labs(title = "Forecast Comparison: ARMA(2,1) vs ARMA(1,1)",
       subtitle = "With 80% and 95% Confidence Intervals and Outlier Region (post-2015)",
       x = "Date", y = "Differenced Series") +
  scale_x_date(limits = as.Date(c("2010-01-01", max(combined_forecast_df$Date))),
               date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("ARMA(2,1)" = "#e6550d", "ARMA(1,1)" = "#31a354")) +
  scale_fill_manual(values = c("ARMA(2,1)" = "#c7e9b4", "ARMA(1,1)" = "#fdd0a2")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray80")
  )
























library(ggplot2)
library(forecast)

# Actual values
actual <- ts_data

# Fitted (forecasted) values
fitted_31 <- fitted(fit_arma31)
fitted_21 <- fitted(fit_arma21)
fitted_11 <- fitted(fit_arma11)

# Combine into a data frame for plotting
comparison_df <- data.frame(
  Time = time(actual),
  Actual = as.numeric(actual),
  ARMA31 = as.numeric(fitted_31),
  ARMA21 = as.numeric(fitted_21),
  ARMA11 = as.numeric(fitted_11)
)

# Melt for ggplot
library(reshape2)
long_df <- melt(comparison_df, id.vars = "Time", 
                variable.name = "Series", value.name = "Value")

# Plot
ggplot(long_df, aes(x = Time, y = Value, color = Series)) +
  geom_line(size = 0.9) +
  labs(title = "Actual vs Fitted (Forecasted) Values",
       y = "Differenced Value", x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red", "blue", "green"),
                     labels = c("Actual", "ARMA(3,1)", "ARMA(2,1)", "ARMA(1,1)")) +
  theme(legend.title = element_blank())










library(tidyr)
library(dplyr)
library(ggplot2)

# Reshape data
comparison_long <- comparison_df %>%
  pivot_longer(cols = -Time, names_to = "Series", values_to = "Value")

# Separate actual from model fits
comparison_long$Model <- ifelse(comparison_long$Series == "Actual", "Actual", gsub("ARMA", "", comparison_long$Series))

# Plot with facets per model
ggplot(comparison_long, aes(x = Time, y = Value, color = Series)) +
  geom_line(size = 0.9) +
  facet_wrap(~Model, scales = "free_y", ncol = 1) +
  labs(title = "Actual vs Fitted (Forecasted) per Model",
       x = "Time", y = "Differenced Value") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "black", "ARMA31" = "red", "ARMA21" = "blue", "ARMA11" = "green")) +
  theme(legend.position = "bottom", legend.title = element_blank())






residuals_df <- comparison_df %>%
  mutate(
    Resid_ARMA31 = Actual - ARMA31,
    Resid_ARMA21 = Actual - ARMA21,
    Resid_ARMA11 = Actual - ARMA11
  )

residuals_long <- residuals_df %>%
  select(Time, starts_with("Resid")) %>%
  pivot_longer(cols = -Time, names_to = "Model", values_to = "Residual")

ggplot(residuals_long, aes(x = Time, y = Residual, color = Model)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals over Time", y = "Residual", x = "Time") +
  theme_minimal() +
  scale_color_manual(values = c("Resid_ARMA31" = "red", "Resid_ARMA21" = "blue", "Resid_ARMA11" = "green")) +
  theme(legend.title = element_blank())






# for seasonality checking
data_diff |>
  gg_season(value, labels = "both") +
  labs(y = "",
       title = "Seasonal plot:")
data_diff |>
  gg_season(diff_value, labels = "both") +
  labs(y = "",
       title = "Seasonal plot:")

gg_subseries(data_diff, value)
gg_subseries(data_diff, diff_value)

data_diff

#  Lag plots
data_diff |>
  gg_lag(diff_value, geom = "point") +
  labs(x = "lag(Beer, k)")


data_diff |> autoplot(diff_value)


data_transformed |> autoplot(boxcox_value)
data_diff24 <- data_transformed %>%
  mutate(diff_value24 = difference(boxcox_value, lag = 12))
data_diff24 %>%
  autoplot(diff_value24)








install.packages("urca")
library(fable)
model_fit <- data_diff %>%
  model(ARIMA(diff_value))
report(model_fit)

model_ma2 <- data_diff %>%
  model(MA2 = ARIMA(diff_value ~ pdq(1, 12, 1)))
report(model_ma2)


library(forecast)
model_auto <- auto.arima(data_diff$diff_value)
summary(model_auto)
library(forecast)
roots <- arima.sim(model = list(order = c(2, 0, 0), seasonal = list(order = c(0, 0, 0), period = 12)))
plot(roots)








### this is very importnat 
min_val <- min(data_diff$diff_value, na.rm = TRUE)
min_val
shifted_data <- data_diff %>% mutate(log_shifted = log(diff_value - min_val + 1))
shifted_data |> autoplot(log_shifted)
shifted_data
par(mfrow = c(2, 1))  # 1 row, 2 columns
acf(na.omit(shifted_data$log_shifted), lag.max = 60)
pacf(na.omit(shifted_data$log_shifted), lag.max = 60)







data_2 <- data_diff %>%
  mutate(log_like = asinh(diff_value))
par(mfrow = c(2, 1))  # 1 row, 2 columns
acf(na.omit(data_2$log_like), lag.max = 60)
pacf(na.omit(data_2$log_like), lag.max = 60)






# second difference
data_diff2 <- data_diff %>%
  mutate(diff2_value = difference(diff_value))
par(mfrow = c(2, 1))  # 1 row, 2 columns
acf(na.omit(data_diff2$diff2_value), lag.max = 60)
pacf(na.omit(data_diff2$diff2_value), lag.max = 60)
acf(na.omit(data_diff$diff_value), lag.max = 60)
pacf(na.omit(data_diff$diff_value), lag.max = 60)

