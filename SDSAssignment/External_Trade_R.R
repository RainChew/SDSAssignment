library(forecast)
library(ggplot2)
library(ggfortify)
library(MASS)
library(tseries)
library(zoo)
library(urca)

#step 0 - Read Dataset
df <- read.csv("External_Trade_Monthly.csv")
head(df)

# Rename Period into date
colnames(df)[colnames(df) == "Period"]<-"date"

# Convert date format
df$date<-as.Date(df$date, format="%m/%d/%Y")
#view again to check character convert to date format
summary(df)
str(df)

#numeric
df$Gross_Exports <- as.numeric(gsub(",", "", df$Gross_Exports))
df$Gross_Imports <- as.numeric(gsub(",", "", df$Gross_Imports))
df$Total_Trade <- as.numeric(gsub(",", "", df$Total_Trade))
df$Trade_Balance <- as.numeric(gsub(",", "", df$Trade_Balance))
head(df)

#information for the data
summary(df)
str(df)

# check the duplicate rows
duplicated_rows <- duplicated(df$date)
sum(duplicated_rows)
# check the missing value
is.null(df)

df_ts<-ts(df, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(df_ts, xlab = "Year", main = "Monthly External Trade Data(RM)" )
#t is Date
t <- df$date
Y <- df$Trade_Balance
acf(Y,lag=24)
pacf(Y,lag=24)
Y_ts<-ts(Y, frequency = 12, start=c(2010,1))
plot.ts(Y_ts, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
#Split data
Y_train<-window(Y_ts, start=c(2010,1),end=c(2017,12))
Y_test<-window(Y_ts, start=c(2018,1))
plot.ts(Y_train, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
# Check stationary using raw dataset (adf,acf,pacf)
# Examine the distribution 
hist(Y_train, main = "Histogram of Trade Balance(RM)", xlab = "Trade Balance(RM)")

# Box plot
boxplot(Y_train, main = "Box Plot of Trade Balance(RM)", ylab = "Trade Balance(RM)")
adf.test(Y_train)
acf(Y_train, lag=24, col = "blue")
pacf(Y_train , lag=24 , col = "blue")
checkresiduals(Y_train)
summary(Y_train)

ts_data <- ts(Y_train, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition <- decompose(ts_data)
plot(decomposition) # Plot the decomposition components (trend, seasonal, and remainder)

# Visual Inspection of Trend Component
plot(decomposition$trend, main = "Trend Component", xlab = "Date", ylab = "Trend")

# Visual Inspection of Seasonal Component
plot(decomposition$seasonal, main = "Seasonal Component", xlab = "Date", ylab = "Seasonal")

# Visual Inspection of Residual Component
plot(decomposition$random, main = "Residual Component", xlab = "Date", ylab = "Residual")


# # Check Transformation or not
# skew <- skewness(Y_train)
# print(skew)
#Check differencing
ndiffs(Y_train)
nsdiffs(Y_train)
adf.test(Y_train)
#seasonal differencing
diff_Y <- diff(Y_train , lag=12)
ndiffs(diff_Y)
nsdiffs(diff_Y)
acf(diff_Y,lag=24)
pacf(diff_Y,lag=24)
adf.test(diff_Y)

diff_Y <- diff(diff_Y,differences = 1)
acf(diff_Y,lag=24)
pacf(diff_Y,lag=24)
adf.test(diff_Y)
checkresiduals(diff_Y)
# STEP 4
# Model
# SARIMA
sarima113011 = arima(x = diff_Y,order= c(1,1,3),seasonal=list(order=c(0,1,1),period=12))
sarima113011
summary(sarima113212)
arima(x = diff_Y, order = c(1, 1, 3), seasonal = list(order = c(0, 1, 1), period = 12))

# Coefficients:
#   ar1      ma1      ma2     ma3     sar1     sar2     sma1     sma2
# -0.7887  -0.9465  -0.3730  0.3205  -1.2558  -0.2576  -0.0234  -0.9702
# s.e.   0.1729   0.2231   0.3235  0.1722   0.1675   0.1661   0.2348   0.2326
# 
# sigma^2 estimated as 8495759:  log likelihood = -679.69,  aic = 1377.37
# 
# Training set error measures:
#   ME    RMSE     MAE     MPE     MAPE      MASE       ACF1
# Training set 85.41577 2676.77 2043.34 103.851 193.1698 0.3890106 0.01247446t

checkresiduals(sarima113011)
# Ljung-Box test
# 
# data:  Residuals from ARIMA(1,1,3)(0,1,1)[12]
# Q* = 33.145, df = 12, p-value = 0.0009188
# 
# Model df: 5.   Total lags used: 17fit <- auto.arima(diff_Y)
fit_sarima <-auto.arima(diff_Y,ic = "aic",trace = TRUE)
summary(fit_sarima)
auto.arima(diff_Y,ic = "aic",trace = TRUE)
# Best model: ARIMA(2,0,1)(2,0,0)[12]
library(lmtest)
coeftest(sarima113011)
aic_value <- AIC(sarima113011)
aic_value



# sarima_model <- arima(diff_Y, order = c(2, 0, 1), seasonal = list(order = c(2, 0, 0), period = 12))
# summary(sarima_model)
# Coefficients:
#   ar1     ar2      ma1     sar1     sar2
# 0.1754  0.3523  -0.9544  -0.5711  -0.1981
# s.e.  0.1217  0.1145   0.0539   0.1200   0.1210
# 
# sigma^2 = 7472646:  log likelihood = -775.06
# AIC=1562.12   AICc=1563.23   BIC=1576.64
# 
# Training set error measures:
#   ME     RMSE      MAE      MPE     MAPE      MASE        ACF1
# Training set 282.21 2649.997 2165.336 8.824996 284.2446 0.4275203 0.003777608
# Box.test(resid(fit),lag=24,type = c("Ljung-Box"))
# checkresiduals(sarima_model)
# Box-Ljung test
# 
# data:  resid(fit)
# X-squared = 23.102, df = 24, p-value = 0.5138
coeftest(fit_sarima)
acf(residuals(fit_sarima))
pacf(residuals(fit_sarima))
checkresiduals(fit_sarima)

sarima_forecasts <- forecast(fit_sarima, h = length(Y_test))
accuracy_metrics <- accuracy(sarima_forecasts, Y_test)
print(accuracy_metrics)
# Plot the SARIMA forecasts and actual data
plot(sarima_forecasts, main = "SARIMA Forecast vs. Actual")
lines(Y_test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

# ETS Model
# ETS 
fit <- ets(diff_Y)
fit
summary(fit)
autoplot(fit)
# Make forecasts
# forecast_values <- forecast((fit), h = 12)  # Forecast for the next 12 periods
checkresiduals(fit)
forecast<-forecast(fit)
forecast
plot(forecast(fit))
# Print the forecasted values
print(forecast_values)
library(stats)
fit1 <- ets(Y_train, model="ANN", alpha=1e-04)
summary(fit1)
accuracy(forecast(fit1), Y_test)
checkresiduals(fit1)
plot(forecast(fit1))

# simple exponential smoothing
# Estimating the level of time series using simple exponential
es1 <- HoltWinters(diff_Y, alpha=1e-04, beta=FALSE, gamma=FALSE)
es1
plot(es1)

# Call SSE and calculate the MSE
es1$SSE
MSE <- es1$"SSE"/(length(diff_Y))
MSE

# Check randomness of the residuals
checkresiduals(es1)

# Computes predictions and prediction intervals
forecast <- predict(es1, n.ahead=10, prediction.interval=T,
                    level=.95)
forecast
plot(es1,forecast)

