install.packages("lmtest")

library(forecast)
library(ggplot2)
library(ggfortify)
library(MASS)
library(tseries)
library(zoo)
library(urca)
library(randtests)
library(lmtest)


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
Y<-ts(Y, frequency = 12, start=c(2010,1),end=c(2019,12))
plot.ts(Y, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
#-----------------------------------------------------
#Split data
Y_train<-window(Y, start=c(2010,1),end=c(2017,12))
Y_test<-window(Y, start=c(2018,1))

plot.ts(Y_train, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
#----------------------------------------------
# Check stationary using raw dataset (adf,acf,pacf)
# Examine the distribution 
hist(Y, main = "Histogram of Trade Balance(RM)", xlab = "Trade Balance(RM)")




# # Box plot
# boxplot(Y, main = "Box Plot of Trade Balance(RM)", ylab = "Trade Balance(RM)")
plot.ts(Y, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
adf.test(Y)
Y_test
kpss_test_result <- ur.kpss(Y, type = "tau")
kpss_test_result
acf(Y, lag=24, col = "blue")
pacf(Y , lag=24 , col = "blue")
checkresiduals(Y)
# Perform the Cox-Stuart trend test
cox_stuart_test_result <- cox.stuart.test(Y)
print(cox_stuart_test_result)
summary(Y)
# ts_data <- ts(Y, frequency = 12)  # Assuming monthly data (frequency = 12)
decomposition <- decompose(Y)
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
ndiffs(Y)
nsdiffs(Y)
adf.test(Y)
#seasonal differencing
diff_Y <- diff(Y , lag=12)
ndiffs(diff_Y)
nsdiffs(diff_Y)
acf(diff_Y,lag=24)
pacf(diff_Y,lag=24)
adf.test(diff_Y)

# diff_Y <- diff(diff_Y,differences = 1)
# acf(diff_Y,lag=24)
# pacf(diff_Y,lag=24)
adf.test(diff_Y)
checkresiduals(diff_Y)
# STEP 4
# Model
# SARIMA
sarima102111 = arima(x = Y_train,order= c(1,0,2),seasonal=list(order=c(1,1,1),period=12))
sarima102111
summary(sarima102111)
accuracy(sarima102111)
sarima_forecasts <- forecast(sarima102111, h = length(Y_test))
accuracy_metrics <- accuracy(sarima_forecasts, Y_test)
print(accuracy_metrics)
# Plot the SARIMA forecasts and actual data
plot(sarima_forecasts, main = "SARIMA Forecast vs. Actual")
lines(Y_test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)
arima(x = Y_train, order = c(1, 0, 2), seasonal = list(order = c(1, 1, 0), period = 12))
# Coefficients:
#   ar1     ma1     ma2     sar1
# -0.3883  0.6172  0.4064  -0.4168
# s.e.   0.3686  0.3430  0.0937   0.1096
# 
# sigma^2 estimated as 7443045:  log likelihood = -785.07,  aic = 1580.14
# 
# Training set error measures:
#   ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
# Training set -162.8263 2551.994 1853.701 -21.91583 40.73951 0.7632795 0.02352365

checkresiduals(sarima102111)
coeftest(sarima102111)
aic_value <- AIC(sarima102111)
aic_value
# Ljung-Box test
# 
# data:  Residuals from ARIMA(1,1,3)(0,1,1)[12]
# Q* = 33.145, df = 12, p-value = 0.0009188
# 
# Model df: 5.   Total lags used: 17fit <- auto.arima(diff_Y)
fit_sarima <-auto.arima(Y,ic = "aic",trace = TRUE)
summary(fit_sarima)
auto.arima(Y,ic = "aic",trace = TRUE)
# Best model: ARIMA(2,1,1)(2,0,0)[12]
library(lmtest)


fit_sarima <- arima(Y_train, order = c(2, 1, 1), seasonal = list(order = c(2, 0, 0), period = 12))
summary(fit_sarima)

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
AIC(fit_sarima)

sarima_forecasts <- forecast(fit_sarima, h = length(Y_test))
accuracy_metrics <- accuracy(sarima_forecasts, Y_test)
print(accuracy_metrics)
# Plot the SARIMA forecasts and actual data
plot(sarima_forecasts, main = "SARIMA Forecast vs. Actual")
lines(Y_test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

# ETS Model
# ETS 
fit <- ets(Y_train)
fit
summary(fit)
autoplot(fit)
# Make forecasts
# forecast_values <- forecast((fit), h = 12)  # Forecast for the next 12 periods
checkresiduals(fit)
forecast<-forecast(fit)
forecast
plot(forecast(fit))
plot(forecast(fit),main = "ETS Forecast vs. Actual")
lines(Y_test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)
# Print the forecasted values
print(forecast)

library(stats)
fit1 <- ets(Y_train, model="ANA", alpha=0.3151,gamma=1e-04)
summary(fit1)
accuracy(forecast(fit1), Y_test)
checkresiduals(fit1)
ets_forecast = forecast(fit1)
plot(ets_forecast)
plot(ets_forecasts, main = "ETS Forecast vs. Actual")
lines(Y_test, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1)

# Hot Winter Addictive Models
# Estimating the level of time series using simple exponential
decompose = decompose(Y)
plot(decompose)
#Holt-Winter Additive Method
library(forecast)
hw <- HoltWinters(Y_train ,seasonal = "additive")
hw
plot(hw)
summary(forecast(hw))
MSE <- hw$"SSE"/((NROW(Y)-3))
MSE

checkresiduals(hw)

forecast <- predict(object=hw, n.ahead=24, prediction.interval=T,
                    level=.95)
forecast
plot(hw,forecast)
#Holt-Winter Multiplicative Method
library(forecast)
hw2 <- HoltWinters(Y_train ,seasonal = "multiplicative")
hw2
plot(hw2)
summary(forecast(hw2))
MSE <- hw2$"SSE"/(NROW(Y)-3)
MSE

checkresiduals(hw2)

forecast <- predict(object=hw2, n.ahead=24, prediction.interval=T,
                    level=.95)
?predict
forecast
plot(hw2,forecast)
accuracy_metrics <- accuracy(forecast, Y_test)
accuracy_metrics
