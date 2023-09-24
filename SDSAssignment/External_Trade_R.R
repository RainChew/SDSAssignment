library(forecast)
library(ggplot2)
library(ggfortify)
library(MASS)
library(tseries)


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
# If Gross_Exports are 
Y <- df$Gross_Exports
export_ts<-ts(Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(export_ts, ylab = "Gross Exports(RM)(millions)", xlab = "Date", main = "Monthly Gross Exports(RM)")
# Check stationary using raw dataset (adf,acf,pacf)
adf.test(export_ts)
acf(export_ts)
pacf(export_ts)
decomposed <- decompose(df_ts)

# If Gross_Imports are 
Y <- df$Gross_Imports
import_ts<-ts(Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(import_ts, ylab = "Gross Imports(RM)(millions)", xlab = "Date", main = "Monthly Gross Imports(RM)")
# Check stationary using raw dataset (adf,acf,pacf)
adf.test(import_ts)
acf(import_ts)
pacf(import_ts)
decomposed <- decompose(import_ts)

# Plot the decomposed components
autoplot(decomposed, main = "Decomposed Components of Time Series")

# Step 2: Analyze Trend
# Extract the trend component from decomposition
trend_component <- decomposed$trend

# If Total_Trade are 
Y <- df$Total_Trade
trade_ts<-ts(Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(trade_ts, ylab = "Total Trade(RM)(millions)", xlab = "Date", main = "Monthly Total Trade(RM)")
# Check stationary using raw dataset (adf,acf,pacf)
adf.test(trade_ts)
acf(trade_ts)
pacf(trade_ts)

# If Trade_Balance are
Y <- df$Trade_Balance
balance_ts<-ts(Y, frequency = 12, start=c(2010,1), end=c(2019,12))
#Check and Remove Outliers
balance_ts_clean = tsclean(balance_ts)
summary(balance_ts_clean)
summary(balance_ts)
plot.ts(balance_ts, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
# Check stationary using raw dataset (adf,acf,pacf)
adf.test(balance_ts)
acf(balance_ts, lag.max=50, col = "blue")
pacf(balance_ts , lag=10 , col = "blue")
?acf
?pacf
decomposed <- decompose(balance_ts)
# Step 2: Analyze Trend
# Extract the trend component from decomposition
trend_component <- decomposed$trend

# Plot the trend component
autoplot(trend_component, ylab = "Trend", main = "Trend Component")

# Step 3: Analyze Seasonality
# Extract the seasonal component from decomposition
seasonal_component <- decomposed$seasonal
# Plot the seasonal component
autoplot(seasonal_component, ylab = "Seasonal Component", main = "Seasonal Component")
decomposition <- decompose(balance_ts, type = "multiplicative")
plot(decomposition)

# Step 4: Analyze Residuals (Random Behavior)
# Extract the residual component from decomposition
residual_component <- decomposed$random
# Plot the residuals
autoplot(residual_component, ylab = "Residuals", main = "Residuals (Random Behavior)")
cbind(decomposed$x,decomposed$trend,decomposed$seasonal,decomposed$random)
# You can also plot ACF and PACF of residuals to identify any autocorrelation
acf(residual_component)
pacf(residual_component)
# Calculate a simple moving average
# Load the zoo package
library(zoo)

# Calculate a simple moving average
ma <- rollmean(Y, k = 12, fill = NA, align = "right")

# Plot the moving average
plot(ma, main = "Moving Average of Time Series Data")





# Check different
ndiffs(Y)
nsdiffs(Y)
bc <- boxcox(Y ~ t)
plot(bc)
(lambda <- bc$x[which.max(bc$y)])


arima_model <- arima(Y, order = c(1, 1, 0))
#log
bc <- boxcox(log_Y ~ t)
plot(bc)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((Y^lambda-1)/lambda) ~ t)
log_Y = log(Y)
balance_log_ts<-ts(log_Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(balance_log_ts, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
ndiffs(log_Y)
adf.test(log_Y)
acf(log_Y)
pacf(log_Y)

#difference
# bc <- boxcox(diff_Y ~ t)
# plot(bc)
(lambda <- bc$x[which.max(bc$y)])
new_model <- lm(((Y^lambda-1)/lambda) ~ t)
diff_Y <- diff(Y, differences = 1)
balance_diff_ts<-ts(diff_Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(balance_diff_ts, ylab = "Trade Balance(RM)(millions)", xlab = "Date", main = "Monthly Trade Balance(RM)")
ndiffs(diff_Y)
adf.test(diff_Y)
acf(diff_Y)
pacf(diff_Y)
# install.packages("urca")
library(urca)
kpss_test_result <- ur.kpss(balance_ts, type = "tau")
kpss_test_result

# Test trend
# Load necessary libraries
library(randtests)

# Generate or load your time series data
# Replace this with your actual time series data
# For example, you can create a time series using `ts()` or read data from a CSV file.
# Example: my_time_series <- ts(your_data, frequency = 12)  # Assuming monthly data
summary(Y)
summary(diff_Y)
# Perform the Cox-Stuart trend test
cox_stuart_test_result <- cox.stuart.test(diff_Y)

# Print the test result
print(cox_stuart_test_result)

# Load necessary libraries (if not already loaded)
library(stats)

# Generate or load your time series data
# Replace this with your actual time series data
# Example: my_time_series <- ts(your_data, frequency = 12)  # Assuming monthly data

# Fit a linear regression model
lm_model <- lm(diff_Y ~ time(diff_Y))

# Check the summary of the regression model
summary(lm_model)

library(Kendall)

mk_test_result <- MannKendall(Y)
print(mk_test_result)
# No trend











# Assuming your data has a 'Period' column and a 'Trade_Balance column

# Step 0: Split the Data into Train and Test Sets

# Set the proportion of data to be used for training (e.g., 80%)
train_proportion <- 0.8

# Calculate the number of rows for the training set
train_rows <- round(nrow(df) * train_proportion)

# Create the training set
train_data <- df2[1:train_rows, ]

# Create the testing set (validation set)
test_data <- df2[(train_rows + 1):nrow(df), ]

# Check the dimensions of the train and test sets
dim(train_data)
dim(test_data)
print(train_data)


# Assuming you have already imported and prepared your time series data as 'df'

# Step 1: Decompose the Time Series
#Select Variable y is Trade Balance and x is Period
y <- df[,c(5)]
x <- df[,c(1)]
y <- ts(y, frequency = 12, start = c(2010, 1))
decomposed <- decompose(y)

# Plot the decomposed components
autoplot(decomposed, main = "Decomposed Components of Time Series")

# Step 2: Analyze Trend
# Extract the trend component from decomposition
trend_component <- decomposed$trend

# Plot the trend component
autoplot(trend_component, ylab = "Trend", main = "Trend Component")

# Step 3: Analyze Seasonality
# Extract the seasonal component from decomposition
seasonal_component <- decomposed$seasonal

# Plot the seasonal component
autoplot(seasonal_component, ylab = "Seasonal Component", main = "Seasonal Component")

# Step 4: Analyze Residuals (Random Behavior)
# Extract the residual component from decomposition
residual_component <- decomposed$random

# Plot the residuals
autoplot(residual_component, ylab = "Residuals", main = "Residuals (Random Behavior)")
cbind(decomposed$x,decomposed$trend,decomposed$seasonal,decomposed$random)
# You can also plot ACF and PACF of residuals to identify any autocorrelation
acf(residual_component)
pacf(residual_component)

# determine the time series plot
df <- ts(df, frequency = 12, start =c(2010,1) ,end = c(2019,12))
# df <- log(df)
plot.ts(df, ylab = "Trade Balance(RM)(millions)", xlab = "Period", main = "Monthly Trade Balance(RM)")


#show plot
#y = trade balance
plot.ts(y, ylab = "Trade Balance(RM)(millions)",xlab="Years", main = "Monthly Trade Balance(RM)")

#adf test
library(tseries)
adf.test(y)
acf(y)
pacf(y)

#step 2
# Fit a linear regression model
model <- lm(Y ~ x)

# Find optimal lambda for Box-Cox transformation
library(MASS)
bc <- boxcox(Y ~ x)
plot(bc)
(lambda <- bc$x[which.max(bc$y)])

# Perform the Box-Cox transformation on y
transformed_y <- (y^lambda - 1) / lambda

# Fit a new linear regression model using the transformed y
new_model <- lm(transformed_y ~ x)
#step3 
library(tseries)
ndiffs(y,alpha=0.05,test="adf")

# Apply non-seasonal differencing
differenced_y <- diff(y, differences = 1)
ndiffs(differenced_y)

# Plot the differenced series
plot.ts(differenced_y, ylab = "Differenced Trade Balance(RM)(millions)", xlab = "Years", main = "Differenced Trade Balance(RM)(2010-2019)")

# Perform Augmented Dickey-Fuller test to check for stationarity
adf.test_result <- adf.test(differenced_y)
print(adf.test_result)

#arima
arima_model <- auto.arima(differenced_y)
checkresiduals(arima_model)
print(arima_model)

#sarima 
sarima_model <- auto.arima(differenced_y, seasonal = TRUE)
print(sarima_model)
checkresiduals(sarima_model)
adf.test(differenced_y)
acf(differenced_y)
pacf(differenced_y)

#ets
ets_model<-ets(differenced_y)
summary(ets_model)
checkresiduals(ets_model)
autoplot(ets_model)

#step 7
library(forecast)
fit <- auto.arima(Y)
summary(fit)
auto.arima(Y, ic="aic", trace=TRUE)
autoplot(fit)
adf.test(Y)
acf(Y)
pacf(Y)
