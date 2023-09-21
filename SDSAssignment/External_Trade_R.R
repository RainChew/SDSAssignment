library(forecast)
library(ggplot2)
library(ggfortify)
library(MASS)

#step 0
df <- read.csv("External_Trade_Monthly.csv")
head(df)


#numeric
df$Gross_Exports <- as.numeric(gsub(",", "", df$Gross_Exports))
df$Gross_Imports <- as.numeric(gsub(",", "", df$Gross_Imports))
df$Total_Trade <- as.numeric(gsub(",", "", df$Total_Trade))
df$Trade_Balance <- as.numeric(gsub(",", "", df$Trade_Balance))
head(df)

summary(df)
str(df)
# check the missing value
is.null(df)
#select columns
Y <- df$Trade_Balance
x <- df$Period
#Plot
df_ts<-ts(Y, frequency = 12, start=c(2010,1), end=c(2019,12))
plot.ts(df_ts, ylab = "Trade Balance(RM)(millions)", xlab = "Period", main = "Monthly Trade Balance(RM)")



df2 <- df[,c(1,5)]

# Assuming your data has a 'date' column and a 'trade_value' column

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

# You can also plot ACF and PACF of residuals to identify any autocorrelation
acf(residual_component)
pacf(residual_component)

# determine the time series plot
df <- ts(df, frequency = 12, start =c(2010,1) ,end = c(2019,12))
# df <- log(df)
plot.ts(df, ylab = "Trade Balance(RM)(millions)", xlab = "Period", main = "Monthly Trade Balance(RM)")

#Drop Variable
y <- df[,c(5)]
x <- df[,c(1)]

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
model <- lm(y ~ x)

# Find optimal lambda for Box-Cox transformation
library(MASS)
bc <- boxcox(y ~ x)
(lambda <- bc$x[which.max(bc$y)])

# Perform the Box-Cox transformation on y
transformed_y <- (y^lambda - 1) / lambda

# Fit a new linear regression model using the transformed y
new_model <- lm(transformed_y ~ x)
#step3 
library(tseries)
ndiffs(y)

# Apply non-seasonal differencing
differenced_y <- diff(y, differences = 1)
ndiffs(differenced_y)

# Plot the differenced series
plot.ts(differenced_y, ylab = "Differenced Trade Balance(RM)(millions)", xlab = "Years", main = "Differenced Trade Balance(RM)(2010-2019)")

# Perform Augmented Dickey-Fuller test to check for stationarity
adf.test_result <- adf.test(differenced_y)
print(adf.test_result)
sarima_model <- auto.arima(differenced_y, seasonal = TRUE)
print(sarima_model)
checkresiduals(ets_model)
#step 7
library(forecast)
fit <- auto.arima(y)
summary(fit)
auto.arima(y, ic="aic", trace=TRUE)

