library(forecast)
library(ggplot2)
library(MASS)
#step 1
df <- read.csv("External_Trade_Monthly.csv")
print(df)
#numeric
df$Gross_Exports <- as.numeric(gsub(",", "", df$Gross_Exports))
df$Gross_Imports <- as.numeric(gsub(",", "", df$Gross_Imports))
df$Total_Trade <- as.numeric(gsub(",", "", df$Total_Trade))
df$Trade_Balance <- as.numeric(gsub(",", "", df$Trade_Balance))
df
df <- ts(df, frequency = 12, start =c(2010,1))
# df <- log(df)
plot.ts(df, ylab = "Trade Balance(RM)(millions)", xlab = "Period", main = "Monthly Trade Balance(RM)")
#Drop Variable
y <- df[,c(5)]
x <- df[,c(1)]
#show plot
#y = trade balance
plot.ts(y, ylab = "Trade Balance(RM)(millions)",xlab="Years", main = "Monthly Trade Balance(RM)")
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

