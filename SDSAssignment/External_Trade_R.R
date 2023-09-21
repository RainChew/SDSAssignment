library(forecast)
library(ggplot2)

df <- read.csv("External_Trade_Monthly.csv")
print(df)
df$Gross_Exports <- as.numeric(gsub(",", "", df$Gross_Exports))
df$Gross_Imports <- as.numeric(gsub(",", "", df$Gross_Imports))
df$Total_Trade <- as.numeric(gsub(",", "", df$Total_Trade))
df$Trade_Balance <- as.numeric(gsub(",", "", df$Trade_Balance))
df
df <- ts(df, frequency = 12, start =c(2010,1))
# df <- log(df)
plot.ts(df)
#Drop Variab1le
df2 <- df[,5]
plot.ts(df2)

library(forecast)
fit <- auto.arima(df2)
summary(fit)
auto.arima(df2, ic="aic", trace=TRUE)

