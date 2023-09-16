data <- read.csv("External_Trade_Data.csv")
print(data)
library(forecast)
library(ggplot2)
# data$Exports <- as.numeric(gsub(",", "", data$Exports))
# data$Imports <- as.numeric(gsub(",", "", data$Imports))
# data$Total_Trade <- as.numeric(gsub(",", "", data$Total_Trade))
# data$Balance_of_Trade <- as.numeric(gsub(",", "", data$Balance_of_Trade))
# #Drop Variable
# data_ts = subset(data, select = -c(Imports,Total_Trade,Balance_of_Trade,Year))
# data
# data <- ts(data, frequency = 1, start =c(1967,1))
# 
# data <- log(data)
# plot.ts(data)

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

