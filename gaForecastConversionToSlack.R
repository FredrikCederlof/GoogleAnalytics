# For more information about Slackr: https://github.com/hrbrmstr/slackr
# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/

library(googleAnalyticsR)
library(forecast)
library(slackr)
library(cronR)
#devtools::install_github("hrbrmstr/slackr")

# check if file exists
# file.exists("~/.slackr")

# remove global variables
# rm(list = ls())

# call setup slack from Renviron file .slackr
slackrSetup()
    
# google analytics authentication
ga_auth()
my_id <- 11111111

# declare time intervall
YearAgo <- Sys.Date() - 365
Yesterday <- Sys.Date() - 1

df <- google_analytics(my_id, 
               date_range = c(YearAgo, Yesterday),
               metrics = c("goal9Completions", "users"),
               dimensions = c("date"),
               anti_sample = TRUE, 
               max = -1) 

# calculate forecast
conversions <- ts(df, start=c(2018,1), end=c(2018,12),frequency=365)
conversionsForecast <- forecast(auto.arima(df$goal6Completions))

# create new data frame based on calculated forecast
df2 = data.frame(conversionsForecast)
df2 <- round(df2, 1)

# calculate weekly mean forecast values and round zero decimals
WeeklyForecast <- sum(head(df2$Point.Forecast,7))
WeeklyForecast <- round(WeeklyForecast, 0)

# prepare message before pusing to Slack
message <- paste("Forecasted conversions for this week is", WeeklyForecast, ":thumbsup:.", sep=" ")

text_slackr(preformatted = FALSE, message)

# send simple message to Slack
#slackr(message)
