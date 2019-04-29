# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/
# Article - https://www.insightworks.se/forecast-google-analytics-r-slack/

library(googleAnalyticsR)
library(ggplot2)
library(googlesheets)
library(forecast)

# Google Analytics Authentication
ga_auth()
my_id <- 123456789

# Create data frame
df1 <- google_analytics_4(my_id,
  date_range = c("365daysAgo", "yesterday"),
  metrics = c("transactions", "sessions"),
  dimensions = c("date")
  anti_sample = TRUE)
  
# Visualize data
  ggplot(df1,aes(x=date,y=transactions,group=1)) +
  geom_line() +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90, hjust = ),
  panel.background = element_blank())
  
# Forecast data with ARIMA
  transactions <- ts(df1, start=c(2016,5), end=c(2017,4),frequency=365)
  transactionsforecast <- forecast(auto.arima(df1$transactions))
  
df2 = data.frame(transactionsforecast)
df2 <- round(df2, 1)

# Send data to Google Spreadsheet
  gs_auth()
  PredictTomorrow <- gs_url("https://docs.google.com.../edit?usp=sharing")
  PredictTomorrow <- PredictTomorrow %>%
  gs_edit_cells(ws = "Sheet", input = df2, trim = TRUE)
  
# Compare with last years transactions
  LastYearTransactions <- head(df1$transactions,1)
  YesterdaysTransactions <- tail(df1$transactions,1)
  
# Count diff as %
  TransactionDifference = (YesterdaysTransactions - LastYearTransactions)/LastYearTransactions * 100
  TransactionDifference = as.integer(TransactionDifference)

dftd = data.frame(TransactionDifference)

# Send results to different sheets
if (TransactionDifference > 0) {
   PredictTomorrow <- PredictTomorrow %>% 
   gs_edit_cells(ws = "Positive", input = dftd, trim = TRUE)
} else if (TransactionDifference < 0) {
   PredictTomorrow <- PredictTomorrow %>% 
   gs_edit_cells(ws = "Negative", input = dftd, trim = TRUE)
}
