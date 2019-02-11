## Insight Works AB, www.insightworks.se, fredrik@insightworks.se
library(googleAnalyticsR)
library(ggplot2) 

# You must be logged into your Google Analytics account on your web browser
# Documentation - https://michalbrys.gitbooks.io/r-google-analytics/content/chapter1/data_sources.html
ga_auth()

# Make sure to replace this with your viewId. 
#google_analytics_account_list()
my_id <- change ID

#---------- Data Frame 1 ----------#
## Transactions by Hour and Day of Week
df1 <- google_analytics_4(my_id, 
                          date_range = c("365daysAgo", "yesterday"),
                          metrics = c("transactions"),
                          dimensions = c("dayOfWeek","hour"))

# Average transaction value, third quartile(Q3) means 75% observations are below this quantity(approx)
# Example summary(df1$transaction) & quantile(df1$transaction)
MeanTransactions <- mean(df1$transaction)
MeanTransactions <- round(MeanTransactions, 0)
MeanTransactions


# Create filters on metrics
mf <- met_filter("transactions", "GREATER_THAN", (MeanTransactions))
# Construct filter objects
fc <- filter_clause_ga4(list(mf), operator = "AND")

#---------- Data Frame 2 ----------#
# Average transactions and higher by Hour and Day of Week 
df2 <- google_analytics_4(my_id, 
                          date_range = c("365daysAgo", "yesterday"),
                          metrics = c("transactions"),
                          dimensions = c("dayOfWeek","hour"),
                          met_filters = fc)

# Heatmap transactions
# Change to df2 %>% in order to see only mean values
df1 %>%
  ggplot(aes(x=dayOfWeek, y=hour, fill=transactions)) + 
  geom_tile(aes(fill = transactions), colour = "black")+ 
  geom_text(aes(label = round(transactions, 0.1))) +
    
# Re-order and rename X-scale
  scale_x_discrete(limits=c("1","2","3","4","5","6","0"), 
  labels=c("1" = "Måndag", "2" = "Tisdag","3" = "Onsdag","4" = "Torsdag","5" = "Fredag","6" = "Lördag","0" = "Söndag"))+
  scale_y_discrete(labels=c("00" = "00:00","01" = "01:00","02" = "02:00","03" = "03:00","04" = "04:00","05" = "05:00","06" = "06:00","07" = "07:00","08" = "08:00","09" = "09:00","10" = "10:00","11" = "11:00","12" = "12:00","13" = "13:00","14" = "14:00","15" = "15:00","16" = "16:00","17" = "17:00","18" = "18:00","19" = "19:00","20" = "20:00","21" = "21:00","22" = "22:00","23" = "23:00"))+
  
# Style  
  scale_fill_gradient(low = '#FFFFA6', high = 'red', name="Transaktioner") +
  labs(title = "Transaktioner per veckodag & timme", x="Dag i veckan", y="Timme på dygnet") +
  
# Change the appearance and the orientation angle
  theme(
        plot.title = element_text(size=18),
        axis.text.x = element_text(face="plain", color="black", size=12, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=12, angle=0), 
        rect = element_rect(fill = "transparent"), # bg of the panel
        panel.background = element_blank())

#Customize - http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
#Style - https://rstudio-pubs-static.s3.amazonaws.com/3364_d1a578f521174152b46b19d0c83cbe7e.html
