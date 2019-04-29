# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/

# Blog post https://medium.com/@fredrikcederlof/analyzing-time-between-bookings-and-events-with-r-google-analytics-293f6c334d73

#install(ggplot2)
#install(googleAnalyticsR)
#install(dplyr)
#install(reshape2)

library(googleAnalyticsR)
library(ggplot2)
library(dplyr)
library(reshape2)

# Load Google Analytics Authorization + View ID
  view_id <- xxxxxxxxx
  ga_auth()

# Create your data frame based on given conditions
gadata <- google_analytics_4(view_id, 
                             date_range = c(Sys.Date() -365, Sys.Date() - 1),
                             metrics = "users",
                             dimensions = c("date","transactionId","ga:dimension1","ChannelGrouping"),
                             filters = c("ga:dimension2==b2c"),
                             anti_sample = TRUE)

# Convert dimension booking date (Char) to Date
  gadata$BookingDate <- as.Date(gadata$dimension1)

# Add a new column for days between booking and the event
  gadata$Days <- gadata$BookingDate - gadata$date

# Aggregate days to weeks
  gadata$Days <-gadata$Days / 7

# Round digits towards zero
  gadata$Days <- round(gadata$Days,digits=0)

# Convert the column from datatype difftime to numeric
  gadata$Days <- as.numeric(gadata$Days)

# Rename df column Days to Weeks
  rename(gadata, Days = Weeks)

# Instead of dplyr use following code
  # names(gadata)[names(gadata) == 'Days'] <- 'Weeks'
  
# Plot number of weeks between booking and the event with ggplot2
ggplot(data=gadata, aes(gadata$Weeks)) + 
  geom_histogram(breaks=seq(0, 30, by = 1), 
                 col="#00FA92", 
                 fill="#00C853", 
                 alpha = 0.9) + 
  labs(title="Weeks between online booking and the event", size=26)+
  labs(x="# of Weeks", y="# of Bookings") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.background = element_blank())+
xlim(0,30)
ylim(0,0)

# Remove Channel Group containing source = (Other)
  gadataChannels = gadata[!gadata$C == "(Other)", ]
  gadataChannels = data.frame(gadataChannels)

# Visualize the plot with a density line
#geom_histogram(aes(y=..density..),breaks=seq(0, 30, by = 1),

# Replaced it with a new Y-variable and add geom_density
  geom_histogram(aes(y=..density..),breaks=seq(0, 30, by = 1),
  geom_density(alpha=.3, col="#4294F7", fill="#4294F7")+
  
# Create a new df (ChannelDF) based on Channel and Booking Days 
  BookingChannel = gadata$ChannelGrouping
  BookingDays = gadata$BookingDate - gadata$date 
  ChannelDF = data.frame(BookingChannel, BookingDays)

# Convert difftime to num and double to char
  ChannelDF$BookingDays <- as.numeric(ChannelDF$BookingDays)
  ChannelDF$BookingChannel <- as.character(ChannelDF$BookingChannel)

# Create new df's from each marketing channel 
  Organic <- ChannelDF[BookingChannel == "Organic Search", ]
  Mail <- ChannelDF[BookingChannel == "Email", ]
  Direct <- ChannelDF[BookingChannel == "Direct", ]
  Referral <- ChannelDF[BookingChannel == "Referral", ]
  PaidSearch <- ChannelDF[BookingChannel == "Paid Search", ]
  Social <- ChannelDF[BookingChannel == "Social", ]

# Sum all rows in Organic and create the variable SumOrganic
  SumOrganic <- nrow(Organic)

# Convert Days to Weeks and round to 1 digit
  Organic <- round(Organic$BookingDays/7,digits=1)

# Extract only 0-1 weeks from the variable Organic
  OneWeekOrganic <- length(subset(Organic, Organic < 2))

# Calculating share of total 
  OrganicShare <- OneWeekOrganic/SumOrganic*100
  
# Create two new vectors with values for each channel
  PercentBTE <- c(OrganicShare, MailShare, DirectShare, PaidSearchShare, ReferralShare, SocialShare)
  ChannelGroup <-c("Organic", "Mail", "Direct", "Paid Search", "Referral", "Social Share")

# Round percentage variables and concatenate variables to a new df
  PercentBTE <- round(PercentBTE,digits=2)

# Construct a df based on the two vectors above
  DFCG = data.frame(PercentBTE, ChannelGroup)
  
# Plot data showing BTE for each channel
ggplot(DFCG, aes(x=ChannelGroup, y=PercentBTE)) +
  geom_text(aes(label=PercentBTE), vjust=0) +  
geom_bar(stat="identity", fill = "#80c3e5") +
  scale_y_continuous(limits = c(0,100)) + 
  theme_bw() +
  theme(panel.border = element_blank(), panel.background = element_blank())+
    labs(title="Share of BTE within a 7 days period", size=30) +
   labs(x="Channel Groups", y="% BTE within 7 days")
   
filters = c("ga:deviceCategory==desktop;ga:dimension5==b2c")

# Desktop
  DFCG_Desktop = data.frame(PercentBTE, ChannelGroup) 
  names(DFCG_Desktop)[names(DFCG_Desktop) == 'PercentBTE'] <- 'Desktop'

# Mobile
  DFCG_Mobile = data.frame(PercentBTE, ChannelGroup)
  names(DFCG_Mobile)[names(DFCG_Mobile) == 'PercentBTE'] <- 'Mobile'

# Tablet
  DFCG_Tablet = data.frame(PercentBTE, ChannelGroup)
  names(DFCG_Tablet)[names(DFCG_Tablet) == 'PercentBTE'] <- 'Tablet'

# Merge all of the df above to one (DFCG_Device)
  DFCG_Device = data.frame(DFCG_Desktop$ChannelGroup, DFCG_Desktop$Desktop, DFCG_Mobile$Mobile, DFCG_Tablet$Tablet)

# Rename df columns
  names(DFCG_Device)[names(DFCG_Device) == 'DFCG_Desktop.ChannelGroup'] <- 'ChannelGroup'
  names(DFCG_Device)[names(DFCG_Device) == 'DFCG_Desktop.Desktop'] <- 'Desktop'
  names(DFCG_Device)[names(DFCG_Device) == 'DFCG_Mobile.Mobile'] <- 'Mobile'
  names(DFCG_Device)[names(DFCG_Device) == 'DFCG_Tablet.Tablet'] <- 'Tablet'

# View df
View(DFCG_Device)

DFCG_Device.long<-melt(DFCG_Device)

# Plot data showing BTE for each channel per device category
ggplot(DFCG_Device.long,aes(ChannelGroup,value,fill=variable))+
  geom_bar(stat="identity", position = position_dodge(width = 0.6), width=0.5)+
  scale_fill_manual("legend", values = c("Desktop" = "#13A1FC", "Mobile" = "#052940", "Tablet" = "#0B507F"))+
   geom_text(aes(label = paste(value,"%")), 
   position = position_dodge(width = 0.5), vjust = -0.4, size=3.5) +
theme_bw() +
 theme(panel.border = element_blank(), panel.background = element_blank())+
  theme(axis.text = element_text(size = 11))+
  theme(axis.title = element_text(size = 13))+
   labs(title="Share of BTE per Marketing Channel", size=30) +
  labs(x="Channel Groups", y="Share% BTE within 7 days")+
    guides(fill=guide_legend(title="Device Category"))
  
# Create your data frame based on given conditions
df_paidsearch <- google_analytics_4(view_id, 
    date_range = c("2017-01-01", "2017-12-31"),
    metrics = c("transactions"),
    dimensions = c("dayOfWeek", "hour", "userGender",   "userAgeBracket"),
    filters = c("ga:channelGrouping==Paid Search;ga:dimension5==B2C"),
    anti_sample = TRUE)
    
# Plot the df values wit ggplot heatmapdf_paidsearch %>%
  ggplot(aes(x=dayOfWeek, userGender, y=hour, fill=transactions)) + 
  geom_tile( colour = "black")+
  geom_text(aes(label = transactions),size=3, lineheight=4, colour="black") +
  facet_grid(userGender~userAgeBracket) +
 
 # Re-order and rename X-scale
  scale_x_discrete(limits=c(
    "1","2","3","4","5","6","0"), 
    labels=c(
      "1" = "Monday", 
      "2" = "Thuesday",
      "3" = "Wednesday",
      "4" = "Thursday",
      "5" = "Friday",
      "6" = "Saturday",
      "0" = "Sunday"))+
  
  scale_y_discrete(labels=c(
    "00" = "00:00",
   "01" = "01:00",
    "02" = "02:00",
    "03" = "03:00",
    "04" = "04:00",
    "05" = "05:00",
    "06" = "06:00",
    "07" = "07:00",
    "08" = "08:00",
    "09" = "09:00",
    "10" = "10:00",
    "11" = "11:00",
    "12" = "12:00",
    "13" = "13:00",
    "14" = "14:00",
    "15" = "15:00",
    "16" = "16:00",
    "17" = "17:00",
    "18" = "18:00",
    "19" = "19:00",
    "20" = "20:00",
    "21" = "21:00",
    "22" = "22:00",
    "23" = "23:00"))
 
# Style color of the heatmap
  scale_fill_gradient(low = '#FFFFA6', high = 'red', name="Bookings",limits= c(0,7600)) +
# Declare description texts for title, Y and X scale
  labs(title = "BTE within 7 days from Paid Search split by age and gender", x="Weekday", y="Hour of day") +
  
  # Change the appearance and the orientation angle
  theme(
    axis.text.x = element_text(face="plain", color="black", size=9, angle=90),
    axis.text.y = element_text(face="plain", color="black", size=9, angle=0), 
    axis.line = element_line(colour = "black", size = 0, linetype = "solid"),
    strip.text.x = element_text(size=11, angle=0, face="bold"),
    strip.text.y = element_text(size=11, face="bold"),
    strip.background = element_rect(colour="white", fill="#FFFFFF"),
    panel.background = element_blank())
    
# Determine the advertising cost to gain x revenue
 
 df_ppc_budget <- google_analytics_4(view_id, 
      date_range = c("2017-08-01", "2018-02-22"),
      metrics = c("adCost","transactionRevenue"),
      dimensions = c("date"),
      filters = c("ga:medium==cpc;ga:dimension5==b2c"),
      anti_sample = TRUE)

# Check variables format
str(df_ppc_budget)

# Remove outliers on row 116 - (in this case, black friday)
df_ppc_budget <- df_ppc_budget[-c(116), ]

#Convert date from char to date, as.Date
df_ppc_budget$date <- as.Date(df_ppc_budget$date)

# Check correlation between Cost and Revenue
round(cor(df_paidsearch_budget$adCost, df_paidsearch_budget$transactionRevenue)  ,2)
cor.test(df_paidsearch_budget$adCost, df_paidsearch_budget$transactionRevenue)

# Melt adCost with transactionRevenue to one single column
m.df <- melt(df_ppc_budget, id="date")
#Plot the result with a smooth line
 ggplot(m.df, aes(x = date, y = value, colour = variable)) + 
  geom_smooth(size=2)+
   theme_minimal() + 
   scale_x_date(date_breaks = "1 month", 
       limits = as.Date(c('2017-08-01','2018-02-22')))+
  labs(title = "Correlation study between Cost & Revenue)", x="Date", y="Cost & Revenue") +
  scale_colour_manual(values=c("#FF7E79", "#97B86C")
  
# Build linear regression model. Response goes on the left side of the ~, and the predictor(s) on the right
  linearMod <- lm(adCost~transactionRevenue,data=df_paidsearch_budget)
  sum=summary(linearMod)

#Print the sum
sum

# Call for predicted values for AdCost based on transaction revenue
  predict(linearMod, data.frame(transactionRevenue = c(20000, 15000, 10000)))
  
