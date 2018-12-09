# DRY principle - Don't Repeat Yourself
# Github - https://github.com/amrrs/itunesr

# Testa installera GUI https://github.com/dreamRs/esquisse


# Install following packages
devtools::install_github("amrrs/itunesr")
#remotes::install_github("ropensci/googleLanguageR")

library(itunesr)
library(googleLanguageR)
library(openxlsx)
library(writexl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(plyr)
library(tidyverse)
library(tidytext)

# Spotify iOS App ID
appstoreID = 324684580

# Get information about the Spotify App
getAttributes(appstoreID,'se')

# Create this df for later purpose
df_App_allMarkets = NULL

##### 1. Prepare for looping through all market reviews

# Spotify App Store Coyntry Codes 
appMarketsSpotify <- c("se", "us", "dk", "de", "hu", "it", "nl", "no", "br", "ca", "ch")

# Each page has 51 reviews between each pagination, also Apple only accepts scraping last 500 reviews 
no_ratings <- 500 #500
no_reviews_per_page <- 51
ratings <- no_ratings/no_reviews_per_page

# Round up pagination number & set variables for second and last review page
ratings <- ceiling(ratings)
reviewStartPage <- 2
reviewEndPage <- ratings

##### 2. Loop through all App markets and merge them as one df
for (appMarket in appMarketsSpotify) {
  
# Before looping through all reviews, create data frame with first review page
df_App <- getReviews(appstoreID,appMarket,1)

# Create a for loop and merge all tables in to one single df
for (page in reviewStartPage:reviewEndPage){
  df_App <- rbind(df_App, getReviews(appstoreID,appMarket, page))
}

# Convert 'Date' from POSIXt to Date and sort df by date (ascending)
df_App$Date <- as.Date(df_App$Date)
df_App <- df_App[order(df_App$Date),] 

# Reorder columns in our df by column index and add market suffix to df
df_App <- df_App[c(7, 4, 5, 1, 6, 2, 3)]
df_App$Market=appMarket

# Create df for each local market
df_App_Market <- print(appMarket)
df_App_Market <- df_App

# Bind all markets together to one single df
df_App_allMarkets <- rbind(df_App_allMarkets, df_App_Market)

# Remove dublicated reviews
df_App_allMarkets <- unique(df_App_allMarkets)

### End loop
}

# View df 
View(df_App_allMarkets)

# Sort df by rating 1-5
df_App_allMarkets$Rating <- factor(df_App_allMarkets$Rating, levels = c("1", "2", "3", "4", "5"))

# Convert data types & rename country codes before visualization 
df_App_allMarkets$Rating <- as.numeric(df_App_allMarkets$Rating)
df_App_allMarkets$Date <- as.Date(df_App_allMarkets$Date)

df_App_allMarkets$Market <- revalue(df_App_allMarkets$Market, 
  c("se" = "Sweden", 
    "us" = "USA", 
    "fi" = "Finland",
    "fr" = "France",
    "de" = "Germany",
    "hu" = "Hungary",
    "it" = "Italy",
    "nl" = "Netherlands",
    "no" = "Norway",
    "br" = "Brazil",
    "ch" = "Switzerland",
    "gb" = "Great Britain",
    "ca" = "Canada",
    "dk" = "Denmark"))

#Save as Excel for back up of all collected reviews
write_xlsx(df_App_allMarkets, path = ("AppStoreReviews.xlsx"), col_names = TRUE)

# Count values in each specific column
table(df_App_allMarkets$App_Version)
table(df_App_allMarkets$Market) #7000 posts
table(df_App_allMarkets$Rating)
qplot(df_App_allMarkets$Date)
qplot(df_App_allMarkets$Date)

View(df_App_allMarkets)

# 1. Plot rating distrubution for each market
ggplot(df_App_allMarkets, aes(x=as.factor(Rating), fill=as.factor(Rating))) +
  geom_bar(col="white")+
    theme_bw() +
    labs(title="App rating distribution per market", x="Ratings 1-5", y="No of Ratings")+
    theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0)) +
scale_fill_manual("Ratings", values = c("1" = "#DA393B", "2" = "#EE6D45", "3" = "#F7E458", "4" = "#68E194", "5" = "#5C8F7F"))+
facet_wrap(~Market, scales = 'free_x')
  
  #  scale_x_discrete(limits=c("1","2","3","4","5"))

# 2. Simplified distrubution for each market

# Create a new df based on values from df_App_allMarkets$Date
df_Ratings_Simplified <- data.frame("Date" = df_App_allMarkets$Date, "Rating" = df_App_allMarkets$Rating, "AppVersion" = df_App_allMarkets$App_Version, "Market" = df_App_allMarkets$Market)

#Convert Rating to vector format and replace no of rating stars to text
df_Ratings_Simplified$Rating <- as.character(df_Ratings_Simplified$Rating)

# Remove all ratings with 3-stars from df
df_Ratings_Simplified <- df_Ratings_Simplified[!df_Ratings_Simplified$Rating == "3", ]
qplot(df_Ratings_Simplified$Rating)

#Replace 1-2 star ratings with text Negative, and 4-5 stars with text Positive
df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '1'] <- 'Negative'
df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '2'] <- 'Negative'
df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '4'] <- 'Positive'
df_Ratings_Simplified$Rating[df_Ratings_Simplified$Rating == '5'] <- 'Positive'

View(df_Ratings_Simplified)
  
#2. Plot user feeling for each market
ggplot(df_Ratings_Simplified, aes(Rating, group = Market)) + 
geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), size = 4, stat= "count", vjust = -0.4) +
theme_bw() +
  theme(legend.position="none")+
  scale_fill_manual("Ratings", values = c("1" = "#ED5540", "2" = "#68E194"))+
  labs(y = "Rating", fill="Rating") +
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels=scales::percent, limits = c(0, 1)) +
  ylab("relative frequencies") +
  xlab("Procent") +  labs(title="User feeling per market", x="Reviews", y="Amount")+
  labs(caption = "(Negative = 1-2 stars, Positive = 4-5 stars)")+
  facet_wrap(~Market, scales = 'free_x')

# Plot feelings by weekday 
df_Ratings_Feeling_Week <- df_Ratings_Simplified
df_Ratings_Feeling_Week$Date <- format(as.Date(df_Ratings_Feeling_Week$Date), '%A')

ggplot(df_Ratings_Feeling_Week, aes(x = as.factor(Date), fill = Rating, label = Rating)) +
  geom_bar(stat = "count")+
  theme_bw() +
  scale_fill_manual("Ratings", values = c("Positive" = "#68E194", "Negative" = "#ED5540"))+
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("relative frequencies") +
  xlab("Procent") +  labs(title="User feeling per weekday", x="Weekday", y="Ratings")+
  labs(caption = "(Negative = 1-2 stars, Positive = 4-5 stars)")+
     scale_x_discrete(limits=c("Måndag","Tisdag","Onsdag","Torsdag","Fredag","Lördag","Söndag"))
#facet_wrap(~Market, scales = 'free_x')

#3. Plot user feeling trend line 
ggplot(df_Ratings_Simplified,aes(x=Date,color=Rating)) + 
  geom_line(stat='count', size=1)+
  theme_bw() +

  
  
#df_Ratings_Time <- aggregate(. ~Date, data=df_Ratings_Time, sum, na.rm=TRUE)
#df_Ratings_Time <- rowsum(df_Ratings_Time[-1], format(df_Ratings_Time$Date, "%b-%y") )


#http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html


#ggplot(data=filter(df_App_allMarkets, Rating !=3), aes(Rating)) + 
 # facet_wrap(~Market, scales = 'free_x')+
  #geom_histogram(breaks=seq(1, 5, by=2), col="white", size=1, aes(fill = ..count..))+ #, fill="#29E58E"
  #scale_fill_gradient2(low = "red", high = "blue", mid = "blue", midpoint = 100)+
  #scale_fill_gradient("Count", low = "red", high = "green")+
  # theme_bw() +
  #labs(title="Negative vs. positive ratings per market", x="Reviews", y="Amount")+
  #labs(caption = "(Negative = 1-2 stars, Positive = 4-5 stars)")+
  #scale_x_discrete(limits=c("","Negative","","Positive",""))
  
#coord_flip()
  
# 3. Average ratings per App version
# Creates a df with mean values for each app version
df_MeanRatingsVersion <- aggregate(df_App_allMarkets$Rating ~ df_App_allMarkets$App_Version, df_App_allMarkets, mean)

# Rename df columns, long vs. short version
#names(df_MeanRatingsVersion)[names(df_MeanRatingsVersion) == 'df_App_allMarkets$App_Version'] <- 'Version'
names(df_MeanRatingsVersion)[1]<- "Version"
names(df_MeanRatingsVersion)[2]<- "Rating"

# Sort by ratings acsending
df_MeanRatingsVersion$Version <- factor(df_MeanRatingsVersion$Version, levels = df_MeanRatingsVersion$Version[order(-df_MeanRatingsVersion$Rating)])

# Strip specific rows
df_MeanRatingsVersion <- df_MeanRatingsVersion[-c(10, 11, 12, 16,22,38), ]
df_MeanRatingsVersion$Rating <- round(df_MeanRatingsVersion$Rating, digits = 2)

#df_MeanRatingsVersion <- df_MeanRatingsVersion[order(df_MeanRatingsVersion$Version),] 

# Plot average ratings for each app version, remove row 16 (0.0 version)
ggplot(df_MeanRatingsVersion, aes(x = Version, y = Rating, label=Rating)) +
  geom_bar(fill = "#29E58E", stat = "identity")+
  geom_text(position = 'identity', stat = 'identity', size = 5, vjust = -0.4)+
  theme_bw() +
  labs(title="Average ratings for each App Version (order by rating)", size=60) +
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0)) +
    labs(x="App Version", y="Avg. Rating")

# Calculate average ratings for each day and change column names
df_MeanRatingsDays <- aggregate(df_App_allMarkets$Rating ~ df_App_allMarkets$Date, df_App_allMarkets, mean)
names(df_MeanRatingsDays)[1]<- "Date"
names(df_MeanRatingsDays)[2]<- "Rating"

# Convert dates to day of  month
df_MeanRatingsDays$Date <- unclass(as.POSIXlt(df_MeanRatingsDays$Date))$mday

# Split Day of month and avg. rating to to separate columns and change names
df_MeanRatingsDays <- aggregate(df_MeanRatingsDays$Rating ~ df_MeanRatingsDays$Date, df_MeanRatingsDays, mean)
names(df_MeanRatingsDays)[1]<- "Day"
names(df_MeanRatingsDays)[2]<- "Rating"

# Round Ratings to 1 digit
df_MeanRatingsDays$Rating <- round(df_MeanRatingsDays$Rating, digits = 1)

# Sort by Rating
df_MeanRatingsDays <- df_MeanRatingsDays[order(df_MeanRatingsDays$Rating),] 

# Plot mean ratings for each day of month
ggplot(df_MeanRatingsDays, aes(x = Day, y = Rating, label = Rating)) +
  geom_bar(fill = "#29E58E", stat = "identity")+
  theme_bw() +
  geom_text(position = 'identity', stat = 'identity', size = 4, vjust = -0.4)+
  labs(title="Average ratings per day of month", size=60) +
  theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=26, hjust=0), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Day of Month", y="Avg. Rating")+
  scale_x_discrete(limits=df_MeanRatingsDays$Day)+
  scale_y_continuous(limits = c(0,5))
  #coord_flip()

# Reorder columns (days) in our df by column index and plot the df (n.b run code once only)
#df_MeanRatingsWeekdays <- df_MeanRatingsWeekdays[c(3,6,4,7,1,2,5),]

# Textanalysis

# 5. Count lenght of reviews and create a sorted df
df_App_allMarkets$Review <- as.character(df_App_allMarkets$Review)
df_App_allMarkets$ReviewLength <- nchar(df_App_allMarkets$Review)

View(df_App_allMarkets)

# Aggregate all markets with tillhörande 
df_MeanLengthMarket <- aggregate(df_App_allMarkets$ReviewLength ~ df_App_allMarkets$Market, df_App_allMarkets, mean)
names(df_MeanLengthMarket)[1]<- "Market"
names(df_MeanLengthMarket)[2]<- "AvgReviewLength"

df2_App_allMarkets <- merge(df_App_allMarkets,df_MeanLengthMarket, by  = "Market") 
df2_MeanLengthMarket$AvgReviewLength <- round(df2_MeanLengthMarket$AvgReviewLength, digits = 2)

# Round numbers before visualizing
df2_App_allMarkets$AvgReviewLength <- round(df2_App_allMarkets$AvgReviewLength, digits = 2)

ggplot(data=df2_App_allMarkets, aes(x=ReviewLength)) + 
  geom_density(aes(y = ..count..), color="#1F3161", fill = "#68E193", alpha=0.6) +
  geom_vline(aes(xintercept = df2_App_allMarkets$AvgReviewLength), linetype = "dashed", size = 0.5)+
  facet_wrap(~Market, scales = 'free')+
  geom_text(data=df2_App_allMarkets, mapping=aes(x=AvgReviewLength, y=2, label=AvgReviewLength), check_overlap = TRUE, size=5, angle=0, vjust=1, hjust=-0.5)+
    ylim(0,5)+
    xlim(5,600)+
  theme_minimal()+
    labs(title="Review Character Length", subtitle = "The average length per review for each market", x="Review Length", y="")+
    theme(plot.title = element_text(family = "Circular Std", color="black", face="bold", size=22, hjust=0)) +
    theme(axis.title = element_text(family = "Circular Std", color="black", face="bold", size=12)) +
    theme(plot.subtitle = element_text(family = "Helvetica", color="black", face="plain", size=14))+
    theme(strip.text = element_text(face="bold", size=12))  +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
    
olle# To avoid overploting use check_overlap=TRUE
#scale_y_continuous(breaks=seq(1, 5, by=1))+
###
ggplot(df_App_allMarkets, aes(x = reorder(Author_Name, -nchar(Review)), y = nchar(Review))) +
  geom_bar(fill = "#29E58E", stat = "identity")+
  theme_bw() +
  labs(title="Average ratings for each App Version", size=60) +
  labs(x="App Version", y="Avg. Rating")+
  
#  scale_x_discrete(limits= df_MeanRatingsWeekdays$Weekday)+


###############################

# 3. Prepare for translation of market reviews with Google Transakte
# How to install Google Translate - http://code.markedmondson.me/googleLanguageR/

# Include Gooogle Cloud Service Identity
gl_auth("Marketing/iTunes/API Translate-83639a1f9217.json")

# Convert Reviews to Character Vector
text <- as.character(df_VOC_canada$Review)

# Create a new df with translated reviews, (N.B. automatic language detection)
df_Translation <- gl_translate(text, target = "en")

# Add english translated reviews to original df
df_VOC_canada$Review_translated <- df_Translation$translatedText

View(df_VOC_canada)

##### 4. Påbärja sentiment analysis

# Set variables
App_Version <- as.vector(df_VOC$App_Version)
Reviews <- as.character(df_VOC$Review_translated )
Market <- as.character(df_VOC$Market)

# Construct new df
df_VOC_Sentiment <- data.frame(App_Version, Reviews, Market)%>%
  mutate(linenumber = row_number(), text = Text) %>%
  ungroup()

# Split reviews to text words "Tokenization"
tidy_tb <- df_VOC_Sentiment %>%
  unnest_tokens(word, text)

# Remove stop words
data(stop_words)

tidy_tb <- word_tb %>%
  anti_join(stop_words)

# Count words
tidy_tb %>%
  count(word, sort = TRUE) 

View(tidy_tb)

tidy_tb %>%
count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Add sentiment scores to words
left_join(get_sentiments("bing"), by = "word") %>%
get_sentiments("bing") 
    
# count number of negative and positive words
count(sentiment) %>%
spread(key = sentiment, value = n) %>%
ungroup 
  
# Sentiment analysis https://www.tidytextmining.com/tidytext.html
# https://afit-r.github.io/sentiment_analysis




### COUNTING REVIEW LENGTH ###

#Convert to charachter, calculate Review lenght 
df_rating_1$ReviewLength <- nchar(df_rating_1$Review)
df_rating_2$ReviewLength <- nchar(df_rating_2$Review)
df_rating_3$ReviewLength <- nchar(df_rating_3$Review)
df_rating_4$ReviewLength <- nchar(df_rating_4$Review)
df_rating_5$ReviewLength <- nchar(df_rating_5$Review)

#Calculate mean-values for reviews
MeanOneStar <- mean(df_rating_1$ReviewLength)
MeanTwoStar <- mean(df_rating_2$ReviewLength)
MeanThreeStar <- mean(df_rating_3$ReviewLength)
MeanFourStar <- mean(df_rating_4$ReviewLength)
MeanFiveStar <- mean(df_rating_5$ReviewLength)

#Stacked bars - https://www.statmethods.net/graphs/bar.html


df_App_allMarkets$HighRatings <- subset(df_App_allMarkets, Rating >= 4)
df_App_allMarkets$LowRatings <-subset(df_App_allMarkets, Rating <= 2)

ggplot(data=df_App_allMarkets, aes(x=Date, y=Rating, fill=Rating)) +
  geom_bar(stat="identity")

head(df_App_allMarkets)

dfAppHighRatings <- subset(df_App_allMarkets, Rating >= 4)
dfAppLowRatings <-subset(df_App_allMarkets, Rating <= 2)

dfAppHighRatings <- data.frame(dfAppHighRatings$Date, dfAppHighRatings$Rating, dfAppHighRatings$Market)
dfAppLowRatings <- data.frame(dfAppLowRatings$Date, dfAppLowRatings$Rating, dfAppLowRatings$Market)

# Rename columns before combining dfs
names(dfAppHighRatings)[names(dfAppHighRatings) == 'dfAppHighRatings.Date'] <- 'Date'
names(dfAppHighRatings)[names(dfAppHighRatings) == 'dfAppHighRatings.Rating'] <- 'Rating'
names(dfAppHighRatings)[names(dfAppHighRatings) == 'dfAppHighRatings.Market'] <- 'Market'

names(dfAppLowRatings)[names(dfAppLowRatings) == 'dfAppLowRatings.Date'] <- 'Date'
names(dfAppLowRatings)[names(dfAppLowRatings) == 'dfAppLowRatings.Rating'] <- 'Rating'
names(dfAppLowRatings)[names(dfAppLowRatings) == 'dfAppLowRatings.Market'] <- 'Market'

View(dfAppHighRatings)

# Byta namn på kolumner, tog bort denna till förmån för en med mindre kod
names(df_MeanRatingsVersion)[names(df_MeanRatingsVersion) == 'df_App_allMarkets$App_Version'] <- 'Version'
names(df_MeanRatingsVersion)[names(df_MeanRatingsVersion) == 'df_App_allMarkets$Rating'] <- 'Rating'
