library(tidyverse)
library(rvest)
library(stringr)
library(textcat)
library(rebus)
library(lubridate)
library(selectr)
library(XML)
library(writexl)
library(stringi)

#Add Truspilot company information
  url = "https://se.trustpilot.com/review/www.spotify.com?languages=all"
  TotalReviews <- 714 #Add no of reviews on Trustpilot
  TotalReviewsPerPage <- 20
  first_page <- url
  last_page <- ceiling(TotalReviews/TotalReviewsPerPage)

#List of all pages including url
  list_of_pages <- str_c(url, '&page=', 1:last_page)

# Create an empty data frame
  dfAllReviews = NULL

#Loop through all reviews
  require(svMisc)
  for (pages in list_of_pages)
    {
    print(pages)
    url = pages
    web <- read_html(url)
  
#Scrape review text from each review
  review_html <- html_nodes(web, '.review-content__text')
  review <- html_text(review_html)
  review <- str_replace_all(review, '[\r\n]' , "")
  
#Remove whitespace
  review <- str_squish(review)
    
#Scrape review date and time
  date_html <- html_nodes(web, '.review-content-header__dates')
  date <- html_text(date_html)
  date <- str_replace_all(date, '[\r\n,T,Z,{\"publishedDate\":\",nrorn}"]' , "") #remove character
  date <- str_squish(date)
  
#Substring date and time
  date <- stri_sub(date,1,16)
  
#Add time format (This is ugly, I know..)
  date <- sub("(......)$", " \\1", date)
  date <- sub("(....)$", ":\\1", date)
  date <- sub("(..)$", ":\\1", date)

#Convet to corrext format
  date <- as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")

#Scrape username of the reviewer
  name_html <- html_nodes(web, '.consumer-information__name')
  name <- html_text(name_html)
  
  name <- str_replace_all(name, '[\n]' , "")
  name <- str_squish(name)

#Scrape no of other written Reviews from same user
  writtenReviews_html <- html_nodes(web,'.consumer-information__review-count')
  writtenReviews <- html_text(writtenReviews_html)
  
  writtenReviews <- str_replace_all(writtenReviews, '[\n]' , "")
  writtenReviews <- str_squish(writtenReviews)
  writtenReviews <- stri_sub(writtenReviews,1,2)
  writtenReviews <- as.numeric(writtenReviews) 

#Scrape rating for each review
  rating_html <- html_nodes(web,".review-content-header")
  rating <- as(rating_html, "character")
  rating <- substr(rating,78,79)
  rating <- as.numeric(rating) 
  
# Combine values and create df
    dfReview <- data.frame(Date = date, Name = name, Review = review, Rating = rating, NoReviews = writtenReviews)
      
# Combine all dataframes
  dfAllReviews <- rbind(dfReview, dfAllReviews)

# End loop
}

#Rename df
  dfTrutspilotReview <- dfAllReviews

#View DataFrame
  View(dfTrutspilotReview)

#Detect language
  dfTrutspilotReview$Market <- textcat(dfTrutspilotReview$Review)

#Add month and year
  dfTrutspilotReview$Month <- format(dfTrutspilotReview$Date, "%b, %y")
  
#Convert reviews to character and add lenght of characters
  dfTrutspilotReview$Review <- as.character(dfTrutspilotReview$Review)
  dfTrutspilotReview$ReviewLength <- nchar(dfTrutspilotReview$Review)
  
#Export to Excel
  write_xlsx(dfTrutspilotReview, path = ("Trustpilot.xlsx"), col_names = TRUE)
  
#Plot review lenght for each rating  
  ggplot(data=dfTrutspilotReview, aes(x=ReviewLength)) + 
    geom_density(aes(y = ..count..), color="#1F3161", fill = "#68E193", alpha=0.6) +
    facet_wrap(~Rating)+
    theme_minimal()+
    labs(title="Character length do varies between star ratings on Trustpilot", subtitle = "Each number represent star-ratings between 1-5", x="Review Length", y="")+
    theme(plot.title = element_text(color="black", face="bold", size="18", hjust=0)) +
    theme(strip.text = element_text(face="bold", size=10))  +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
