# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/

library(googleAnalyticsR)
library(ggplot2)


ga_auth()
my_id <- xxxxxxxx

df <- google_analytics(my_id, 
                       date_range = c("2018-01-01", "2018-12-31"),
                       metrics = c("goal6Completions", "users"),
                       dimensions = c("dayOfWeek","month","deviceCategory"),
                       anti_sample = TRUE, 
                       max = -1)  

decomp_ga <- ga_model_example("decomp_ga.gamr")

# Visualize conversions per month
ggplot(data = df) +
  aes(x = month, weight = goal6Completions) +
  geom_bar(fill = '#6a3d9a') +
  labs(title = 'Beviljade konverteringar per månad 2018',
       x = 'Månad',
       y = 'Beviljade') +
  theme_minimal()

# Visualize conversions per month and per device 
ggplot(data=df, aes(x=month, y=goal6Completions, fill=deviceCategory)) +
  geom_bar(stat="identity", fill='#6a3d9a') +
  theme_minimal() +
  labs(title = "Antal beviljade konverteringar per enhet under 2018", x="Enhet", y="Beviljade") +
  facet_grid(. ~deviceCategory)

# Visualize conversions per month and per device, weekday and hour of the day
ggplot(data=df, aes(x=dayOfWeek, y=month, fill=goal6Completions)) + 
  geom_tile(colour = "black")+
  geom_text(aes(label = goal6Completions),size=2.1, face="plain", lineheight=.8) +
  facet_grid(~deviceCategory) +
  
  # Re-order and rename X-scale
  scale_x_discrete(limits=c(
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "0"), 
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
    "23" = "23:00"))+
  
  # Style  
  scale_fill_gradient(low = '#FFFFA6', high = 'red', name="# of Users", limits= c(0,250)) +
  #scale_fill_gradient(low = '#FFFFA6', high = 'red', name="# Buy Now Button", limits= c(0,40000)) +
  labs(title = "Konverteringar webb 1 JAN - 31 DEC 2019", x="Weekday", y="Hour of day") +
  #labs(title = "GA Goal Conversions: Buy Now Button, 2017", x="Weekday", y="Hour of day") +
  
  # Change the appearance and the orientation angle
  theme(
    axis.text.x = element_text(face="plain", color="black", size=10, angle=90),
    axis.text.y = element_text(face="plain", color="black", size=10, angle=0), 
    axis.line = element_line(colour = "black", size = 0, linetype = "solid"),
    strip.text.x = element_text(size=12, angle=0),
    strip.text.y = element_text(size=12, face="bold"),
    strip.background = element_rect(color="#F6F3F0", fill="#F6F3F0"),
    rect = element_rect(fill = "#F6F3F0"), # bg of the panel
    panel.background = element_blank())
