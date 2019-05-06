library(googleAnalyticsR)
library(ggplot2)
library(gganimate)

ga_auth()

my_id <- XXXXXXX

## Query your data and save if in a data frame
df <- google_analytics(my_id, 
   date_range = c("2018-01-01", "2018-12-31"),
   metrics = c("users"),
   dimensions = c(”dayOfWeek”,"deviceCategory", ”hour”, "date")
          anti_sample = TRUE, 
          max = -1)  

# convert date to month
df$date <- format(as.Date(df$date), "%m")
df$date <- as.integer(df$date)

anime <- ggplot(data=df, aes(x=dayOfWeek, y=users, fill=deviceCategory))+
  geom_bar(stat="identity", fill='#6a3d9a') +
  theme_minimal() +
  labs(title = "Months - {frame_time}", x="WEEKDAY", y="USERS")+
  facet_grid(. ~deviceCategory)+
theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  transition_time(date)+
  exit_shrink()

# run animation
  animate(anime, fps=25)
  
# save animation as mp4 or .gif
  anim_save("animation.mp4", anime, width = 1000, height = 650)
  anim_save("output.gif", anime)
