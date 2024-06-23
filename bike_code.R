library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)
library(geosphere)



tp_2020 <- read.csv("C:\\Users\\Pasindu\\Desktop\\case study\\Trips_2020_Q1.CSV")
View(tp_2020)
summary(tp_2020)

#tp_2020$start_time <- asTime(tp_2020$started_at, format = "%H:%M") 

tp_2020$date <- as.Date(tp_2020$started_at, format = "%m/%d/%Y")
tp_2020$ed <- as.Date(tp_2020$ended_at, format = "%m/%d/%Y")#The default format is yyyy-mm-dd
tp_2020$month <- format(as.Date(tp_2020$date), "%m")
tp_2020$day <- format(as.Date(tp_2020$date), "%d")
tp_2020$year <- format(as.Date(tp_2020$date), "%Y")
tp_2020$day_of_week <- format(as.Date(tp_2020$date), "%A")

filtered_df <- tp1 %>%
  filter(date != ed)
dim(filtered_df)
View(filtered_df)

tp1 <- tp_2020[!duplicated(tp_2020[, c("ride_id", "started_at", "ended_at")]) & !duplicated(tp_2020[, c("ride_id", "started_at", "ended_at")], fromLast = TRUE), ]
dim(tp1)
View(tp1)
dim(tp_2020)
View(tp_2020)


tp1$start_time <- mdy_hm(tp1$started_at)
tp1$end_time <- mdy_hm(tp1$ended_at)
tp1$time1 <- format(tp1$start_time, format = "%H:%M:%S")
tp1$time2 <- format(tp1$end_time, format = "%H:%M:%S")

# Convert time columns to POSIXct objects (date is arbitrary, only time part is needed)
tp1$time1 <- as.POSIXct(tp1$time1, format = "%H:%M:%S", tz = "UTC")
tp1$time2 <- as.POSIXct(tp1$time2, format = "%H:%M:%S", tz = "UTC")

# Calculate duration in minutes as ride length
tp1$ride_length <- as.numeric(difftime(tp1$end_time, tp1$start_time, units = "mins"))

tp2 <- subset(tp1, ride_length >= 0 & ride_length <= 1400)
dim(tp2)
View(tp2)


#calculate ride distance
tp2$ride_distance <- distGeo(matrix(c(tp2$start_lng,tp2$start_lat),ncol=2),matrix(c(tp2$end_lng,tp2$end_lat),ncol=2))

calculate_distance <- function(start_lat, start_lng, end_lat, end_lng) {
  distHaversine(c(start_lng, start_lat), c(end_lng, end_lat))
}

# Apply the function to calculate distance for each row
tp2$ride_distance <- mapply(calculate_distance, tp2$start_lat, tp2$start_lng, tp2$end_lat, tp2$end_lng)
tp2$ride_distance_km <- tp2$ride_distance / 1000

#openxlsx::write.xlsx(tp2, file = "tp2_2020.xlsx")

# Verify the file creation
#list.files(pattern = "tp2_2020.xlsx")

tp3 <- tp2[!duplicated(tp2$ride_id),]
dim(tp3)

tp3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)

tp3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(month)

tp3 %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n() , .groups = 'drop')

tp3 %>%
  group_by(rideable_type) %>%
  summarise(amount = n(), .groups = "drop")

View(tp3)

####Plots

tp3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

tp3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

tp3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

tp3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")

tp3 %>%
  group_by(member_casual) %>%
  filter(ride_distance < 10000) %>% #Remove outliner
  ggplot(aes(x = ride_distance, fill = member_casual)) + 
  geom_histogram() #Disregard binwidth
