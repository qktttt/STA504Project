library(tidyr)
library(ggthemes)
library(lubridate)
library(dplyr)
library(ggmap)
library(ggplot2)
library(plotly)

# loading data
setwd("C:/Users/qik/Desktop/STA404 Project/")
Airbnb <- read.csv("AB_NYC_2019.csv")

# Cleaning and sorting
Airbnb2 <- Airbnb %>%
  mutate("Apt_Name" = name,
         "Host" = host_name,
         "Area" = neighbourhood_group,
         "Subarea" = neighbourhood,
         "Nights" = minimum_nights,
         "Reviews" = number_of_reviews,
         "Availability" = availability_365,
         "Review_date" = ymd(last_review)) %>%
  filter(Nights > 0,
         Availability > 0) %>%
  select(Apt_Name, Host, Area, Subarea, latitude, longitude,
         room_type, price, Nights, Reviews, Review_date, Availability) %>%
  arrange(desc(price)) %>%
  drop_na()

#### Overall mean price grouped by sub_area
grouped_by_data <- Airbnb2 %>% group_by(Area, Subarea) %>% summarise(meanPrice = mean(price),
                                                                     long=mean(longitude), lat = mean(latitude))

#### Sub area mean price grouped by, also with diffrent type of airbnbs
grouped_by_data_withType <- Airbnb2 %>% group_by(Area, Subarea, room_type) %>% summarise(meanPrice = mean(price),
                                      long=mean(longitude), lat = mean(latitude))

register_google("###################")
newYorkMap <- get_map("New York", zoom = 10, size = c(1000, 1000))

## save data
save(grouped_by_data, grouped_by_data_withType, Airbnb, Airbnb2, newYorkMap, file="SavedData.RData")
