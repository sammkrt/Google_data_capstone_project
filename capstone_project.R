#gerekli paketleri yükleyelim

install.packages(tidyverse)
install.packages("skimr")
install.packages("here")
install.packages("janitor")
install.packages("dplyr")

library(tidyverse)
library(skimr)
library(here)
library(janitor)
library(dplyr)
library(lubridate)  # for time and date
library(dplyr) # for connect rows
library(ggplot2) #for the vizs

#set up work directories

setwd("C:\\Users\\Lenovo\\Masaüstü\\Google Data Certificate\\Capstone_project\\Case_Study_1_Bike_sharing\\Last_One_year")

#Loading datasets

apr_2021 <- read.csv("202104-divvy-tripdata.csv")
may_2021 <- read.csv("202105-divvy-tripdata.csv")
june_2021 <- read.csv("202106-divvy-tripdata.csv")
jul_2021 <- read.csv("202107-divvy-tripdata.csv")
agu_2021 <- read.csv("202108-divvy-tripdata.csv")
sep_2021 <- read.csv("202109-divvy-tripdata.csv")
oct_2021 <- read.csv("202110-divvy-tripdata.csv")
nov_2021 <- read.csv("202111-divvy-tripdata.csv")
dec_2021 <- read.csv("202112-divvy-tripdata.csv")
jan_2022 <- read.csv("202201-divvy-tripdata.csv")
feb_2022 <- read.csv("202202-divvy-tripdata.csv")
mar_2022 <- read.csv("202203-divvy-tripdata.csv")
apr_2022 <- read.csv("202204-divvy-tripdata.csv")

# a quick glimpse to a data

glimpse(apr_2021)

# we can check the column names of data

colnames(apr_2021)
colnames(may_2021)
colnames(june_2021)
colnames(jul_2021)
colnames(agu_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)


#we need to convert  some column as character

apr_2021 <- mutate(apr_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

may_2021 <- mutate(may_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

june_2021 <- mutate(june_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

jul_2021 <- mutate(jul_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

agu_2021 <- mutate(agu_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

sep_2021 <- mutate(sep_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

oct_2021 <- mutate(oct_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

nov_2021 <- mutate(nov_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

dec_2021 <- mutate(dec_2021 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

jan_2022 <- mutate(jan_2022 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

feb_2022 <- mutate(feb_2022 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

mar_2022 <- mutate(mar_2022 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))

apr_2022 <- mutate(apr_2022 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))


#Combine all data into one 

all_year <- bind_rows(apr_2021,may_2021,june_2021,jul_2021,agu_2021,sep_2021,oct_2021,nov_2021,dec_2021,jan_2022,feb_2022,mar_2022,apr_2022)



#we dont need some column for our analysis therefore we can drop sum columns

all_year <- subset(all_year, select = -c(start_lng,end_lng,start_lat,end_lat))


glimpse(all_year) # glimpse last data

colnames(all_year) # column names of data

nrow(all_year) #number of rows

dim(all_year) # dimension of the rows


#converting type of started_at and ended_at as POSIXct

all_year <- mutate(all_year, started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S" ,tz =""), ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S" ,tz =""))

#delete NA values

all_year <- na.omit(all_year)

#to avoid negative value apply filter function

all_year <- all_year %>% 
  filter(started_at < ended_at)

#creating new column as ride_length

all_year$ride_length <- all_year$ended_at - all_year$started_at


colnames(all_year) #checking the new column


#format the column to h,m,s

all_year$ride_length <- hms::hms(seconds_to_period(all_year$ride_length))


#converting ride_length column to number

all_year <- mutate(all_year , ride_length = as.numeric(ride_length)) # check this code again


#creating a new column as day_of_week

all_year$day_of_week <- wday(all_year$started_at,label = TRUE)


#save file

all_year %>% 
  write.csv("capstone_project_cleaned.csv")



###### Analyze ######


summary(all_year$ride_length) # Statistical summary due to ride length

#comparing ride_length by member_casual

aggregate(all_year$ride_length~all_year$member_casual, FUN = mean)

aggregate(all_year$ride_length~all_year$member_casual, FUN = median)
aggregate(all_year$ride_length~all_year$member_casual, FUN = min)
aggregate(all_year$ride_length~all_year$member_casual, FUN = max)

#Avg ride_lentgth by each day_of_week

aggregate(all_year$ride_length ~ all_year$member_casual + all_year$day_of_week,FUN = mean)

#let's visulate this data
#number of rides by ride type

all_year %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual , weekday) %>% 
  ggplot(mapping = aes(x=weekday, y= number_of_rides,fill=member_casual)) + 
  geom_col(position = "dodge")

#Average of duration

all_year %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual , weekday) %>% 
  ggplot(mapping = aes(x=weekday, y= average_duration,fill=member_casual)) + 
  geom_col(position = "dodge")

#Distributed within the weekday

all_year %>% 
  group_by(day_of_week) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(all_year)) * 100,
            'member' = (sum(member_casual == "member")/ length(ride_id))*100,
            'casual' = (sum(member_casual == "casual")/ length(ride_id))*100,
            'Member x Casual Perc Diffirent' = member - casual)

ggplot(all_year , aes(day_of_week , fill = member_casual)) +
  geom_bar() +
  labs ( x="Weekday" , title = "Distribution by weekday")




#Distributed by ride type
all_year %>% 
  group_by(rideable_type) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(all_year)) * 100,
            'member' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'Member x Casual Perc Diferrent' = member - casual)


ggplot(all_year , aes(rideable_type , fill = member_casual)) +
  geom_bar() +
  labs ( x="Rideable Type" , title = "Distribution by type of bikes")
