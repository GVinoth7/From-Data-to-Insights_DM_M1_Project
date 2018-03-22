#################################################################
# Analysing the weather dataframe of the given period           #
#################################################################

library(lubridate)
library(dplyr)
library(Hmisc)
library(reshape2)
library(GGally)
library(scales)
library(repr)


# converting the date into data frame of day, month
# Get dates and zip_code variables in right format
weather$date <- mdy(weather$date)
trip$start_date <- mdy_hm(trip$start_date)
trip$end_date <- mdy_hm(trip$end_date)

trip$date <- trip$start_date
trip$date <- as.Date(trip$date)

trip$zip_code <- as.numeric(levels(trip$zip_code))[trip$zip_code]
#NAs introduced and some prior coding errors of zip_code maintained 

#Merge weather df variables into trip df
#Based on the start_station ID, merge the "city" variable into trip df
trip$date <- as.Date(trip$start_date)
trip$id2 <- trip$id
trip$id <- trip$start_station_id 
trip <- left_join(trip, station, by = c ("id"))

#Now get a city variable into weather df
zip_code <- unique(weather$zip_code)
city <- c ("San Francisco", "Redwood City", "Palo Alto", "Mountain View", "San Jose")
index <- cbind(city, zip_code)   
weather <- merge(weather, index, by = "zip_code")

#Now merge weather and trip data based on city 
trip <- left_join(trip,weather, by = c("date", "city"))
rm("weather", "station", "index", "city", "zip_code")

# Remove some influential observations in alternate version of df
# First take out some extreme values; Z-score time 

trip$durZ <- scale(trip$duration, center = TRUE, scale = TRUE)
length(which(trip$durZ > 3 | trip$durZ < -3))
#There are 739 elements which are more than 3 sd's from the mean 

#Take out elements more than 3 SDs from the mean 
daytrip <- trip[trip$durZ <= 3,] 
daytrip <- daytrip[daytrip$durZ >= -3,]

#Make a weekend vs. weekday variable
trip$weekend <- as.factor(wday(trip$date))
trip$weekend <- (trip$weekend == 1 | trip$weekend == 7)
trip$weekend <- factor(trip$weekend, labels = c("Weekday", "Weekend"))

#Get weather events into same format (rain coded as both "rain" and "Rain")
trip$events <- tolower(trip$events)
trip$events <- factor(trip$events)

df1 <- melt(trip[,c(1,22,25,28,31,37)], id.vars = "id") 


###################################################################
# Different weather parameters with respect to the count of trips #
###################################################################

ggplot(df1,aes(x = value)) + 
  facet_wrap(~variable,  scales = "free") + 
  geom_histogram(color = "blue", alpha = 0.7)+
  ylab("Total number of bicycle trips (08/2013 - 08/2015)") +
  xlab("") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###################################################################
# Data analysis of the system  #
###################################################################


df3 <- melt(trip[,c(12,22,25,28,31,37)], id.vars = "date")

ggplot(df3, aes(x = date, y = value)) +
  geom_point(color = "red", alpha = 0.03, na.rm = TRUE) +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  theme_light()

###################################################################
# Cycle trips based on the type of customers                      #
###################################################################

options(repr.plot.width=6, repr.plot.height=3)

ggplot(trip, aes(subscription_type)) + 
  geom_bar(stat="count", fill = "blue") +
  scale_y_continuous(labels = comma) +
  ylab("Number of bicycle trips") +
  theme_light()


###################################################################
# Temperature compared with growth of trips                       #
###################################################################

options(repr.plot.width=8, repr.plot.height=4)

ggplot(trip, aes(mean_temperature_f)) + 
  geom_bar(stat = "count", aes(fill = weekend), position = "dodge") +
  facet_wrap(~subscription_type, scales = "free_y") +
  ylab("Total number of bicycle trips") +
  xlab("Mean temperature for the day") + 
  ggtitle("Temperature vs. Number of Bicycle Trips") +
  theme_light()

###################################################################
# Humidity compared with growth of trips                          #
###################################################################


ggplot(trip, aes(mean_humidity)) + 
  geom_bar(stat = "count", aes(fill = weekend), position = "dodge") +
  facet_wrap(~subscription_type, scales = "free_y") +
  ylab("Total number of bicycle trips") +
  xlab("Mean humidity for the day") + 
  ggtitle("Humidity vs. Number of Bicycle Trips") +
  theme_light()

###################################################################
# Wind speed  compared with growth of trips                       #
###################################################################

options(repr.plot.width=8, repr.plot.height=4)

ggplot(trip, aes(mean_wind_speed_mph)) + 
  geom_bar(stat = "count", aes(fill = weekend), position = "dodge") +
  facet_wrap(~subscription_type, scales = "free_y") +
  ylab("Total number of bicycle trips") +
  xlab("Mean wind speed (in mph) for the day") + 
  ggtitle("Wind Speed vs. Number of Bicycle Trips") +
  theme_light()


###################################################################
# Bike trips compared with different weather pattern              #
###################################################################

library(plyr)

populationcount <- aggregate(city ~ events * date, data = trip, count) %>%
  summarise(events = count(events))
obscount <- trip %>% summarise(events = count(events))
a <- obscount[,1]
b <- populationcount[,1]
ratio <- a[,2]/b[,2]
c <- c("No Fog or Rain", "Fog", "Fog and Rain", "Rain", "Thunderstorm")
ratio <- as.data.frame(cbind(ratio, c))
ratio2 <- rbind(ratio, c)


###################################################################
# Riders Vs Weather pattern                                       #
###################################################################

options(repr.plot.width=7, repr.plot.height=4)
ggplot(ratio, aes(c,ratio)) +
  geom_bar(stat = "identity", fill = "red") +
  xlab("weather pattern") +
  ylab("Average number of bicycle trips") +
  ggtitle("How sensitive are riders to different weather patterns?") +
  theme_light()

avg <- aggregate(duration ~ mean_temperature_f, data = daytrip, mean)      


###################################################################
# Average of duration of ride with temperature                    #
###################################################################

ggplot(data = avg, aes(mean_temperature_f, duration)) +
  geom_point(size = 4, color = "red", alpha = 0.8) +
  geom_smooth(se = FALSE) +
  ggtitle("Average trip duration for average temperatures") +
  ylab("Average Duration (in Seconds) of bicycle trip") +
  xlab("Average Temperature (In Fahrenheit)") +
  coord_cartesian(ylim = c(400, 1500))