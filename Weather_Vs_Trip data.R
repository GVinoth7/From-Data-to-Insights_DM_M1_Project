#################################################################
# Regression model of the dataframe                             #
#################################################################

library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(Hmisc)

#Prepare data for analysis 
#Get dates in right format
trip$start_date <- mdy_hm(trip$start_date)
trip$end_date <- mdy_hm(trip$end_date)
trip$date <- trip$start_date
trip$date <- as.Date(trip$date) 

#Based on the start_station ID, merge the "city" variable into trip df
trip$date <- as.Date(trip$start_date)
trip$id2 <- trip$id
trip$id <- trip$start_station_id 
trip <- left_join(trip, station, by = c ("id"))

#################################################################
# Analysing the weather dataframe of the given period           #
#################################################################

datefreq <- count(trip, date)

ggplot(data = datefreq, aes(date, n)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Trips Each Day") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Date")


#################################################################
# Analysing the weather dataframe of the given period           #
#################################################################

dailyfreq <- as.data.frame(table(wday(trip$date, label = TRUE)))

ggplot(data = dailyfreq, aes(x = Var1, Freq)) +
  geom_bar(stat="identity",  fill = "#FF6666") + #need to include stat = identity or will 
  #try to make a count of the count 
  ggtitle("Total Number of Trips Per Day") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Day of the Week")



#Makes variable with True if date == sunday(1) or saturday (7)

datefreq <- mutate(datefreq, weekend = (wday(datefreq$date) == 1 |
                                          wday(datefreq$date) == 7))

# Marking the weekday and weekend
datefreq$weekend <- factor(datefreq$weekend, labels = c("Weekday", "Weekend"))


# Date Vs the Number of bicycle trips

ggplot(data = datefreq, aes(date, n)) +
  geom_point(aes(color = weekend), size = 3, alpha = 0.65) +
  ggtitle("Total Number of Trips Per Day") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Date")



ggplot(data = datefreq, aes(date, n)) +
  geom_point(aes(color = weekend),size = 3, alpha = 0.65) +
  facet_grid(. ~ weekend) + 
  geom_smooth(se = FALSE) +
  ylab("Total Number of Bicycle Trips") +
  xlab("Date")


# separting the date and time 
start_date_hour_split <- ymd_hms(trip$start_date) 
time_trip <- hour(start_date_hour_split) + minute(start_date_hour_split)/60
trip$daytime <- time_trip

# removing the temporary variables
rm( start_date_hour_split, time_trip) 

# Finding the peak time of the trip in a day
ggplot(trip, aes(daytime)) +
  geom_histogram(binwidth = 0.25, fill = 'blue') + 
  geom_vline(xintercept = 8.75, color = 'green')+
  geom_vline(xintercept = 17, color = 'green', alpha = 0.4) +
  annotate("text", x = 8.75, y = 27000, label = "9:00 AM", color = "green",
           size = 7) +
  annotate("text", x = 17, y = 27000, label = "5:00 PM", color = "green", 
           size = 7) +
  xlab("Time of day on 24 hour clock") +
  ylab("Total number of bicycle trips")



trip$quarter <- quarter(trip$date)

ggplot(trip, aes(daytime)) +
  geom_histogram(binwidth = 0.25) + #Every fifteen minutes = binwidth 
  geom_vline(xintercept = 9, color = 'orange')+
  geom_vline(xintercept = 17, color = 'red', alpha = 0.7) +
  xlab("Time of day on 24 hour clock") +
  ylab("Total number of bicycle trips") +
  facet_wrap(~quarter)


# Trip across various weekdays and weekends 

trip <- mutate(trip, weekend = (wday(trip$date) == 1 |
                                  wday(trip$date) == 7))
trip$weekend <- factor(trip$weekend, labels = c("Weekday", "Weekend"))



#################################################################
# # Analysis of Trip access by all the cities                   #
#################################################################


ggplot(data = trip, aes(date)) +
  geom_bar(aes(color = weekend), stat = "count",
           position = "stack") +
  ggtitle("Trips by City Across Time") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Trend Across Time") +
  facet_grid(~city) +
  theme(axis.text.x = element_blank())


#################################################################
# # Analysis of Weekday and Weekend usage in the city           #
#################################################################

ggplot(data = trip, aes(date)) +
  geom_bar(aes(color = subscription_type), stat = "count", 
           position = "stack") +
  ggtitle("Customer Vs. Subscriber on Weekends and Weekdays") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Trend Across Time") +
  facet_grid(~weekend) +
  theme(axis.text.x = element_blank())

# Analysis of Weekday and Weekend usage in the city

ggplot(data = trip, aes(date)) +
  geom_bar(aes(color = subscription_type), stat = "count", position = "stack") +
  ggtitle("Subscribers Vs. Customers - Trips Per Day by City ") +
  ylab("Total Number of Bicycle Trips") +
  xlab("Trend Across Time") +
  facet_wrap(~city, scale = "free_y") +
  theme(axis.text.x = element_blank())