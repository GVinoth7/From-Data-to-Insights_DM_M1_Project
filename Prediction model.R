#################################################################
# Removing the outliers                                         #
#################################################################

# converting seconds to minutes

trip$duration = trip$duration/60
trip$duration

# removing the trips greater than 6 hours

trip_less_than_6hrs <- trip[trip$duration <= 360,]
nrow(trip_less_than_6hrs)

trip$start_date <- as_datetime(trip$start_date, format='%m/%d/%Y %H:%M')

trip$start_date <- as.Date(trip$start_date)

table(trip$start_date)

dataset_date_trip = data.frame(table(trip$start_date))
x <- c('date','trips')

# Data limiting to the specific columns
colnames(dataset_date_trip) <- x
dim(dataset_date_trip)

weather$date <- as_datetime(weather$date, format='%m/%d/%Y')
weather$date <- as.Date(weather$date)
dim(weather)

unique(weather$zip_code)
weather <- weather[weather$zip_code == 94107,]

weather[weather$events =="rain","events" ] = 'Rain'
weather[(weather$events) =="", 'events'] = 'Normal'
weather$events

weather[(weather$precipitation_inches)=='0.00', 'precipitation_inches'] = median(weather$precipitation_inches)

weather$precipitation_inches

dim(trip)
dim(weather)

trains = merge(x = dataset_date_trip, y= weather, by= dataset_date_trip$start_date)
View(head(trains))
dim(trains)

#################################################################
# Everyday trips                                                #
#################################################################


ggplot(data = dataset_date_trip, aes(x = dataset_date_trip$date, y = dataset_date_trip$trips)) +
  geom_point() +
  ylab("Count of trips") +
  xlab("Date") + 
  ggtitle("Daily trips of the user") +
  theme_light()



#################################################################
# Clustering of trips                                           #
#################################################################
cl <- kmeans(dataset_date_trip[2], 15)
print(cl)
plot(dataset_date_trip$trips, col = cl$cluster, xlab= 'Day', ylab='Count of Trips')



#################################################################
# Forecasting based on daily trips                              #
#################################################################
dataset_date_trip

dataset_date_trip$date = as.Date( dataset_date_trip$date,format="%Y-%m-%d")

library(forecast)
library(xts)
library(sos)

dataset_date_trip$date

x = xts(x=dataset_date_trip$trips, order.by= dataset_date_trip$date)
# To get the start date (305)
#     > as.POSIXlt(x = "2011-11-01", origin="2011-11-01")$yday
##    [1] 304
# Add one since that starts at "0"
x.ts = ts(x, freq=365, start=c(2013,241))
plot(forecast(ets(x.ts), 50), xlab='Date', ylab = 'Number of trips', main='Forecasting of trips')


fit.ex3 <- lm(dataset_date_trip$date ~ dataset_date_trip$trips, data=dataset_date_trip)

#################################################################
# Clustering Index                                              #
#################################################################


df <- scale(dataset_date_trip[-1]) 


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main = 'Clustering of trips with date')}

wssplot(df)

#################################################################
# Data clustering methods                                       #
#################################################################

library(NbClust)

set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")


set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)                         
fit.km$size

fit.km$centers

aggregate(dataset_date_trip[-1], by=list(cluster=fit.km$cluster), mean)

ct.km <- table(dataset_date_trip$date, fit.km$cluster)

ct.km


#################################################################
# Parameters focusing on the bike usage                         #
#################################################################

trains$date <- trains$date_x

# Maintenance of bikes
bikeusageduration = {}

sum(trip$duration[trip$bike_id == 288])
for (bikeid in trip$bike_id) {
  bikeusageduration[bikeid] <- sum(trip$duration[trip$bike_id==bikeid])
  
}


bikeusage <-data.frame( table(trip$bike_id))
x <- c('bike id', 'Usage count')

colnames(bikeusage) <- x


create_model <- function(trainData,target) {
  set.seed(120)
  myglm <- glm(target ~ . , data = dataset_date_trip, family = "binomial")
  return(myglm) }

score <- predict(myglm, newdata = dataset_date_trip, type = 'response')
score_train <- predict(myglm, newdata = complete, type='response')


################ Trip Distribution over the period of time ##############

customtripdata <- data.frame(trip$id,trip$duration,trip$start_date,trip$end_date,trip$start_station_id,trip$end_station_id)

dim(customtripdata)


dataset_date_trip$trips = ma(dataset_date_trip$trips, order=7) # using the clean count with no outliers


# The Plot of date with respect to the count of trips

ggplot() +
  geom_line(data = dataset_date_trip, aes(x = dataset_date_trip$date, y = dataset_date_trip$trips)) +
  ylab('Bicycle Count') +
  xlab('Date')
