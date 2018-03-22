#############################################################
# Data Mining Project                                       #
# Mining Data with R and Rattle                             #
#############################################################

# Handling Big datasets
# To remove memory
rm(list = ls()) 
# Set system language to english in  
Sys.setenv(LANGUAGE="en")
setwd("C:/Users/Administrateur/Downloads/Datasets/sf-bay-area-bike-share")

library(sqldf)
library(RSQLite)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(Hmisc)
library(reshape2)
library(GGally)
library(plyr)


#################################################################
# Connection to the Database                                    #
#################################################################

con = dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")
tables <- dbListTables(con)

# To hide warnings
options(warn=-1)

# Get the Table Properties

#################################################################
# Data Understanding, Data Preparation, Modeling and Evaluation #
#################################################################

stationTableProperties <- dbGetQuery(conn=con, statement=paste(" PRAGMA table_info(station)", sep=""))
stationTableProperties
statusTableProperties <- dbGetQuery(conn=con, statement=paste(" PRAGMA table_info(status)", sep=""))
statusTableProperties
tripTableProperties <- dbGetQuery(conn=con, statement=paste(" PRAGMA table_info(trip)", sep=""))
tripTableProperties
weatherTableProperties <- dbGetQuery(conn=con, statement=paste(" PRAGMA table_info(weather)", sep=""))
weatherTableProperties

# View Sample Dataset

dbGetQuery(conn=con, statement=paste("SELECT * FROM station LIMIT 5", sep=""))
dbGetQuery(conn=con, statement=paste("SELECT * FROM status LIMIT 5", sep=""))
dbGetQuery(conn=con, statement=paste("SELECT * FROM trip LIMIT 5", sep=""))
dbGetQuery(conn=con, statement=paste("SELECT * FROM weather LIMIT 5", sep=""))

# Load Dataframes

stationDataFrame <- dbGetQuery(conn=con, statement=paste("SELECT * FROM station", sep=""))
tripDataFrame <- dbGetQuery(conn=con, statement=paste("SELECT * FROM trip", sep=""))
statusDataFrame <- dbGetQuery(conn=con, statement=paste("SELECT * FROM status", sep=""))
weatherDataFrame <- dbGetQuery(conn=con, statement=paste("SELECT * FROM weather", sep=""))


# Load Dataframes into new objects for short referrence

station <- stationDataFrame
trip <- tripDataFrame
status <- statusDataFrame
weather <- weatherDataFrame

# Summary of the dataframe
summary(stationDataFrame)
summary(tripDataFrame)
summary(statusDataFrame)
summary(weatherDataFrame)

# View full Dataframe but not recommended if the RAM memory is less than 4 GB

View(head(stationDataFrame))
View(head(tripDataFrame))
View(head(statusDataFrame))
View(head(weatherDataFrame))


#################################################################
# Data Checking for NULL or Infinite data                       #
#################################################################

apply(station, 0, function(x) any(is.na(x) | is.infinite(x) ))
apply(trip, 0, function(x) any(is.na(x) | is.infinite(x) ))
apply(status, 0, function(x) any(is.na(x) | is.infinite(x) ))
apply(weather, 0, function(x) any(is.na(x) | is.infinite(x) ))


############ Data Cleaning #############

weather$events[weather$events=='rain']<-"Rain"
weather$events[weather$events=='']<-"Normal Day"

#################################################################
# Visualization of Cycle Stations in map                        #
#################################################################
colnames(weather)

addMarkers(addTiles(leaflet()),lng=stationDataFrame$long, lat=stationDataFrame$lat, popup= stationDataFrame$name)