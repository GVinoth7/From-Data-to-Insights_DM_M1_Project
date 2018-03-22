###################################################################
# Bike station analysis based on source and destination           #
###################################################################

# Unbalanced Trips of San Francisco station - Station(70) looks more unstable
departure_dur_counts = table(trip$start_station_id)

arrival_dur_counts = table(trip$end_station_id)
# Find Unstable station
options(repr.plot.width=8, repr.plot.height=4)

my_data_long <- melt(trip$start_station_id, trip$end_station_id)

ggplot(trip, aes(trip$start_station_id)) + 
  scale_fill_discrete(name ="Jouney")+
  geom_bar(stat = "count", aes(trip$end_station_id,fill = 'End place')) +
  geom_bar(stat = "count", aes(trip$start_station_id, fill = 'Start place')) +
  ylab("Total number of bicycle trips") +
  xlab("Station ID") + 
  ggtitle("Unstable Station based on trips") +
  theme_light()


#################################################################
#     Visualing the usage of bikes based on trips               #
#################################################################

ggplot(trip, aes(x=trip$bike_id))+
  scale_fill_discrete(name ="Usage")+
  geom_bar(aes(trip$bike_id,fill = ' Bike id'))+
  ylab("Number of trips") +
  xlab("Bike Id") + 
  ggtitle("Usage of bikes based on the trips") +
  theme_light()
