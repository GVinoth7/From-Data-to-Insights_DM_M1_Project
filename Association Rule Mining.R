###########################
# Association Rule Mining #
###########################

library(arulesViz)
library(arules)
 
Customtrip <-  trip

SelectedColumn <- c("id", "duration", "start_station_id", "end_station_id", "bike_id", "zip_code", "subscription_type")
Customtrip <- trip[SelectedColumn]

View(head(Customtrip))

# Assigning the Subscriber to 2 and Customer 1

Customtrip$subscription_type <- as.numeric(as.factor(Customtrip$subscription_type))

# Converting it into the factor

for(i in 1:7) { Customtrip[,i] <-  as.factor(Customtrip[,i])}

# Transaction class of the dataframe

trancustomtrip <- as(Customtrip, "transactions") 

inspect(head(trancustomtrip,3))

View(head(Customtrip))

rules = apriori(trancustomtrip, parameter=list(support=0.022, confidence=0.5))

rules

# Summary of rules
summary(rules)

plot(rules, method = "graph", shading = "lift")

# Plot methods graph, mosaic", "doubledecker", "graph", "paracoord" or "grouped", "iplots

inspectDT(rules)

# Inspect rules with the highest lift.
inspect(head(rules, by = "lift"))


#$$$$$$$$$$$$ Use of Eclat Algorithms $$$$$$$$$$$$$$$$

# calculates support for frequent items
frequentItems <- eclat (Customtrip, parameter = list(supp = 0.022, maxlen = 15))
inspect(frequentItems)

plot(frequentItems, method = 'graph', interactive = TRUE)

itemFrequencyPlot(frequentItems, topN=10, type="absolute", main="Item Frequency") # plot frequency
