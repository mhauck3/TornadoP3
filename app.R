# Project 3 - You Spin Me Round
# Group 5 - Megan, Pedro, Namaswi, Shoaib

library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(jpeg)
library(DT)
library(grid)
library(rsconnect)
library(scales)
library(shiny)
library(shinydashboard)
library(leaflet)
library(geosphere)

#Read data
data = fread("Dataset/allTornadoes.csv")

#Change adjust loss data 
data[yr < 1996 & loss == 1, loss := 25/1000000]
data[yr < 1996 & loss == 2, loss := 275/1000000]
data[yr < 1996 & loss == 3, loss := 2750/1000000]
data[yr < 1996 & loss == 4, loss := 27500/1000000]
data[yr < 1996 & loss == 5, loss := 275000/1000000]
data[yr < 1996 & loss == 6, loss := 2750000/1000000]
data[yr < 1996 & loss == 7, loss := 27500000/1000000]
data[yr < 1996 & loss == 8, loss := 50000000/1000000]

#Set names and datatype
newNames = c("tornadoNumber", "year", "month", "day", "date", "time", "timeZone", "state", "fips", 
             "stateNumber", "fscale","injuries", "fatalities", "loss", "cropLoss", "startLat", 
             "startLon", "endLat", "endLon", "length", "width", "numberOfStates", "stateNumber2", 
             "tornadoSegment","fips1st", "fips2nd", "fips3rd", "fips4th","fscale2")
setnames(data, newNames)
data$date = as.Date(data$date)
factor_list = c("tornadoNumber", "year", "month", "day", "timeZone", "state", "fips", 
                "stateNumber", "fscale","fips1st", "fips2nd", "fips3rd", "fips4th","fscale2")
data[, (factor_list) := lapply(.SD, factor), .SDcols=factor_list]

#Create tornado ID (unique to the state single track, NOT unique as a key)
data[, tornadoID := paste(year,tornadoNumber, sep = "")]
data[, tornadoID := factor(tornadoID)]

newData = data %>%
  mutate(rad = pi/180,
         a1 = startLat * rad,
         a2 = startLon * rad,
         b1 = endLat * rad,
         b2 = endLon * rad,
         dlon = b2 - a2,
         dlat = b1 - a1,
         a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2,
         c = 2 * atan2(sqrt(a), sqrt(1 - a)),
         R = 6378.145,
         distance = R * c) %>%
  select(-rad,-a1,-a2,-b1,-b2,-dlon,-dlat,-a,-c,-R) %>%
  mutate(distance = ifelse(endLat ==0 & endLon == 0, 0, distance)) %>% #making distance = 0 when both end lat and end lon are not available(ie. end lat = end long = 0)
  data.table()

newData

# Define new column
#data = data %>%
#  mutate(distance = distm(c(startLon, startLat),c(endLon, endLat), fun = distHaversine)) %>%
#  data.table()

#data[,distance:= distm(as.numeric(c(startLon, startLat)),as.numeric(c(endLon, endLat)), fun = distHaversine)]
#distm(c(1, 1),c(2, 2), fun = distHaversine)

# C1
tornadoesByYear = data %>%
  group_by(year, fscale) %>%
  summarize(tornadoCount = n()) %>%
  group_by(year) %>%
  mutate(annualTornadoCount = sum(tornadoCount)) %>%
  mutate(percTornadoByFscale = tornadoCount/annualTornadoCount) %>%
  data.table()

tornadoesOverall = nrow(data)
# nrow(data)
# ncol(data)
# dim(data)[1]
# dim(data)[2]

data %>%
  group_by(fscale) %>%
  summarize(totalCount = n())

ggplot(tornadoesByYear, aes(x = year, y = tornadoCount, color = fscale)) + geom_bar(stat = "identity", position = 'dodge')

ggplot(tornadoesByYear, aes(x = year, y = percTornadoByFscale, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position = 'dodge') #sans identity, we get row count -- yikes!

# C2
tornadoesByMonth = data %>%
  group_by(month, fscale) %>%
  summarize(tornadoCount = n()) %>%
  group_by(month) %>%
  mutate(monthlyTornadoCount = sum(tornadoCount)) %>%
  mutate(percTornadoByFscale = tornadoCount/monthlyTornadoCount) %>%
  data.table()

ggplot(tornadoesByMonth, aes(x = month, y = tornadoCount, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')

# C3 
tornadoesByHour = data %>%
  mutate(hour = as.numeric(substr(time,start = 1, stop = 2))) %>%
  group_by(hour, fscale) %>%
  summarize(tornadoCount = n()) %>%
  group_by(hour) %>%
  mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
  mutate(percTornadoByFscale = tornadoCount/hourlyTornadoCount) %>%
  data.table()

ggplot(tornadoesByHour, aes(x = factor(hour), y = tornadoCount, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')

# C4
# woohoo! It's underway. Add two columns using mutate: startLatLon and endLatLon <-- to calculate distance from Chicago; based on the range, eg. 0-100 km, filter out tornadoes in that range and THEN group it

# C5
damagesByYear = data %>%
  group_by(year) %>%
  summarize(damagesInjuries = sum(injuries),
            damagesFatalities = sum(fatalities),
            damagesLoss = sum(loss)) %>%
  data.table()

ggplot(damagesByYear, aes(x = year, y = damagesInjuries)) + geom_bar(stat = "identity")

ggplot(damagesByYear, aes(x = year, y = damagesFatalities)) + geom_bar(stat = "identity")

# C6
damagesByMonth = data %>%
  group_by(month) %>%
  summarize(damagesInjuries = sum(injuries),
            damagesFatalities = sum(fatalities),
            damagesLoss = sum(loss)) %>%
  data.table()

ggplot(damagesByMonth, aes(x = factor(month), y = damagesInjuries)) + geom_bar(stat = "identity")

ggplot(damagesByMonth, aes(x = factor(month), y = damagesFatalities)) + geom_bar(stat = "identity")

ggplot(damagesByMonth, aes(x = factor(month), y = damagesLoss)) + geom_bar(stat = "identity")

# C7

damagesByHour = data %>%
  mutate(hour = substr(time,1,2)) %>%
  group_by(hour) %>%
  summarize(damagesInjuries = sum(injuries),
            damagesFatalities = sum(fatalities),
            damagesLoss = sum(loss)) %>%
  data.table()

ggplot(damagesByHour, aes(x = factor(hour), y = damagesInjuries)) + geom_bar(stat = "identity")

ggplot(damagesByHour, aes(x = factor(hour), y = damagesFatalities)) + geom_bar(stat = "identity")

ggplot(damagesByHour, aes(x = factor(hour), y = damagesLoss)) + geom_bar(stat = "identity")

# C8

tornadoCountByCounty = data %>%
  group_by(fips) %>%
  summarize(tornadoCount = n()) %>%
  arrange(-tornadoCount) %>% 
# mutate(fips = as.factor(fips)) %>%
  data.table()

#ggplot(tornadoCountByCounty[1:nrow(tornadoCountByCounty) %in% 1:10], aes(x = fips, y = tornadoCount)) + geom_bar(stat = "identity")

ggplot(tornadoesByCounty, aes(x = reorder(fips, -tornadoCount), y = tornadoCount)) + geom_bar(stat = "identity")

# C9

#Function maps tornados by year. Excludes missing coordinates
map_track_state_year = function(year_var, state_var){
  track_state_start = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                             endLat > 0 & endLon <0, c("tornadoID", "startLon", "startLat")]
  track_state_end = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                         endLat > 0 & endLon <0, c("tornadoID", "endLat","endLon")]
  setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
  setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
  track_state = rbind(track_state_start,track_state_end)
  
  m = leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
  for (i in unique(track_state$tornadoID)) {
    print(i)
    m <- m %>%
      addPolylines(data = track_state[tornadoID == i],
                   lng = ~lon,
                   lat = ~lat)
  }
  return(m)
}

#map_track_state_year(year_var = 2013, state_var = "MO")

#ggplot(damagesByYear, aes(x = factor(year), y = damagesLoss)) + geom_bar(stat = "identity")
