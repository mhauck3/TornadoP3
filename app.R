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

#ggplot(damagesByYear, aes(x = factor(year), y = damagesLoss)) + geom_bar(stat = "identity")