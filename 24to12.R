data = fread("Dataset/allTornadoes.csv")


newNames = c("tornadoNumber", "year", "month", "day", "date", "time", "timeZone", "state", "fips", 
             "stateNumber", "fscale","injuries", "fatalities", "loss", "cropLoss", "startLat", 
             "startLon", "endLat", "endLon", "length", "width", "numberOfStates", "stateNumber2", 
             "tornadoSegment","fips1st", "fips2nd", "fips3rd", "fips4th","fscale2")
setnames(data, newNames)

data$time_12=format(strptime(data$time, format='%H:%M:%S'), '%r')
