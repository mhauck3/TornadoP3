library(shinythemes)
library(shiny)
library(shinydashboard)
library(markdown)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(jpeg)
library(DT)
library(grid)
library(rsconnect)
library(scales)
library(leaflet)
library(geosphere)


# DATA PRE-PROCESSING
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
data[yr == 2016, loss:= loss/1000000]

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

#REMOVE BAD DATA
data = data[!fscale == -9,]

# HELPER FUNCTIONS
normalize = function (x){
  (x-min(x))/(max(x)-min(x))
  }

getGeoDist = function(startlat, startlon, endlat, endlon){
  rad = pi/180
  a1 = startlat * rad
  a2 = startlon * rad
  b1 = endlat * rad
  b2 = endlon * rad
  dlon = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  R = 6378.145
  distance = R * c
  distance
}

# Define new column
#data = data %>%
#  mutate(distance = distm(c(startLon, startLat),c(endLon, endLat), fun = distHaversine)) %>%
#  data.table()

#data[,distance:= distm(as.numeric(c(startLon, startLat)),as.numeric(c(endLon, endLat)), fun = distHaversine)]
#distm(c(1, 1),c(2, 2), fun = distHaversine)

# PLOT GENERATING FUNCTIONS

#PART C9  
map_track_state_year = function(year_var, state_var, frange = c(-9,9), wrange = c(0,5000), lrange = c(0,250), 
                                irange = c(0,1800), fatrange = c(0,160), map_markers="fscale"){
  frange = seq(min(frange), max(frange))
  track_data = data[stateNumber2 == 1 & startLat > 0 & startLon < 0 & endLat > 0 & endLon <0 &
                      year == year_var & 
                      state == state_var &
                      fscale %in% frange &
                      width %between% wrange & 
                      length %between% lrange & 
                      injuries %between% irange &
                      fatalities %between% fatrange,
                    c("tornadoID", "startLon", "startLat","endLat","endLon", map_markers), with = FALSE]
  setnames(track_data, map_markers, "map_marker")
  
  #Normalize data - In order to visualize
  track_data[, ("map_marker_normal") := (normalize(as.numeric(map_marker))+1)*3]
  if (is.factor(track_data$map_marker)){
    track_data[, ("map_marker") := as.numeric(as.character(map_marker))]
  }
  
  track_state_start = track_data[,c("tornadoID", "startLon", "startLat", "map_marker", "map_marker_normal"), with = FALSE]
  track_state_end = track_data[,c("tornadoID", "endLat","endLon", "map_marker", "map_marker_normal"), with = FALSE]
  setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
  setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
  track_state = rbind(track_state_start,track_state_end)
  
  pal_colors = colorRamp(c("#ff6767", "red4"))
  pal <- colorNumeric(
    palette = pal_colors,
    domain = track_state$map_marker)
  
  m = leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Dark") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "City Classic") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Topological") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite") %>%
    setView(-87.987437, 41.913741, zoom = 5) %>%
    addLayersControl(baseGroups = c("Light","Dark", "City Classic", "Topological", "Sattelite"),
                     options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", pal = pal, values = track_state$map_marker,
              opacity = 1, bins = 5
    )
  for (i in unique(track_state$tornadoID)) {
    m <- m %>%
      addPolylines(data = track_state[tornadoID == i],
                   lng = ~lon,
                   lat = ~lat,
                   col = ~pal(map_marker),
                   weight = ~(map_marker_normal),
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                          bringToFront = TRUE),
                   label = ~paste(map_markers, ":",map_marker)) 
  }
  return(m)
}

#PART B1

shinyApp(
  ui = fluidPage(
    theme = shinytheme("superhero"),
    
    tabPanel("Something",
             sidebarLayout(
               sidebarPanel(width = 2, h2("Preferences"),
                            
                            radioButtons("hr", h3("Hour:"),
                                         choices = list("24Hr" = 1, "12Hr" = 2),selected = 1),
                            
                            radioButtons("units", h3("Units:"),
                                         choices = list("Imperial" = 1, "Metric" = 2),selected = 1),
                            h2("Filters"),
                            sliderInput("width_input",label=h3("Width:"), min=0, max=5000, value = c(0, 5000),width="100%"),
                            sliderInput("length_input",label=h3("Length:"), min=0, max=250, value = c(0, 250),width="100%"),
                            sliderInput("injuries_input",label=h3("Injuries:"), min=0, max=1800, value = c(0, 1800),width="100%"),
                            sliderInput("fatalities_input",label=h3("Fatalities:"), min=0, max=160, value = c(0, 160),width="100%"),
                            sliderInput("loss_input",label=h3("Loss"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
                            sliderInput("fscale_input",label=h3("F-Scale"), min=0, max=5, value = c(0, 5),width="100%"), #Ugly range (just to include -9). Need to change this
                            tags$style(HTML(".irs-grid-text { font-size: 0pt; } .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
                            tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                            tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: red}")),
                            tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
                            tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: red}")),
                            HTML("<div class=card text-white bg-primary mb-3 style=max-width: 20rem;><div class=card-header>Header</div><div class=card-body><h4 class=card-title>About</h4>
         <p class=card-text>Some quick example text to build on the card title and make up the bulk of the card's content.</p>
         </div></div>")),
               
               mainPanel(
                 
                 
                 fluidRow(
                   
                   column(9,
                          
                          "Sliders",
                          
                          sliderInput("year_input",label=h4("Year:"), min=1950, max=2016, value = 1950,animate = TRUE,width="100%",step=1),
                          
                          tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red}")),
                          tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: red}")),
                          #sliderInput("Month_input", "Month:"),
                          #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                          #sliderInput("Day_input", "Day:"),
                          #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                          #sliderInput("Hour_input", "Hour:", 0, 24, 0),
                          fixedRow(
                            column(8,
                                   leafletOutput("map_track"),
                                   "Map",
                                   radioButtons("radio", h3("Inputs buttons"),
                                                choices = list("F-scale" = "fscale", "Injuries" = "injuries",
                                                               "Losses" = "loss", "Length" = "length", 
                                                               "Width" = "width", "Fatalities" = "fatalities"),selected = "fscale")
                            ),
                            column(4,
                                   "Table"
                            )
                          )
                          
                   ),
                   column(2,
                          "Column 2",
                          fluidRow("Heat Map"),
                          fluidRow("10 destructive Tornadoes")
                   )
                 )
               )
             )),
    tabPanel("Summary",
             verbatimTextOutput("summary")
    )
  ),
  server = function(input, output) {
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
    data = data %>% 
      mutate(distChicagoStrt = getGeoDist(startlat = 41.8781, startlon = -87.6298,
                                          endlat = startLat, endlon = startLon),
             distChicagoEnd = getGeoDist(startlat = 41.8781, startlon = -87.6298,
                                         endlat = endLat, endlon = endLon)) %>% 
      data.table()
    
    data$distChicago = apply(data[,.(distChicagoStrt, distChicagoEnd)], MARGIN = 1, FUN = min)
    
    data = data %>%
      mutate(distChicagoBin = ifelse(distChicago<100,"< 100 km", ifelse(distChicago<200,"100 - 200 km", ifelse(distChicago < 400, "200 - 400 km", "> 400 km")))) %>%
      data.table()
    
    getBinInfo = function(distBin = c("< 100 km","100 - 200 km", "200 - 400 km","> 400 km")){
      binData = data %>%
        filter(distChicagoBin %in% distBin)
      binData
    }
    
    binRecords = getBinInfo(distBin = "< 100 km")
    
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
    
    tornadoesByCounty = data %>%
      group_by(fips) %>%
      summarize(tornadoCount = n()) %>%
      arrange(-tornadoCount) %>% 
      # mutate(fips = as.factor(fips)) %>%
      data.table()
    
    #ggplot(tornadoCountByCounty[1:nrow(tornadoCountByCounty) %in% 1:10], aes(x = fips, y = tornadoCount)) + geom_bar(stat = "identity")
    
    ggplot(tornadoesByCounty, aes(x = reorder(fips, -tornadoCount), y = tornadoCount)) + geom_bar(stat = "identity")
    
    # C9
    #Function maps tornados by year. Excludes missing coordinates
    output$map_track = renderLeaflet({
      map_track_state_year(input$year_input, state_var = "IL", frange = input$fscale_input, wrange = input$width_input, 
                           lrange = input$length_input, irange = input$injuries_input, fatrange = input$fatalities_input,
                           map_markers = input$radio)
    }
    )
    
  }
)
