#install.packages("shinythemes")   C4  C5LOSS    HOUR     METRIC SYSTEM
library(rlang)
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
library(geotools)###CHANGE
library(gmt)###CHANGE
library(gridExtra)###CHANGE



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

#data = data %>%
#  mutate(rad = pi/180,
#         a1 = startLat * rad,
#         a2 = startLon * rad,
#         b1 = endLat * rad,
#         b2 = endLon * rad,
#         dlon = b2 - a2,
#         dlat = b1 - a1,
#         a = (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2,
#         c = 2 * atan2(sqrt(a), sqrt(1 - a)),
#         R = 6378.145,
#         distance = R * c) %>%
#  select(-rad,-a1,-a2,-b1,-b2,-dlon,-dlat,-a,-c,-R) %>%
#  mutate(distance = ifelse(endLat ==0 & endLon == 0, 0, distance)) %>% #making distance = 0 when both end lat and end lon are not available(ie. end lat = end long = 0)
#  data.table()

# Define new column
#data = data %>%
#  mutate(distance = distm(c(startLon, startLat),c(endLon, endLat), fun = distHaversine)) %>%
#  data.table()

#data[,distance:= distm(as.numeric(c(startLon, startLat)),as.numeric(c(endLon, endLat)), fun = distHaversine)]
#distm(c(1, 1),c(2, 2), fun = distHaversine)


  ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Sp 18 Project 2"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Information", tabName = "item0"),
        menuItem("Annual", tabName = "item1"),
        menuItem("Monthly", tabName = "item2"),
        menuItem("Hourly",tabName = "item3"),
 
        menuItem("Distance", tabName = "item4"),
        menuItem("Annual Data",tabName = "item5"),
        menuItem("Monthly Data",tabName = "item6"),
        menuItem("Hour",tabName = "item7"),
        menuItem("Frequency",tabName = "item8"),
        menuItem("LeafletMap",tabName = "item9"),
        menuItem("Safety HeatMap",tabName = "item10")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "item0",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset0", 
                         tabPanel("Information",
                                  htmlOutput("info"))
                  )
                )
        ),
        
        tabItem(tabName = "item1",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("Tornadototalplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C1Plot",width="450px",height="450px")) ),
                         tabPanel("TornadototalTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C1Table"))  )
                        )
                )
        ),
        
        
        tabItem(tabName = "item2",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("Tornadomonthplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C2Plot",width="450px",height="450px")) ),
                         tabPanel("TornamonthTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C2Table"))  )
                  )
                )
        ),
        
        tabItem(tabName = "item3",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("Tornadohourplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C3Plot",width="450px",height="450px")) ),
                         tabPanel("TornadohourTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C3Table"))  )
                  )
                )
        ),
        
        
        tabItem(tabName = "item4",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("TornadsoDistplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C4Plot",width="450px",height="450px")) ),
                         tabPanel("TornadoDistTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C4Table"))  )
                  )
                )
        ),
        tabItem(tabName = "item5",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("TornadsoYearlydataplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C5Plot",width="450px",height="450px")) ),
                         tabPanel("TornadoYearlydataTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C5Table"))  )
                  )
                )
        ),
        tabItem(tabName = "item6",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("TornadsoMonthlydataplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C6Plot",width="450px",height="450px")) ),
                         tabPanel("TornadoMonthlydataTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C6Table"))  )
                  )
                )
        ),
        tabItem(tabName = "item7",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("TornadsoHourlydataplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C7Plot",width="450px",height="450px")) ),
                         tabPanel("TornadoHourlydataTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C7Table"))  )
                  )
                )
        ),
        tabItem(tabName = "item8",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("TornadsoMonthlydataplot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("C8Plot",width="450px",height="450px")) ),
                         tabPanel("TornadoMonthlydataTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("C8Table"))  )
                  )
                )
        ),
        tabItem(tabName = "item9",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("Leafletmap",box( title = "Safety Mapss", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("GradHeat2",width="600px",height="600px")) ),
                         sliderInput("year_input",label=h4("Year:"), min=1950, max=2016, value = 1950,animate = TRUE,width="100%",step=1)
                         )
                )
        )
        ,
        tabItem(tabName = "item10",
                fluidRow(
                  tabBox(title = "",
                         width = "100%",
                         height = "2000px",
                         id = "tabset2", 
                         tabPanel("SafetyMaps",box( title = "Safety Mapss", solidHeader = TRUE, status = "primary", width = 12, plotOutput("GradHeat",width="600px",height="600px")) )
                  )
                )
        )
        
        
    )
  )
  )
  
  server = function(input, output) {
    
    
    output$info <- renderUI({
      HTML('<p><b>Authors:</b> Pedro, Shoaib, Namaswi, and Megan</p>
           <p><b>Required libaries</b>: shiny, ggplot2, shinydashboard</p>
           <p> <b>Data Source:</b>http://www.spc.noaa.gov/wcm/index.html#data</p>
           <p>: This project shows data regarding how Tornados have affected the United States over the last several years, particularly in Illinois </p>')
      
    })
       ####################### C1
 
    output$C1Table <-DT::renderDataTable(
      
      
      
      DT::datatable({ 
          tornadoesByYear = data %>%
          group_by(year, fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(year) %>%
          mutate(annualTornadoCount = sum(tornadoCount)) %>%
          mutate(percTornadoByFscale = tornadoCount/annualTornadoCount) %>%
          data.table()
      
          tornadoesByYear
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C1Plot <- 
      renderPlot({
    
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
    g1=ggplot(tornadoesByYear, aes(x = year, y = tornadoCount, color = fscale)) + geom_bar(stat = "identity", position = 'dodge')
    g2=ggplot(tornadoesByYear, aes(x = year, y = percTornadoByFscale, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position = 'dodge') #sans identity, we get row count -- yikes!
    grid.arrange(g1,g2)
    })
  
    ####################### C2
    
    
    output$C2Table <-DT::renderDataTable(
      
      
      
      DT::datatable({ 
        tornadoesByMonth = data %>%
          group_by(month, fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(month) %>%
          mutate(monthlyTornadoCount = sum(tornadoCount)) %>%
          mutate(percTornadoByFscale = tornadoCount/monthlyTornadoCount) %>%
          data.table()
        
        tornadoesByMonth
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C2Plot <- 
      renderPlot({
        
        tornadoesByMonth = data %>%
          group_by(month, fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(month) %>%
          mutate(monthlyTornadoCount = sum(tornadoCount)) %>%
          mutate(percTornadoByFscale = tornadoCount/monthlyTornadoCount) %>%
          data.table()
        
        g1=ggplot(tornadoesByMonth, aes(x = month, y = tornadoCount, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')
        g2=ggplot(tornadoesByMonth, aes(x = month, y = percTornadoByFscale, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')
        grid.arrange(g1,g2)
      })

    

    ################################ C3 
 
    output$C3Table <-DT::renderDataTable(
      
      
      
      DT::datatable({ 
        tornadoesByHour = data %>%
          mutate(hour = as.numeric(substr(time,start = 1, stop = 2))) %>%
          group_by(hour, fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(hour) %>%
          mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
          mutate(percTornadoByFscale = tornadoCount/hourlyTornadoCount) %>%
          data.table()
        
        
        tornadoesByHour
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C3Plot <- 
      renderPlot({
        
        tornadoesByHour = data %>%
          mutate(hour = as.numeric(substr(time,start = 1, stop = 2))) %>%
          group_by(hour, fscale) %>%
          summarize(tornadoCount = n()) %>%
          group_by(hour) %>%
          mutate(hourlyTornadoCount = sum(tornadoCount)) %>%
          mutate(percTornadoByFscale = tornadoCount/hourlyTornadoCount) %>%
          data.table()
        
        g1=ggplot(tornadoesByHour, aes(x = factor(hour), y = tornadoCount, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')
        g2=ggplot(tornadoesByHour, aes(x = factor(hour), y = percTornadoByFscale, color = fscale, fill = fscale)) + geom_bar(stat = "identity", position ='dodge')
        grid.arrange(g1,g2)
        })
    
######################### C4?????  
    output$C4Table <-DT::renderDataTable(
      
      
      
      DT::datatable({ 
        data = data %>%   ###CHANGE
          mutate(distChicagoStrt = geodist(Nfrom = 41.8781, Efrom= -87.6298,   
                                           Nto = startLat, Eto = startLon),
                 distChicagoEnd = geodist(Nfrom = 41.8781, Efrom= -87.6298,     
                                          Nto = endLat, Eto= endLon)) %>% 
          data.table()
        
        data$distChicago = apply(data[,.(distChicagoStrt, distChicagoEnd)], MARGIN = 1, FUN = min)
        
        data = data %>%
          mutate(distChicagoBin = ifelse(distChicago<100,"< 100 km", ifelse(distChicago<200,"100 - 200 km", ifelse(distChicago < 400, "200 - 400 km", "> 400 km")))) %>%
          data.table()
        
        data
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C4Plot <- 
      renderPlot({
        
        data = data %>%   ###CHANGE
          mutate(distChicagoStrt = geodist(Nfrom = 41.8781, Efrom= -87.6298,   
                                           Nto = startLat, Eto = startLon),
                 distChicagoEnd = geodist(Nfrom = 41.8781, Efrom= -87.6298,     
                                          Nto = endLat, Eto= endLon)) %>% 
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
      })
    
    


    
    ########### C5??????  damageloss????
    
    output$C5Table <-DT::renderDataTable(
      
      
      
      DT::datatable({ 
        
        damagesByYear = data %>%
          group_by(year) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
        
        damagesByYear
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C5Plot <- 
      renderPlot({
        
        damagesByYear = data %>%
          group_by(year) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
        
        g1=ggplot(damagesByYear, aes(x = year, y = damagesInjuries)) + geom_bar(stat = "identity")
        g2=ggplot(damagesByYear, aes(x = year, y = damagesFatalities)) + geom_bar(stat = "identity")
        g3=ggplot(damagesByYear, aes(x = year, y = damagesLoss)) + geom_bar(stat = "identity")
        
        grid.arrange(g1,g2,g3)
      })
   
    ################# C6    
    output$C6Table <-DT::renderDataTable(
      DT::datatable({ 
      damagesByMonth = data %>%
        group_by(month) %>%
        summarize(damagesInjuries = sum(injuries),
                  damagesFatalities = sum(fatalities),
                  damagesLoss = sum(loss)) %>%
        data.table()
      
      
        damagesByMonth
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C6Plot <- 
      renderPlot({
        
        damagesByMonth = data %>%
          group_by(month) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
        
        g1=ggplot(damagesByMonth, aes(x = factor(month), y = damagesInjuries)) + geom_bar(stat = "identity")
        g2=ggplot(damagesByMonth, aes(x = factor(month), y = damagesFatalities)) + geom_bar(stat = "identity")
        g3=ggplot(damagesByMonth, aes(x = factor(month), y = damagesLoss)) + geom_bar(stat = "identity")
        grid.arrange(g1,g2,g3)
        
      })

    
    ############################## C7
    output$C7Table <-DT::renderDataTable(
      DT::datatable({ 
        damagesByHour = data %>%
          mutate(hour = substr(time,1,2)) %>%
          group_by(hour) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
        
        damagesByHour
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C7Plot <- 
      renderPlot({
        
        
        damagesByHour = data %>%
          mutate(hour = substr(time,1,2)) %>%
          group_by(hour) %>%
          summarize(damagesInjuries = sum(injuries),
                    damagesFatalities = sum(fatalities),
                    damagesLoss = sum(loss)) %>%
          data.table()
        
        g1=ggplot(damagesByHour, aes(x = factor(hour), y = damagesInjuries)) + geom_bar(stat = "identity")
        
        g2=ggplot(damagesByHour, aes(x = factor(hour), y = damagesFatalities)) + geom_bar(stat = "identity")
        
        g3=ggplot(damagesByHour, aes(x = factor(hour), y = damagesLoss)) + geom_bar(stat = "identity")
        
        grid.arrange(g1,g2,g3)
        
      })
    
    
    
    ############################## C8
    output$C8Table <-DT::renderDataTable(
      DT::datatable({ 
        tornadoesByCounty = data %>%
          group_by(fips) %>%
          summarize(tornadoCount = n()) %>%
          arrange(-tornadoCount) %>% 
          # mutate(fips = as.factor(fips)) %>%
          data.table()
        tornadoesByCounty
      } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
      )
      
    )
    
    output$C8Plot <- 
      renderPlot({
        tornadoesByCounty = data %>%
          group_by(fips) %>%
          summarize(tornadoCount = n()) %>%
          arrange(-tornadoCount) %>% 
          # mutate(fips = as.factor(fips)) %>%
          data.table()
        
        #ggplot(tornadoCountByCounty[1:nrow(tornadoCountByCounty) %in% 1:10], aes(x = fips, y = tornadoCount)) + geom_bar(stat = "identity")
        
        ggplot(tornadoesByCounty, aes(x = reorder(fips, -tornadoCount), y = tornadoCount)) + geom_bar(stat = "identity")
        
        
      })
   
    
    # C9
    #Function maps tornados by year. Excludes missing coordinates
    output$map_track = renderLeaflet({
      map_track_state_year = function(year_var, state_var){
        track_state_start = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                                   endLat > 0 & endLon <0, c("tornadoID", "startLon", "startLat")]
        track_state_end = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                                 endLat > 0 & endLon <0, c("tornadoID", "endLat","endLon")]
        setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
        setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
        track_state = rbind(track_state_start,track_state_end)
        
        m = leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(-87.987437, 41.913741, zoom = 5)
        for (i in unique(track_state$tornadoID)) {
          m <- m %>%
            addPolylines(data = track_state[tornadoID == i],
                         lng = ~lon,
                         lat = ~lat)
        }
        return(m)
      }      
      map_track_state_year(input$year_input, "IL")
    }
    )
    
 
    output$GradHeat <- renderPlot({
      fatalities_df=read.csv( file = "Dataset/heat_fatalities.csv",header=TRUE)
      injuries_df=read.csv(file = "Dataset/heat_injuries.csv",header=TRUE)
      
      
      g1=ggplot(fatalities_df, aes(x=long, y=lat, group=group, fill=numbers)) + 
        geom_polygon()+theme_void()
      
      g2=ggplot(injuries_df, aes(x=long, y=lat, group=group, fill=numbers)) + 
        geom_polygon()+theme_void()
      
      grid.arrange(g1,g2)
    })  
    
    output$GradHeat2 <- renderLeaflet({
      map_track_state_year = function(year_var, state_var){
        track_state_start = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                                   endLat > 0 & endLon <0, c("tornadoID", "startLon", "startLat")]
        track_state_end = data[state == state_var & stateNumber2 == 1 & year == year_var & startLat > 0 & startLon < 0 &
                                 endLat > 0 & endLon <0, c("tornadoID", "endLat","endLon")]
        setnames(track_state_start, c("startLon", "startLat"), c("lon","lat"))
        setnames(track_state_end, c("endLon", "endLat"), c("lon","lat"))
        track_state = rbind(track_state_start,track_state_end)
        
        m = leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(-87.987437, 41.913741, zoom = 5)
        for (i in unique(track_state$tornadoID)) {
          m <- m %>%
            addPolylines(data = track_state[tornadoID == i],
                         lng = ~lon,
                         lat = ~lat)
        }
        return(m)
      }      
      map_track_state_year(input$year_input, "IL")
    }
    )
    
    
  }
  
  
  
  
  shinyApp(ui = ui, server = server)
