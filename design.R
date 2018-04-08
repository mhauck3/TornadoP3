#install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(shinydashboard)
library(markdown)
shinyApp(
  
  ui = fluidPage(
    theme = shinytheme("superhero"),
    
    tabPanel("Something",
             sidebarLayout(
               sidebarPanel(width = 2, h2("Preferences"),
    
    radioButtons("hr", h3("Hour:"),
                 choices = list("24Hr" = 1, "12Hr" = 2),selected = 1),
    
    radioButtons("units", h3("Units:"),
                 choices = list("Imperical" = 1, "Metric" = 2),selected = 1),
    h2("Filters"),
    sliderInput("width_input",label=h3("Width:"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
    sliderInput("length_input",label=h3("Length:"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
    sliderInput("injuries_input",label=h3("Injuries:"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
    sliderInput("fatalities_input",label=h3("Fatalities:"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
    sliderInput("loss_input",label=h3("Loss"), min=1950, max=2016, value = c(1950, 2016),width="100%"),
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
                        
                        sliderInput("year_input",label=h4("Year:"), min=1950, max=2016, value = c(1950, 2016),animate = TRUE,width="100%",step=1),
                        
                        tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red}")),
                        tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: red}")),
                        #sliderInput("Month_input", "Month:"),
                        #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                        #sliderInput("Day_input", "Day:"),
                        #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
                        #sliderInput("Hour_input", "Hour:", 0, 24, 0),
                        fixedRow(
                          column(8,
                                 "Map"
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
  server = function(input, output) {}
  
)
 