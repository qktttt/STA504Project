library(dplyr)
library(shiny)
library(shinyjs)
library(plotly)
library(ggmap)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(shinyWidgets)

setwd("C:/Users/qik/Desktop/STA404 Project/")
load("SavedData.RData")

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "New York Airbnb"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      # put your input controls here!
      conditionalPanel(
        condition = "input.tabs == 'Mapplot' ", 
        selectInput(inputId="roomType", label="Select a Airbnb Type",
                    choices=c("All rooms" = "all",
                              "Private Room"="Private room",
                              "Entire Home/Apartment"="Entire home/apt",
                              "Shared Room"="Shared room")), 
      ),
      conditionalPanel(
        condition = "input.tabs == 'Statplot' ", 
        selectInput(inputId="area", label="Select a Area",
                    choices=(unique(Airbnb2$Area))), 
      )
    ),
    # Main panel typically used to display outputs
    # To display multiple tabs, use "Tabsets"
    # refer to https://shiny.rstudio.com/articles/layout-guide.html
    
    # mainPanel(
    #   plotOutput(outputId="timeplot")
    # )
    mainPanel(
      tabsetPanel(
        id="tabs",
        tabPanel("Mapplot", plotlyOutput("Mapplot")), 
        tabPanel("Statplot", plotlyOutput("Statplot"), 
                 plotlyOutput("PercentagePlot")) #, 
        #tabPanel("CorrePlot")
      )
    )    
    
  )
)


### Define server behavior for application here
server <- function(input, output) {
  selectedGroupedData <- grouped_by_data
  
  textFunction <- function(dta) {
    map(
      paste(
        ' <b>Area:</b>',
        dta$Area,
        '<br>',
        '<b>Neighborhood:</b>', 
        dta$Subarea,
        '<br>',
        '<b>Average Price:</b>',
        sprintf("%02f", dta$meanPrice)
      ),
      HTML
    )
  }
  
  output$Mapplot <- renderPlotly({
    if (input$roomType != "all") {
      selectedGroupedData <- grouped_by_data_withType %>% filter(room_type == input$roomType)
    }
    
    breaks = quantile(log(selectedGroupedData$meanPrice), c(0.05, 0.5, 0.75, 0.95))
    labels <- round(exp(breaks))
    
    result <- ggmap(newYorkMap) + geom_point(data=selectedGroupedData, aes(x=long, y=lat, colour=log(meanPrice), 
                              text=textFunction(selectedGroupedData))) + 
      scale_colour_continuous(low="lightgray", high="darkred",
                            breaks = breaks,
                            labels = labels)  +
      theme(legend.title = element_text("Average Price")) 
      
    ggplotly(result, height=900, width=1100, tooltip = "text")
    
  })
  
  output$Statplot <- renderPlotly({
    airbnbNumber <- Airbnb2 %>% group_by(Area) %>% count()
    airbnbNumber <- airbnbNumber %>% arrange(n)
    result <- ggplot() + geom_histogram(aes(x=Area, y=n), data=airbnbNumber, stat="identity", fill="orange", binwidth = 0.5) + labs(
      x = "District Name", y = "Number of Airbnb", title = "Number of Airbnb for different Areas"
    )
    ggplotly(result, width=600)
  }) 
  
  output$PercentagePlot <- renderPlotly({
    if (input$area != "") { 
      selectedDta <- Airbnb2 %>% filter(Area == input$area)
    } else {
      selectedDta <- Airbnb2
    }
    
    selectedDta_Count <- selectedDta %>% group_by(room_type) %>% count() %>% mutate(room_type = as.factor(room_type))
    oevrallSum <- sum(selectedDta_Count$n)
    selectedDta_Count <- selectedDta_Count %>% mutate(percentage = n / oevrallSum)
    
    perPlot <- ggplot(selectedDta_Count, aes(x = room_type, y = percentage, fill = room_type)) +
                geom_histogram(stat="identity", fill="lightblue") + labs(
                  x = "Room Type",
                  y = "Percentage of room type",
                  title = paste("Percentage of Three Room Types for Area: ", input$area, sep="")
                )
    
    ggplotly(perPlot, width=600)
  })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
