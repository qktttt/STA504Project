library(dplyr)
library(shiny)
library(shinyjs)
library(plotly)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinyWidgets)

### Set to the path of the repository after you get the repository
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
      width = 4,
      conditionalPanel(
        condition = "input.tabs == 'Visualization with Map' ",
        selectInput(inputId="roomType", label="Select a Airbnb Type",
                    choices=c("All rooms" = "all type",
                              "Private Room"="Private room",
                              "Entire Home/Apartment"="Entire home/apt",
                              "Shared Room"="Shared room")),
        plotlyOutput("histogram"),
        span(tags$h3(textOutput("mapDescription")))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Visualization by Area' ",
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
        tabPanel("Visualization with Map", plotlyOutput("Mapplot")),
        tabPanel("Visualization by Area", plotlyOutput("Statplot"),
                 plotlyOutput("PercentagePlot")) #,
        #tabPanel("CorrePlot")
      )
    )

  )
)


### Define server behavior for application here
server <- function(input, output) {
  selectedGroupedData <- grouped_by_data
  storedSubarea <- reactiveVal("New York City")
  storedSubareaData <- reactiveVal(Airbnb2)
  roomType <- reactiveVal("all types")

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

  #### creating the map plot here
  output$Mapplot <- renderPlotly({
    if (input$roomType != "all type") {
      selectedGroupedData <- grouped_by_data_withType %>% filter(room_type == input$roomType)
    }

    breaks = quantile(log(selectedGroupedData$meanPrice), c(0.05, 0.5, 0.75, 0.95, 0.99))
    labels <- round(exp(breaks))

    result <- ggmap(newYorkMap) + geom_point(data=selectedGroupedData, aes(x=long, y=lat, colour=log(meanPrice),
                              text=textFunction(selectedGroupedData)), size=2.5) +
      scale_colour_continuous(low="lightgray", high="darkred",
                            breaks = breaks,
                            labels = labels)  +
      theme(legend.title = element_text("Average Price"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(paste("Airbnb Neighborhoods Average Price Map -", input$roomType, "of Airbnb")) + labs(x="", y="", colour = "Average Price")

    ggplotly(result, height=700*1.1, width=1000*1.1, tooltip = "text")

  })

  # create the bar plot for number of airbnbs at different area
  output$Statplot <- renderPlotly({
    airbnbNumber <- Airbnb2 %>% group_by(Area) %>% count()
    airbnbNumber <- airbnbNumber %>% arrange(n)
    result <- ggplot() + geom_histogram(aes(x=Area, y=n), data=airbnbNumber, stat="identity", fill="orange", binwidth = 0.5) + labs(
      x = "District Name", y = "Number of Airbnb", title = "Number of Airbnb for different Areas"
    ) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(result, width=600)
  })

  # Creating histogram
  output$histogram <- renderPlotly({
    data <- event_data("plotly_click")

    ### filter possible data
    selectedData <- NULL
    if (is.null(data)) {
      selectedSubarea = "New York City"
      selectedData <- Airbnb2

    } else {
      print(data)
      # filter out invalid click
      if (as.integer(data["curveNumber"]) == 1 | abs(as.integer(data["x"])) < 20) {
        selectedSubarea = "New York City"
        selectedData <- Airbnb2
      } else {
        selectedX <- as.numeric(data["x"])
        selectedY <- as.numeric(data["y"])

        ## searching which area is clicked
        #print(grouped_by_data)
        print(selectedX)
        print(selectedY)
        distances <- sqrt((grouped_by_data$long * 1000 - selectedX * 1000)^2 + (grouped_by_data$lat * 1000 - selectedY * 1000)^2)
        print(length(distances))
        print(distances)
        selectedSubarea <- grouped_by_data$Subarea[which(distances == min(distances))]
        print(selectedSubarea)
        selectedData <- Airbnb2 %>% filter(Subarea == selectedSubarea)
      }
    }


    ### store the roomtype for further text description
    if (input$roomType != "all type") {
      selectedData <- selectedData %>% filter(room_type == input$roomType)
      roomType(paste(input$roomType, "type"))
    } else {
      roomType("all types")
    }

    storedSubarea(selectedSubarea)
    storedSubareaData(selectedData)
    #print(selectedData)
    result <- ggplot() + geom_histogram(data=selectedData, aes(x=price), binwidth = 10, color="orange") + xlim(0, min(800, max(selectedData$price) + 10)) +
      ggtitle(paste("Airbnb price Histogram \n for:", selectedSubarea, "-", input$roomType)) + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(result, width=550)
  })



  # ploting the percentage plot
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
                  y = "Proportion of room type",
                  title = paste("Proportion of Three Room Types for Area: ", input$area, sep="")
                ) + theme(plot.title = element_text(hjust = 0.5))

    ggplotly(perPlot, width=600)
  })

  # creating text description for the data clicked
  output$mapDescription <- renderText({
    result <- paste("Current seleced area is: ", storedSubarea(), ".", sep="")
    numAvai <- NROW(storedSubareaData())
    meanPrice <- mean(storedSubareaData()$price)
    medianPrice <- median(storedSubareaData()$price)
    maxPrice <- max(storedSubareaData()$price)
    minPrice <- min(storedSubareaData()$price)
    result <- paste(result, paste(" In this area, considering Airbnb of ", roomType(),
                                  ", there are ", as.integer(numAvai), " Airbnbs available. The average Airbnb price at here is ",sprintf("%.2f", meanPrice),
    ". The maximum, minimum Airbnb price in this area are, respectively: ", sprintf("%.2f", maxPrice),
    " ", sprintf("%.2f", minPrice), ". And the median price is: ", sprintf("%.2f", medianPrice), ".", " For display effects, some extremely values maybe removed.",sep=""))
  })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
