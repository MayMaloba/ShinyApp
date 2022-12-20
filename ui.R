setwd("C:/Users/Paul/Documents/May/Data science/Homework 12/CriminalTrends")
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(shinythemes)



server <- function(input, output, session) {
  
  dff1 = read.csv("KansasCrimeLong.csv", header = TRUE,  sep = ",")
  dff <- pivot_longer(dff1,cols="X2012":"X2017",names_to = "year", values_to = "crime",values_drop_na = TRUE)
  dff$year <- sub(".","",dff$year)
  
  #Summarize Data and then Plot
  data <- reactive({
    req(input$geog_area)
    df <- dff %>% filter(County %in% input$geog_area) %>%  group_by(year) %>% summarise(crime = sum(crime))
  })
  
  #Update SelectInput Dynamically
  observe({
    updateSelectInput(session, "geog_area", choices = dff$County)
  })
  
  #Plot 
  output$plot <- renderPlot({
    g <- ggplot(data(), aes( y = crimes, x = year))
    g + geom_bar(stat = "sum")
  })
}


ui <- fluidPage(
  
  titlePanel("Trend of Crime Rate in Kansas"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "County",
                  label = "Choose Geographical Area",
                  "Names")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)




shinyApp(ui = ui, server = server)