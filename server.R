#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(scales)
library(DT)

shinyServer(
  function(input, output) { 
    linechoice <- reactive({
      finalturnstiledata %>% filter(LineID == input$linename)
    })
    
    output$Entry <- renderPlot(
      linechoice() %>%
        ggplot(aes(x = STATION, y = TOTAL6MONTHS/1000000)) +
        geom_col(fill = "red") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      + ggtitle(paste(input$linename, "Line Entries by Station")) + ylab('Total Entries (in millions)')
    )
    output$mymap <- renderLeaflet(
      leaflet(linechoice()) %>% addTiles("https://api.mapbox.com/styles/v1/mapbox/streets-v10/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGxhdTExNyIsImEiOiJjajhlcjE4a3kxNzkzMzNuOHN2d3hiajkyIn0.U9VY6-Um2f99cfPj9n46Ow") %>%
        fitBounds(~min(finalturnstiledata$GTFS.Longitude), ~min(finalturnstiledata$GTFS.Latitude), ~max(finalturnstiledata$GTFS.Longitude), ~max(finalturnstiledata$GTFS.Latitude)) %>% 
        addCircleMarkers(linechoice()$GTFS.Longitude, linechoice()$GTFS.Latitude, label = ~htmlEscape(paste(linechoice()$STATION, "\n", linechoice()$TOTAL6MONTHS)), radius = linechoice()$TOTAL6MONTHS/650000)
    )
    output$Pie <- renderPlot(
      borototals %>% ggplot(aes(x = "", y = Total, fill = Borough)) + 
        geom_bar(width = 1, stat = "identity") + coord_polar("y") + 
        xlab("") + ylab("") + theme_minimal() + 
        scale_fill_discrete(labels = c('Brooklyn 19.8%', 'Bronx 4.4%', 'Manhattan 64.5%', 'Queens 11.3%')) + 
        ggtitle("Entries by Borough")
    )
    output$rawdata <- renderDataTable(
      turnstileinfo
    )
    output$bad <- renderDataTable(
      baddata
    )
  }
)