#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shinydashboard)

# Define UI for application that draws a histogram


shinyUI(
  dashboardPage(skin = "green",
    dashboardHeader(title = "MTA Subway Data"),
    
    dashboardSidebar(
      sidebarUserPanel(name = "Tony Parrillo"
      ),
      selectizeInput(inputId = "linename",
                     label = "Subway Line Number",
                     choices = unique(linelist)
      ),
      sidebarMenu(
        menuItem("Bar Graph", tabName = "bargraph", icon = icon("th")),
        menuItem("NYC Map", tabName = "nycmap", icon = icon("map")), 
        menuItem("By Borough", tabName = "piechart", icon = icon("circle")),
        menuItem("Original Data", tabName = "orgdata", icon = icon("square")),
        menuItem("Bad Data", tabName = "garbage", icon = icon("square"))
      )
    ),
    
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "bargraph",
                fluidRow(
                  plotOutput("Entry")
                )
        ),
        
        # Second tab content
        tabItem(tabName = "nycmap",
                leafletOutput("mymap", width = "100%", height = 400)
        ),
        
        # Third tab content
        tabItem(tabName = "piechart",
                fluidRow(
                  plotOutput("Pie")
                )
        ), 
        
        # Fourth tab content
        tabItem(tabName = "orgdata",
                fluidRow(
                 dataTableOutput("rawdata")
                )
        ),
        
        # Fifth tab content
        tabItem(tabName = "garbage",
                fluidRow(
                  dataTableOutput("bad")
                )
        )
      )
    )
  )
)