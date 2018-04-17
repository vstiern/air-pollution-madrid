# Import libraries
library(data.table) 
library(shiny)
library(leaflet)

# Import data environment
daily_dt <- readRDS(file = "data/daily_dt.RDS")
daily_station_dt <- readRDS(file = "data/daily_station_dt.RDS")
parameter_info <- readRDS(file = "data/parameter_info.RDS")

# ShinyApp
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # define tabset panel
  tabsetPanel(

    # 7. define seventh tab 'Spatial Heat Map for Madrid'
    tabPanel('Heat Map',
             # define sidebar layout
             sidebarLayout(
               # define sidebar panel
               sidebarPanel(
                 # define variable input selector
                 selectInput("map_var", label = "Select Variable",
                             choices = names(daily_dt)[2:19], selected = "SO2"),
                 # define hour input selector
                 selectInput("map_hour", label = "Select Hour",
                             choices = c(seq(1,24)), selected = 12),
                 # define month input selector
                 selectInput("map_month", label = "Select Month",
                             choices = c(seq(1,12)), selected = 6),
                 # define start year input selector
                 selectInput("map_start", label = "Select Start Year",
                             choices = c(seq(2011,2016)), selected = 2011),
                 # define end year input selector
                 selectInput("map_end", label = "Select End Year",
                             choices = c(seq(2011,2016)), selected = 2016)
               ),
               # define main panel
               mainPanel(titlePanel("Averages for selected hour/month/year"),
                 # define output and name
                 leafletOutput("mapplot", width = "100%", height = 600))
             )
      )
    )
  )
)