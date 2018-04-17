# Import libraries
library(data.table) 
library(ggplot2)
library(shiny)
library(leaflet)
library(lubridate)

# Import data environment
daily_dt <- readRDS(file = "data/daily_dt.RDS")
daily_station_dt <- readRDS(file = "data/daily_station_dt.RDS")
parameter_info <- readRDS(file = "data/parameter_info.RDS")

### Shiny App
# Define server logic
shinyServer(function(input, output) {

  # 7. define heat map output
  output$mapplot = renderLeaflet({

    # input parameters
    setHour <- input$map_hour
    setMonth <- input$map_month
    startYear <- input$map_start
    endYear <- input$map_end
    parameter <- input$map_var

    # subset df as per input parameters
    tmp_dt <- daily_station_dt[(year(date) >= startYear) & (year(date) <= endYear)
                               & month(date) == setMonth & hour == setHour,
                               sapply(.SD, mean, na.rm = TRUE), .SDcols = parameter,
                               by = c('station', 'station_loc', 'Lng', 'Lat')]

    # Create a color palette with defined bins
    mypalette = colorBin(palette = "YlOrBr", domain = tmp_dt$V1, na.color="black", bins = 5)

    # Define text
    mytext = paste("Stations: ", tmp_dt$station_loc,
                   "<br/>", "Magnitude: ", tmp_dt$V1,
                   sep = "") %>% lapply(htmltools::HTML)
    # plot
    leaflet(data = tmp_dt) %>% addTiles() %>%
      # set map boundaries
      fitBounds(min(tmp_dt$Lng), min(tmp_dt$Lat),
                max(daily_station_dt$Lng), max(daily_station_dt$Lat)) %>%
      # define map options
      addProviderTiles("Esri.WorldImagery") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$Stamen.TonerLabels) %>%
      addProviderTiles(providers$Stamen.TonerLines, options = providerTileOptions(opacity = 0.35))  %>%
      # define marker options
      addCircleMarkers(~Lng, ~Lat, fillColor = ~mypalette(V1), fillOpacity = 0.7, color = "white",
                       radius = 8, stroke=FALSE, label = mytext,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "13px", direction = "auto")) %>%
      # define legend options
      addLegend(pal = mypalette, values= ~V1, opacity=0.9, title = parameter_info[param_Form == parameter, param_unit],
                position = "bottomright" )
    })
# close server side
})  