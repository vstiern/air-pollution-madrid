## Master ShinyApp
shinyApp(
  # option settings
  options = list(width = "100%", height = 700),
  # define ui
  ui <- shinyUI(
    fluidPage(
      # define tabset panel
      tabsetPanel(
        
        # 1. define first tab 'histogram'
        tabPanel('Histogram',
                 # define sidebar laybout
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       # define slider for nr of bins for histogram
                       sliderInput("binnr", label = "Select Nr of Bins:",
                                   min = 5, max = 30, step = 5, value = 10),
                       # define checkbox input for variable selection
                       checkboxGroupInput("hist_var", label = "Select Variables:",
                                          choices = c(colnames(daily_dt[, 2:19])),
                                          selected = c(colnames(daily_dt[, 2]))))
                   ),
                   # define main panel
                   mainPanel(titlePanel("Histogram"),
                             # define output option and name         
                             plotOutput("histoplot", width = "100%", height = 500))
                 )
        ),
        
        # 2. define second tab 'boxplot'
        tabPanel('Boxplot',
                 # define topbar layout
                 fluidRow(
                   # define variable input selector
                   column(width = 3,
                          selectInput("box_var", label = "Select Variables:",
                                      choices = colnames(daily_dt[, 2:19]), selected = "SO2")
                   ),
                   # define weekday or month input selector
                   column(width = 3,
                          radioButtons("box_period", label = "Select Weekday or Month:",
                                       choices = c("Weekday" = T, "Month" = F), selected = T))
                 ),
                 # define main panel
                 mainPanel(titlePanel("Boxplot"),
                           # define output option and name          
                           plotOutput("boxplot", width = "100%", height = 500))
        ),
        
        # 3. define third tab 'scatterplot'
        tabPanel('Scatterplot',
                 # define topbar layout
                 fluidRow(
                   # define variable x-axis input selector
                   column(width = 3,
                          selectInput("scat_var1", label = "Select X-Axis:",
                                      choices = as.factor(colnames(wide_dt[,4:21])), selected = "SO2")),
                   # define variable y-axis input selector
                   column(width = 3,
                          selectInput("scat_var2", label = "Select Y-Axis:",
                                      choices = as.factor(colnames(wide_dt[,4:21])), selected = "NO2")),
                   # define line option 'radio' button input selector
                   column(width = 5,
                          radioButtons("scat_line", label = "", inline = TRUE,
                                       choices = c("None" = "", "Smooth Line" = "loess", "LM" = "lm"),
                                       selected = ""))
                 ),
                 # define main panel
                 mainPanel(titlePanel("Scatterplot"),
                           # define output option and name          
                           plotOutput("scatterplot", width = "100%", height = 500))
        ),
        
        # 4. define fourth tab 'time series graph'
        tabPanel('Time Series',
                 # define topbar layout
                 fluidRow(
                   # define variable input selector
                   column(width = 3,
                          selectInput("ts_var", label = "Select Variable:",
                                      choices = colnames(daily_dt[,2:19]), selected = "SO2")),
                   # define graph option 'mean'
                   column(width = 1,
                          checkboxInput("ts_mean", label = "Mean", value = F)),
                   # define graph option 'standard deviation'
                   column(width = 3,
                          checkboxInput("ts_std", label = "Mean +/- Std", value = F))
                 ),
                 # define main panel
                 mainPanel(titlePanel("Time Series"),
                           # define output option and name          
                           dygraphOutput("timeseriesplot", width = "100%", height = 500))
        ),
        
        # 5. define fifth tab 'Correlogram'
        tabPanel('Correlogram',
                 # define sidebar layout
                 sidebarLayout(
                   sidebarPanel(
                     # define radio buttion input for graph options
                     radioButtons("cor_option", label = "Select Addition:",
                                  choices = c("None", "Significance Level" = "sig",
                                              "Correlation Coefficients" = "coeff"), selected = "None"),
                     # define checkbox variable input selector
                     checkboxGroupInput("cor_var", label = "Select Variables:", choices = c(colnames(daily_dt[, 2:19])),
                                        selected = c(colnames(daily_dt[, 2])))
                   ),
                   # define main panel 
                   mainPanel(titlePanel("Correlogram"),
                             # define output option and name
                             plotOutput("correplot", width = "100%", height = 500))
                 )
        ),
        
        # 6. define sixth tab 'NO2 Forecast'
        tabPanel('NO2 Forecast',            
                 # define topbar layout
                 fluidRow(
                   # define threshold level selector
                   column(width = 3, helpText("Select NO2 Threshold"),
                          selectInput("threshold", label = "Threshold Level",
                                      choices = seq(50, 100, by= 5)), selected = 70)
                 ),
                 # define main panel
                 mainPanel(titlePanel("NO2 Forecast"),
                  # define output option and name
                  dygraphOutput("thresholdplot", width = "100%", height = 500))
        ),
        
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
                   mainPanel(
                     # define 
                     leafletOutput("mapplot", width = "100%", height = 600))    
                )
        )
      )
    )
  ),
  
  # define server side
  server <- shinyServer(function(input,output) {
    
    # 1. define histogram ouput
    output$histoplot = renderPlot({hp_func(input$hist_var, input$binnr)})
    
    # 2. define boxplot output
    output$boxplot = renderPlot({bp_func(input$box_var, input$box_period)})
    
    # 3. define scatterplot output
    output$scatterplot = renderPlot({sp_func(input$scat_var1, input$scat_var2, input$scat_line)})
    
    # 4. define timeseries output
    output$timeseriesplot = renderDygraph({tsp_func(input$ts_var, input$ts_mean, input$ts_std)})
    
    # 5. define correlogram output
    output$correplot = renderPlot({
      # define input and correlation of selected input
      dt_sel <- daily_dt[, input$cor_var, with = F]
      dt_cor_mat <- rcorr(as.matrix(dt_sel))
      # output with signifigance test
      if(input$cor_option == "sig") {
        corrplot(cor(dt_sel), p.mat = dt_cor_mat$P, method = "color", type = "lower", tl.col = "black",
                 tl.srt = 45, order = "alphabet", sig.level = c(.001, .01, .05), pch.cex = 10/ncol(dt_sel),
                 insig = "label_sig", pch.col = "grey")
      } 
      # output with correlations coeffecients 
      if(input$cor_option == "coeff") {
        corrplot(cor(dt_sel), method = "color", type = "lower", tl.col = "black", tl.srt = 45, order = "alphabet",
                 addCoef.col = "grey", number.cex = 7/ncol(dt_sel))  
      }
      # output with no additions
      if(input$cor_option == "None")
        corrplot(cor(dt_sel), method = "color", type = "lower", tl.col = "black", tl.srt = 45, order = "alphabet")
    })
    
    # 6. define NO2 forecast 
    output$thresholdplot = renderDygraph({
      
      NO2_threshold <- as.numeric(input$threshold)
      flaged_dates <- N02_predictions[(NO2_Fcst - NO2_threshold) > 0, date]
      
      dygraph(N02_predictions, main = "NO2 Forecast") %>% 
        dySeries( "NO2_Fcst", label = "NO2 Level") %>%
        dyAxis("y", label = paste(parameter_info[param_Form == "NO2", param_unit])) %>%
        dyEvent(flaged_dates, color = 'red') %>%
        dyRangeSelector() %>%
        dyOptions(colors = 'cornflowerblue')
    })
    
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
# close shiny app
)
