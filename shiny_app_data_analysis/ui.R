# Import libraries
library(data.table) 
library(shiny)

# Import data environment
daily_dt <- readRDS(file = "data/daily_dt.RDS")
parameter_info <- readRDS(file = "data/parameter_info.RDS")

# ShinyApp
# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
                                  choices = as.factor(colnames(daily_dt[,2:19])), selected = "SO2")),
               # define variable y-axis input selector
               column(width = 3,
                      selectInput("scat_var2", label = "Select Y-Axis:",
                                  choices = as.factor(colnames(daily_dt[,2:19])), selected = "NO2")),
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
                                    selected = c(colnames(daily_dt[, 2]), colnames(daily_dt[, 6]),
                                                 colnames(daily_dt[, 10]), colnames(daily_dt[, 15])))
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
      )
    )
  )
)