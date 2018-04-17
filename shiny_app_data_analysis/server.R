# Import libraries
library(data.table) 
library(ggplot2)
library(dygraphs)
library(shiny)
library(corrplot)
library(lubridate)
library(Hmisc)
library(reshape2)

# Import data environment
daily_dt <- readRDS(file = "data/daily_dt.RDS")
N02_predictions <- readRDS(file = "data/N02_predictions.RDS")
# daily_station_dt <- readRDS(file = "data/daily_station_dt.RDS")
parameter_info <- readRDS(file = "data/parameter_info.RDS")

### Functions
## Plots - Descriptive Analysis
# 1. Multiplot histogram for each parameter 
hp_func <- function(vars, binnr) {
  dt_sel <- melt(daily_dt[, vars, with = F])
  
  ggplot(data = melt(dt_sel), mapping = aes(x = value, colour = variable)) + 
    geom_histogram(bins = binnr, show.legend = FALSE) + facet_wrap(~variable, scales = 'free_x') +
    labs(y = "Frequency", x = "", 
         caption = "Unit in parameter_info: microg/m^3 or millig/m3")
}

# 2. Boxplot Function
bp_func <- function(parameter, wday = TRUE) {

  if(wday == TRUE) {
    ggplot(data = daily_dt, aes(x = wday(date, label = T), y = get(parameter), fill = wday(date, label = T))) +
      geom_boxplot(outlier.alpha = 0.5) +
      ggtitle(paste(parameter, "by Weekday", sep = " ")) +
      labs(x = "", y = parameter_info[param_Form == parameter, param_unit]) +
      theme(legend.position ="none")
  }
  else {
    ggplot(data = daily_dt, aes(x = month(date, label = T), y = get(parameter), fill = month(date, label = T))) +
      geom_boxplot(outlier.alpha = 0.5) +
      ggtitle(paste(parameter, "by Month", sep = " ")) +
      labs(x = "", y = parameter_info[param_Form == parameter, param_unit]) +
      theme(legend.position="none")
  }
}

# 3. Scatterplot function
sp_func <- function(var1, var2, line) {
  ggplot(daily_dt, aes(x = get(var1), y = get(var2))) +
    geom_smooth(method = paste(line), se=F) +
    geom_jitter(width = .5, size = 1) +
    labs(x = paste(var1, parameter_info[param_Form == var1, param_unit]), y = paste(var2, parameter_info[param_Form == var2, param_unit]))
}

# 4. Function for time series graph
tsp_func <- function(var1, m = T, s = T) {
  mean <- as.numeric(lapply(daily_dt[, var1, with = F], mean, na.rm = T))
  std <- mean - as.numeric(lapply(daily_dt[, var1, with = F], sd, na.rm = T))

  dygraph(daily_dt[, c("date", var1), with = F], main = paste("Avg daily value of", var1, sep = ": ")) %>%
    dyAxis("y", label = paste(parameter_info[param_Form == var1, param_unit])) %>%
    dyLimit(ifelse(m == T, mean, NA), color = "darkgreen", label = "Mean", labelLoc = "left") %>%
    dyShading(from = mean - std, to = mean + std, color = ifelse(s == T, "lightgrey", "white"), axis = "y") %>%
    dyRangeSelector()
}

### Shiny App
# Define server logic
shinyServer(function(input, output) {
   
  # 1. define histogram ouput
  output$histoplot = renderPlot({hp_func(input$hist_var, input$binnr)})
  
  # 2. define boxplot output
  output$boxplot = renderPlot({bp_func(input$box_var, input$box_period)})

  # 3. define scatterplot output
  output$scatterplot = renderPlot({sp_func(input$scat_var1, input$scat_var2, input$scat_line)})
  # 
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
# close server side
})  