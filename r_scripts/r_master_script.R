### 1. Source Scripts
source("ap_libraries.R")
source("ap_func_def.R")
# source("ap_shiny_apps.R") 

### 2. Data Ingestion
# Put air pollution filenames into list.
files <- list.files(path = "workgroup_data", pattern = ".csv", full.names = TRUE)

# Ingest air pollution data
dt <- data.table(ldply(files, read_csv_filename))

# Ingest weather data
wth_data <- data.table(read_xlsx("weather.xlsx"))
wth_data$date <- ymd(wth_data$date)

# Merge weather and air pollution data
dt <- merge(dt, wth_data, by = "date", all.x = TRUE)

# Ingest parameter information
parameter_info <- data.table(read.csv("parameters.csv"))

# Add parameter notation to air pollution weather data -> final data table
dt <- merge(x = dt, y = parameter_info[ , c("param_ID", "param_Form")],
            by.x = "parameter", by.y = "param_ID", all.x = TRUE)

# Remove redunant variable
dt$parameter <- NULL

# Ingest station information
station_info <- data.table(read.csv("stations.csv"))

### 3. Data Transformation
# Assume NAs are missing data and remove all associated rows.
dt <- dt[complete.cases(dt), ]

# Cast each parameter to induvidual column
wide_dt <- dcast(dt, date + hour + station + temp_avg + temp_max + temp_min + precipitation +
                 humidity + wind_avg_speed ~ param_Form, value.var = 'value')

# Daily Averages per parameter (mean of all hours per date and station)
daily_station_dt <- wide_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = names(wide_dt[, 4:21]),
                            by = c('date', 'hour', 'station')]

# Merge with station_info to get lat and long
daily_station_dt <- merge(x = daily_station_dt, y = station_info,
                          by.x = "station", by.y = "station_ID", all.x = TRUE)

# Daily Averages per parameter (mean of all hours and stations)
daily_dt <- wide_dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = names(wide_dt[, 4:21]), by = date]


### 4. Descriptive Analysis -> Shiny Apps
# Summary of parameters
dt[, as.list(summary(value)), by = param_Form]


### 5. Regression Analysis - Data Sets 
reg_dt <- daily_dt[, 2:19]

# Explantory variables for NO2 Regression
NO2_ExVars <- c('SO2','CO','PM2.5','EBE', 'temp_avg', 'wind_avg_speed')

# 2011-2015 chosen as training data, 2016 as testing data
trainSet <- daily_dt[year(date) < 2016, ]
testSet <- daily_dt[year(date) == 2016, ]

# Regression Model
NO2Reg <- lm(regFormula('NO2', NO2_ExVars), trainSet)
# summary(NO2Reg)

# Initial Diagnostic Plot
par(mfrow=c(2, 2))
par(mar = rep(2, 4))
# plot(NO2Reg)

# TrainSet: Actual vs Fitted values for model
fitted_val <- NO2Reg$fitted
actual_data <- trainSet[, c("date", "NO2")]
a_vs_f <- cbind(actual_data, fitted_val)
# a_vs_f <- a_vs_f[, lapply(.SD, mean), .SDcols = c("NO2", "fitted_val"), by = date]

# plot  
dygraph(a_vs_f, main = "Actual vs Fitted: Training Set") %>% 
  dySeries( "NO2", label = "Actual") %>%
  dySeries( "fitted_val" , label = "Fitted") %>% 
  dyAxis("y", label = paste(parameter_info[param_Form == "NO2", param_unit])) %>%
  dyRangeSelector() 

# TestSet: Actual vs Predicted values for model3
predicted_val <- predict(NO2Reg, newdata = testSet[, NO2_ExVars, with = F])
act_data <- testSet[, c("date", "NO2")]
a_vs_p <- cbind(act_data, predicted_val)

# plot
dygraph(a_vs_p, main = "Actual vs Predicted: Testing Set") %>% 
  dySeries( "NO2", label = "Actual") %>%
  dySeries( "predicted_val" , label = "Predicted") %>% 
  dyAxis("y", label = paste(parameter_info[param_Form == "NO2", param_unit])) %>%
  dyRangeSelector() 


### 6. Predicting Future NO2 levels for 2017-2018
# Time series with daily seasonality for NO2 regressors
ts_ExVars <- msts(daily_dt[, NO2_ExVars, with=F], start=2011, seasonal.periods = c(7, 365.25))

# Define empty lists to hold forecasted values 
tmp_fc_ExVars <- list()
fc_ExVars <- list()

# Create loop that will produce forecast per regressor using HoltWinters method
# and add to above lists
for (i in 1:ncol(ts_ExVars)){
  tmp_fc_ExVars[[i]] <- forecast(HoltWinters(ts_ExVars[,i]), h = 730)  
  fc_ExVars[[i]] <- as.numeric(tmp_fc_ExVars[[i]]$mean) 
}

# merge date range with forecasted values
fcts_dt <- data.table('date' = seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by="days"), 
                      data.table(do.call(cbind, fc_ExVars)))

fcts_df <- cbind(seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by="days"),
                 data.frame(do.call(cbind, fc_ExVars)))

# define variable names
names(fcts_df)[1] <- 'date'
names(fcts_df)[2:7] <- NO2_ExVars

# For parameters, convert incorrect negetive values from forecast to
# positive by absolute approximation
fcts_df[, c(2:5)] <- abs(fcts_df[, c(2:5)])

# Create dt with forecast range and NO2 predictions using LM with forecasted regressors
N02_predictions <- data.table('date' = fcts_df[,'date'], 'NO2_Fcst' = predict(NO2Reg, newdata = fcts_df[, c(NO2_ExVars)]))

# use shiny app to show forecast together with warning threshold
# plot
# dygraph(N02_predictions, main = "NO2 Forecast 2017-2018") %>% 
#  dySeries( "NO2_Fcst", label = "NO2 Level") %>%
#  dyAxis("y", label = paste(parameter_info[param_Form == "NO2", param_unit])) %>%
#  dyRangeSelector() 




