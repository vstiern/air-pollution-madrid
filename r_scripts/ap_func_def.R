### Functions

## Data Ingestion
# 1. Function to extract year and month from file name and insert new columns
read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$year <- paste0(20, substring(filename, 28,29)) #year
  ret$month <- ifelse(nchar(filename)>35, substring(filename, 31,32), paste0(0, substring(filename, 31,31))) #month
  ret$date <- ymd(paste(ret$year, ret$month, ret$day, sep = "-")) #date
  ret$year <- NULL
  ret$month <- NULL
  ret$day <- NULL
  ret
}

## Data transformation
# 1. Function to flatten matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}

# 2. Function to convert a vector into linear regression formula
regFormula <- function(depVar,exVar) 
{
  t1 <- paste(depVar, '~', sep = "")
  t2 <- paste(exVar[1:length(exVar)], collapse = "+")
  f <- as.formula(paste(t1,t2, sep = ""))
  return(f)
}


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