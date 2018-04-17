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