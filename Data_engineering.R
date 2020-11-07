## setting working directory 
setwd("~/C:/Users/glane/Downloads")
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(timedate)
library(tidyverse)
library(pastecs)
library(gapminder)

### Correcting parsing errors while reading data #prevention
### Matching and removal of duplicates done in SQL through grouping


weatherdata <- read_delim('Weatherdata1.csv', ';', 
           escape_double = FALSE, 
           col_types = cols(Date = col_date(format = "%d/%m/%y")), 
           trim_ws = TRUE)

view(weatherdata)


Wehkampdata <- read_delim("Wehkamp_source.csv", ";", 
           escape_double = FALSE, col_types = cols(sex = col_factor(levels = c("F", 
           "M")), session_date = col_date(format = "%d/%m/%Y"), 
           items = col_integer(), delivered = col_integer(), 
           returned = col_integer(), free_shipping = col_integer(), 
           weekend = col_integer(), Vacation = col_integer(), 
           Corona_lockdown = col_integer(), 
           Competitor_sales = col_integer()), 
           locale = locale(decimal_mark = ","), 
           trim_ws = TRUE)
View(Wehkampdata)


wehkamp_weather_data_all <- merge(Wehkampdata, weatherdata, by.x='session_date', by.y='Date', Header = TRUE, label = TRUE)

wehkamp_weather_data_all$weekday = wday(wehkamp_weather_data_all$session_date, 
           label=TRUE)
           view(wehkamp_weather_data_all)
View(wehkamp_weather_data_all)
WW <- wehkamp_weather_data_all

      
WW<-na.omit(WW)
View(WW)
WW$sex <- as.integer(WW$sex == "M")
View(WW)
WW<- select(WW ,conversion_rate,Sunshine_hours, Sunshine_percent,Precipitation_hours,Precipitation_sum,sex,weekend,Vacation,Corona_lockdown,Competitor_sales,free_shipping,total_price_before_discount)
View(WW)
#T-Test
t.test(WW$Avg_temp , WW$conversion_rate)
t.test(WW$Sunshine_hours , WW$conversion_rate)
t.test(WW$Sunshine_percent , WW$conversion_rate)
t.test(WW$Precipitation_hours , WW$conversion_rate)
t.test(WW$Precipitation_sum , WW$conversion_rate)
t.test(WW$weekend ,WW$conversion_rate)
t.test(WW$Vacation ,WW$conversion_rate)
t.test(WW$Corona_lockdown ,WW$conversion_rate)
t.test(WW$Competitor_sales , WW$conversion_rate)
t.test(WW$free_shipping , WW$conversion_rate)
t.test(WW$total_price_before_discount , WW$conversion_rate)
t.test(WW$sex , WW$conversion_rate)


