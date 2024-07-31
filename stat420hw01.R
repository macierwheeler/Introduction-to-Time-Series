library(tidyverse)
library(readr)
library(ggplot2)

### Question 1
## Part 1
nike <- read.csv("/Users/maciewheeler/Downloads/NKE_2006-01-01_to_2018-01-01.csv")
nike$Date <- as.Date(nike$Date)

p1 <- ggplot(nike, aes(x=Date, y=Close)) + geom_line() + 
  labs(x='Date', y='Close Price', title='NKE Close Prices From 2006-01-01 to 2018-01-01')
p1

## Part 2
covid <- read.csv("/Users/maciewheeler/Downloads/case_time_series.csv")
covid$Date <- paste("2020-", covid$Date, sep = "")
covid$Date <- as.Date(covid$Date, "%Y-%d-%b")

p2 <- ggplot(covid, aes(x=Date, y=Daily.Confirmed)) + geom_line() + 
  labs(x='Date', y='Daily Confirmed COVID-19 Cases', 
       title='Daily Confirmed COVID-19 Cases in India From 2020-01-30 to 2020-05-01')
p2

### Question 2
library(readxl)
library(reshape)

## Part 1
unemployment <- read_excel("/Users/maciewheeler/Downloads/unemployment.xlsx")
unemployment_melted <- melt(unemployment, id = c('Jan', 'Feb', 'Mar', 'Apr', 
                                                 'May', 'Jun', 'Jul', 'Aug', 
                                                 'Sep', 'Oct', 'Nov', 'Dec'))

unemployment_melted %>% pivot_longer(1:12, names_to='Month', 
                                     values_to='Unemployment_Rate') %>% 
  ggplot(aes(x=value, y=Unemployment_Rate, color=Month)) + geom_line() + 
  labs(x='Year', y='Unemplyoment Rate', title='Unemplyment Rates in the US Since 2000')

## Part 2
unemployment <- as.data.frame(unemployment)

unemployment_melted2 <- melt(unemployment, id = 'Year')

unemployment_melted2$Date <- paste(unemployment_melted2$variable, "01")
unemployment_melted2$Date <- paste(unemployment_melted2$Date, unemployment_melted2$Year)
unemployment_melted2$Date <- as.Date(unemployment_melted2$Date, format = "%b %d %Y")

ggplot(unemployment_melted2, aes(x=Date, y=value)) + geom_line() + 
  labs(y='Unemployment Rate', title='Monthly Unemployment Rate From Jan 2000 to Dec 2021')

### Question 3
GOOGL <- read_csv('/Users/maciewheeler/Downloads/GOOGL.csv')
GOOGL$date = with_tz(GOOGL$date, "America/New_York")
GOOGL$date15 <- cut(GOOGL$date, breaks='15 min')

GOOGL.summary <- aggregate(open ~ date15, FUN = mean, data = GOOGL)
GOOGL.summary$period = format(as.POSIXct(GOOGL.summary$date15), format="%H:%M")

p3 <- ggplot(GOOGL.summary, aes(x=period, y=open, group=1)) + geom_line() +
  labs(x='Period', y='Average Opening Price', title='Average GOOGL Opening Prices in 15-minute Periods')
p3
