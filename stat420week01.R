knitr::opts_chunk$set(echo = TRUE)
library(tidyquant)
library(tidyverse)

library(readr)
#library(lubridate)
GOOGL <- read.csv("/Users/maciewheeler/Downloads/GOOGL.csv")
# changes the date column to a specific timezone
GOOGL$date=with_tz(GOOGL$date, "America/New_York")
head(GOOGL)

## lubridate examples
# time = GOOGL$date
time=pull(GOOGL, date)
time[1:5]
year(time[1:5])
month(time[1:5], label=T)
day(time[1:5])

wday(time[1:5])
wday(time[1:5], label = T)
hour(time[1:5])
minute(time[1:5])

## parsing of date-times
# functions: ymd, ymd_hms, dmy, dmy_hms, mdy, ...
ymd(20201215)
mdy("4/1/21")

## dealing with time zone
# with_tz(): when time zone is UTC
# force_tz(): when time zone is something other than UTC
time = ymd_hms("2020-08-20 15:30:30")
with_tz(time, "America/New_York")
force_tz(time, 'America/New_York')

## extract a subset of time series
# to extract the data in the afternoon trading
filter(GOOGL, date > '2021-08-20 12:00')

## plot time series
# recommend ggplot in the ggplot2 library
p1=ggplot(GOOGL, aes(x=date, y=close))+
  geom_line() + labs(x='time', y="close price")
p1+labs(title='GOOGL close price on 8/20')

# plot multiple time series together
GOOGL %>% pivot_longer(3:6, names_to='type', 
                       values_to='value') %>% 
  ggplot(aes(x=date, y=value, color=type))+geom_line()

GOOGL %>% ggplot(aes(x=date, y=close))+geom_line()

GOOGL %>% filter(date>'2021-08-20 12:00') %>% ggplot(aes(x=date, y=close))+geom_line()

# the pipe %>% provides an efficient way for manipulating and plotting data
# data %>% function(...) is equivalent to function(data, ...)

## a note on pivot_longer
# i use a smaller data set to show what the function pivot_longer does, it makes 
# the dataset longer
# the following pivot_longer command put columns 3 to 6 into one column
x = GOOGL[1:3,]
y = pivot_longer(x, 3:6, names_to='type', values_to='price')
y[1:8,]

## breaks and labels for the x-axis
# in the previous plot, we like the breaks to be at every one hour and label it
# first, we add two columns to the data
p1=GOOGL %>% mutate(hour=hour(date), minute=minute(date), index=row_number()) %>% ggplot(aes(x=index, y=close))+
  geom_line()
maj_breaks=which(minute(GOOGL$date)==0)
hours=hour(GOOGL$date[maj_breaks])
p1+
  scale_x_continuous(breaks=maj_breaks, labels = hours)+
  labs(x='time', y='close price', title='GOOGL price movement')

# a more efficient way to modify the breaks and labels
p1 = GOOGL %>% mutate(hour=hour(date), minute=minute(date), index=row_number()) %>% ggplot(aes(x=index, y=close))+
  geom_line()
breaks = p1$data %>% filter(minute==0) %>% select(hour, index)
p1+scale_x_continuous(breaks=breaks$index, labels = breaks$hour)+
  labs(x='time', y='close price')