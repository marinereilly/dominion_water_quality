#####Packages#####
library(tidyverse)
library(dadjoke)
library(lubridate)

#####Load data or data from Join all data script#####
wq_all<-read.csv("Raw YSI Data/wq_2010_2017.csv")
kayak<-read.csv("kayak.csv")
kayak$Date<-mdy(kayak$Date)
kayak$DateTime<-paste(kayak$Date, "12:00", sep = " ")
kayak$DateTime<-ymd_hm(kayak$DateTime)

wq_all$DateTime<-ymd_hms(wq_all$DateTime)
wq_all$Year<-as.factor(wq_all$Year)
wq_all$Month<-as.factor(wq_all$Month)
wq_all$Day<-day(wq_all$DateTime)
wq_all$YDay<-as.factor(wq_all$YDay)
wq_all$Hour<-as.factor(wq_all$Hour)
#Use this instead of ydays because it keeps it in date format for plotting
#just make sure that the YEAR column exists as a factor/that you don't delete
#the DateTime column
wq_all$days<-as.Date(format(wq_all$DateTime,"%d-%m-2007"),format="%d-%m-%y")
###Factor order for stations and Remove NA from year###
wq_all$Station<-factor(wq_all$Station, levels = c("North", "Mid", "South"))

#####Daily and Hourly Averages#####
daily_av_2010_2017<-wq_all %>% 
  group_by(Station, Year, days) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(Mean="mean", Sd="sd"))
daily_av_2010_2017$Month<-month(daily_av_2010_2017$days)
daily_av_2010_2017$day<-day(daily_av_2010_2017$days)
daily_av_2010_2017$date<-paste0(daily_av_2010_2017$Month,"-",daily_av_2010_2017$day,"-",daily_av_2010_2017$Year)
daily_av_2010_2017$date<-mdy(daily_av_2010_2017$date)

hourly_av_2010_2017<-wq_all %>% 
  group_by(Station, Year, days, Hour) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(Mean="mean", Sd="sd"))
hourly_av_2017<-hourly_av_2010_2017 %>% 
  filter(Year==2017)
daily_av_2017<- daily_av_2010_2017 %>% 
  filter(Year==2017)

salinity_hour_2017<-wq_all %>% 
  filter(Year==2017) %>% 
  group_by(Month, Day, Hour, Station) %>% 
  summarise(salinity=mean(Salinity))
salinity_hour_2017$Date.Time<-paste0(salinity_hour_2017$Month,"-", salinity_hour_2017$Day, "-2017", " ", salinity_hour_2017$Hour, ":00", sep="")
salinity_hour_2017$Date.Time<-mdy_hm(salinity_hour_2017$Date.Time)

#####Remove Crazy outliers#####
#There are two Salinity averages above 500 which seems improbable but deleting
#these values likely removes other values so I am going to attempt to remove
#during the plotting process so it isn't permenant

#####Hypoxia#####
hypoxia_all<-wq_all %>% 
  filter(., ODO_Conc<= 2)
