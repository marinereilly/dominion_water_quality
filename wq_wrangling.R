#####Calculating Averages, cumulative totals etc for plotting#####
###You must already have the wq_2017 and hobo files###
###packages###
library(tidyverse)
library(lubridate)

#####Salinity Averages#####
wq_2017$Month<-month(wq_2017$DateTime)
wq_2017$Day<-day(wq_2017$DateTime)
wq_2017$Year<-year(wq_2017$DateTime)
wq_2017$hour<-hour(wq_2017$DateTime)
wq_2017$yday<-yday(wq_2017$DateTime)

hobo$Month<-month(hobo$date_time)
hobo$Day<-day(hobo$date_time)
hobo$Year<-year(hobo$date_time)
hobo$hour<-hour(hobo$date_time)
hobo$yday<-yday(hobo$date_time)

salinity_hour<-wq_2017 %>% 
  group_by(Station, Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(Salinity))
salinity_hour$date_time<- paste(salinity_hour$Year,"-", 
                                salinity_hour$Month, "-", salinity_hour$Day, " ",
                                salinity_hour$hour, sep = "")
salinity_hour$date_time<-ymd_h(salinity_hour$date_time)

salinity_day<-wq_2017 %>% 
  group_by(Station, Year, Month, Day) %>% 
  summarise(daily_av=mean(Salinity))
salinity_day$date<- paste(salinity_day$Year, 
                     salinity_day$Month, salinity_day$Day, sep = "-")
salinity_day$date<-ymd(salinity_day$date)

salinity_hour_hobo<-hobo %>% 
  group_by(station, Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(salinity)) %>% 
  rename(Station="station")

salinity_hour_hobo$date_time<- paste(salinity_hour_hobo$Year,"-", 
                                salinity_hour_hobo$Month, "-", salinity_hour_hobo$Day, " ",
                                salinity_hour_hobo$hour, sep = "")
salinity_hour_hobo$date_time<-ymd_h(salinity_hour_hobo$date_time)

salinity_day_hobo<-hobo %>% 
  group_by(station, Year, Month, Day) %>% 
  summarise(daily_av=mean(salinity)) %>% 
  rename(Station="station")
salinity_day_hobo$date<- paste(salinity_day_hobo$Year, 
                          salinity_day_hobo$Month, salinity_day_hobo$Day, sep = "-")
salinity_day_hobo$date<-ymd(salinity_day_hobo$date)
#You need this step because anything with a geom_line that doesn't have points missing connects them
#this step adds NA points for each day so they don't connect
date<-seq(as.Date("2017-01-01"),as.Date("2017-12-31"),1)
Station<-"North"
Northdays<-data_frame(date, Station)
Station<-"Mid"
Midays<-data_frame(date, Station)
Station<-"South"
Southdays<-data_frame(date, Station)
salinity_day2<-Northdays %>% 
  full_join(., salinity_day) %>% 
  full_join(., Midays) %>% 
  full_join(., Southdays)


date<-seq(as.Date("2017-01-01"),as.Date("2017-12-31"),1)
Station<-"Hobo"
alldays<-data_frame(date, Station)
salinity_day_hobo2<-alldays %>% 
  full_join(., salinity_day_hobo) 

#####Water Level Averages#####
wl_hour<-wq_2017 %>% 
  group_by(Station, Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(Depth))
wl_hour$date_time<- paste(wl_hour$Year,"-", 
                          wl_hour$Month, "-", wl_hour$Day, " ",
                          wl_hour$hour, sep = "")
wl_hour$date_time<-ymd_h(wl_hour$date_time)

wl_day<-wq_2017 %>% 
  group_by(Station, Year, Month, Day) %>% 
  summarise(daily_av=mean(Depth))
wl_day$date<- paste(wl_day$Year, 
                    wl_day$Month, wl_day$Day, sep = "-")
wl_day$date<-ymd(wl_day$date)

#####Temp Averages#####
temp_hour<-wq_2017 %>% 
  group_by(Station, Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(Temp))
temp_hour$date_time<- paste(temp_hour$Year,"-", 
                            temp_hour$Month, "-", temp_hour$Day, " ",
                          temp_hour$hour, sep = "")
temp_hour$date_time<-ymd_h(temp_hour$date_time)

temp_hour_all<-wq_2017 %>% 
  group_by(Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(Temp))
temp_hour_all$date_time<- paste(temp_hour_all$Year,"-", 
                            temp_hour_all$Month, "-", temp_hour_all$Day, " ",
                            temp_hour_all$hour, sep = "")
temp_hour_all$date_time<-ymd_h(temp_hour_all$date_time)

temp_day<-wq_2017 %>% 
  group_by(Station, Year, Month, Day) %>% 
  summarise(daily_av=mean(Temp))
temp_day$date<- paste(temp_day$Year, 
                      temp_day$Month, temp_day$Day, sep = "-")
temp_day$date<-ymd(temp_day$date)

#####DO Averages#####
do_hour<-wq_2017 %>% 
  group_by(Station, Year, Month, Day, hour) %>% 
  summarise(hourly_av=mean(ODO_Conc))
do_hour$date_time<- paste(do_hour$Year,"-", 
                          do_hour$Month, "-", 
                          do_hour$Day, " ",
                          do_hour$hour, sep = "")
do_hour$date_time<-ymd_h(do_hour$date_time)

do_day<-wq_2017 %>% 
  group_by(Station, Year, Month, Day) %>% 
  summarise(daily_av=mean(ODO_Conc))
do_day$date<- paste(do_day$Year, 
                    do_day$Month, 
                    do_day$Day, sep = "-")
do_day$date<-ymd(do_day$date)
#####Hypoxia times#####
hypoxia<-wq_2017 %>% 
  filter(., ODO_Conc<= 2)
