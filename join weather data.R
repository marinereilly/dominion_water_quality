####This Script Describes how to join the 2017 data with the older data files####
#In 2017, YSI switched the system to a new one but wasn't sure when they would actually get it accomplished or how the
#new system would work.  As a result there are several data downloads from the first system and then a bunch from
#the new system that need to be combined and wrangled before the data can be analyzed

####Packages####
library(tidyverse)
library(dadjoke)
library(lubridate)

####Loading all of the different data pieces####
met_2017<-read.csv("Raw YSI Data/MET2017.csv")
groan()
met_2017$DateTime<-ymd_hms(met_2017$DateTime)
#there was a failure of the air temperature sensor on October 18th.  
#all 2017 past that are being removed
met_2017$Air_Temp<- ifelse(met_2017$DateTime >="2017-10-18 00:00:00", NA, met_2017$Air_Temp)


met_2016<-read.csv("Data_2010_2016/MET_2016.csv")
met_2015<-read.csv("Data_2010_2016/MET_2015.csv")
met_2014<-read.csv("Data_2010_2016/MET_2014.csv")
met_2013<-read.csv("Data_2010_2016/MET_2013.csv")
met_2012<-read.csv("Data_2010_2016/MET_2012.csv")
met_2011<-read.csv("Data_2010_2016/MET_2011.csv")
met_2010<-read.csv("Data_2010_2016/MET_2010.csv")

#####formatting everything for joining####
 
  
met_a<-met_2010 %>% 
  bind_rows(., met_2011) %>% 
  bind_rows(., met_2012) %>% 
  bind_rows(., met_2013) %>% 
  bind_rows(., met_2014) %>% 
  bind_rows(., met_2015) %>% 
  bind_rows(., met_2016) %>% 
  rename(Rainfall = "RainFall", Air_Temp = "AirTemp", 
         Wind_Dir = "WindDir", Wind_spd = "WindSpd", 
         Wind_Max = "WindGust", Wind_Min = "WindLull")
met_a$DateTime<-paste(met_a$Date, met_a$Time, sep = " ")  
met_a$DateTime<-mdy_hm(met_a$DateTime)
met_a<-met_a %>% 
  select(-c(Date, Time))

met_all<-met_2017 %>% 
  bind_rows(., met_a)
View(met_all)
write.csv(met_all, "Raw YSI Data/met_all.csv", row.names = FALSE)

remove(met_a)
remove(met_2010)
remove(met_2011)
remove(met_2012)
remove(met_2013)
remove(met_2014)
remove(met_2015)
remove(met_2016)
remove(met_2017)



