####This Script Describes how to join the 2017 data with the older data files####
#In 2017, YSI switched the system to a new one but wasn't sure when they would actually get it accomplished or how the
#new system would work.  As a result there are several data downloads from the first system and then a bunch from
#the new system that need to be combined and wrangled before the data can be analyzed

####Packages####
library(tidyverse)
library(dadjoke)
library(lubridate)

####Loading all of the different data pieces####
wq_2017<-read.csv("Raw YSI Data/wq_2017")
wq_2017<-wq_2017 %>% 
  select(-X)
wq_2017$DateTime<-ymd_hms(wq_2017$DateTime)

####North Station####
n2010<-read.csv("Data_2010_2016/NORTH_2010.csv")
n2011<-read.csv("Data_2010_2016/NORTH_2011.csv")
n2012<-read.csv("Data_2010_2016/NORTH_2012.csv")
n2013<-read.csv("Data_2010_2016/NORTH_2013.csv")
n2014<-read.csv("Data_2010_2016/NORTH_2014.csv")
n2015<-read.csv("Data_2010_2016/NORTH_2015.csv")
n2016<-read.csv("Data_2010_2016/NORTH_2016.csv")

n_all<- n2010 %>% 
  bind_rows(., n2011) %>% 
  bind_rows(., n2012) %>% 
  bind_rows(., n2013) %>% 
  bind_rows(., n2014) %>% 
  bind_rows(., n2015) %>% 
  bind_rows(., n2016)

n_all$Station<-"North"
n_all$DateTime<-paste(n_all$Date, n_all$Time, sep = " ")
n_all$DateTime<-mdy_hm(n_all$DateTime)
n_all<-n_all %>% 
  rename(ODO = "ODO_PC") %>% 
  select(-c(Date, Time))
View(n_all)

remove(n2010)
remove(n2011)
remove(n2012)
remove(n2013)
remove(n2014)
remove(n2015)
remove(n2016)

#####Mid Station#####
m2010<-read.csv("Data_2010_2016/MID_2010.csv")
m2011<-read.csv("Data_2010_2016/MID_2011.csv")
m2012<-read.csv("Data_2010_2016/MID_2012.csv")
m2013<-read.csv("Data_2010_2016/MID_2013.csv")
m2014_1<-read.csv("Data_2010_2016/MID_2014-1.csv")
m2014_2<-read.csv("Data_2010_2016/MID_2014-2.csv")
m2015<-read.csv("Data_2010_2016/MID_2015a.csv")
m2016<-read.csv("Data_2010_2016/MID_2016.csv")

m_all<- m2010 %>% 
  bind_rows(., m2011) %>% 
  bind_rows(., m2012) %>% 
  bind_rows(., m2013) %>% 
  bind_rows(., m2014_1) %>% 
  bind_rows(., m2014_2) %>% 
  bind_rows(., m2015) %>% 
  bind_rows(., m2016)

m_all$Station<-"Mid"
m_all$DateTime<-paste(m_all$Date, m_all$Time, sep = " ")
m_all$DateTime<-mdy_hm(m_all$DateTime)
m_all<-m_all %>% 
  rename(ODO = "ODO_PC") %>% 
  select(-c(Date, Time, Cond))
View(m_all)

remove(m2010)
remove(m2011)
remove(m2012)
remove(m2013)
remove(m2014_1)
remove(m2014_2)
remove(m2015)
remove(m2016)

####South Station####
s2010<-read.csv("Data_2010_2016/SOUTH_2010.csv")
s2011<-read.csv("Data_2010_2016/SOUTH_2011.csv")
s2012<-read.csv("Data_2010_2016/SOUTH_2012.csv")
s2013<-read.csv("Data_2010_2016/SOUTH_2013.csv")
s2014<-read.csv("Data_2010_2016/SOUTH_2014.csv")
s2015<-read.csv("Data_2010_2016/SOUTH_2015.csv")
s2016<-read.csv("Data_2010_2016/SOUTH_2016.csv")

s_all<- s2010 %>% 
  bind_rows(., s2011) %>% 
  bind_rows(., s2012) %>% 
  bind_rows(., s2013) %>% 
  bind_rows(., s2014) %>% 
  bind_rows(., s2015) %>% 
  bind_rows(., s2016)

s_all$Station<-"South"
s_all$DateTime<-paste(s_all$Date, s_all$Time, sep = " ")
s_all$DateTime<-mdy_hm(s_all$DateTime)
s_all<-s_all %>% 
  rename(ODO = "ODO_PC") %>% 
  select(-c(Date, Time))
View(s_all)

remove(s2010)
remove(s2011)
remove(s2012)
remove(s2013)
remove(s2014)
remove(s2015)
remove(s2016)

#####Join all data#####
wq_all<-wq_2017 %>% 
  bind_rows(., n_all) %>% 
  bind_rows(., m_all) %>% 
  bind_rows(., s_all)

write.csv(wq_all, "Raw YSI Data/wq_2010_2017.csv", row.names=FALSE)

#####Prep for averages and plots#####
wq_all$Year<-year(wq_all$DateTime)
wq_all$Month<-month(wq_all$DateTime)
wq_all$YDay<-yday(wq_all$DateTime)
wq_all$Hour<-hour(wq_all$DateTime)

#####Save Years Separately#####
wq_all %>% 
  filter(Year == 2010) %>% 
  write.csv(., "Data_2010_2016/wq_2010.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2011) %>% 
  write.csv(., "Data_2010_2016/wq_2011.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2012) %>% 
  write.csv(., "Data_2010_2016/wq_2012.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2013) %>% 
  write.csv(., "Data_2010_2016/wq_2013.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2014) %>% 
  write.csv(., "Data_2010_2016/wq_2014.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2015) %>% 
  write.csv(., "Data_2010_2016/wq_2015.csv", row.names=FALSE)

wq_all %>% 
  filter(Year == 2016) %>% 
  write.csv(., "Data_2010_2016/wq_2016.csv", row.names=FALSE)
