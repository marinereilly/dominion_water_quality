#####This script is for exploratory graphing of weather 
#####data at Cove Point marsh from the MET Station data
#####You should have already run the join data scripts

#####start#####

#####Load Packages#####
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library(dadjoke)

#####Load data if you haven't already#####
met_all<-read.csv("Raw YSI Data/met_all.csv")
wq_all<-read.csv("Raw YSI Data/wq_2010_2017.csv")

#####Formatting data if you haven't already#####
met_all$Year<-as.factor(met_all$Year)

#####Creating Monthly Daily and Hourly Averages/Accumulations####
met_month<-met_all %>% 
  group_by(Year, Month) %>% 
  summarise(Av_Temp=mean(Air_Temp), Av_RH = mean(RH), Av_BP = mean(BP),
            mo_Rf = sum(Rainfall), mo_Hail = sum(Hail))
met_month$FakeDay<-15
met_month$DateTime<-paste(met_month$Year, met_month$Month, 
                          met_month$FakeDay, sep = "-")
met_month$DateTime<-ymd(met_month$DateTime)
met_month<-met_month %>% 
  select(-c(Month,FakeDay))

met_day<-met_all %>% 
  group_by(Year, Yday) %>% 
  summarise(Av_Temp=mean(Air_Temp), Av_RH = mean(RH), Av_BP = mean(BP),
            mo_Rf = sum(Rainfall), mo_Hail = sum(Hail))

met_hour<-met_all %>% 
  group_by(Year, Yday, Hour) %>% 
  summarise(Av_Temp=mean(Air_Temp), Av_RH = mean(RH), Av_BP = mean(BP),
            mo_Rf = sum(Rainfall), mo_Hail = sum(Hail))

#####Annual Temperatures#####
####Raw Data
a<-ggplot(met_2017)
b<-a+
  geom_point(aes(x=DateTime, y=Temp), size = .5)
b
