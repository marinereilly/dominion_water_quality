#####Packages#####
library(tidyverse)
library(dadjoke)
library(lubridate)

#####Load data#####
met_all<-read.csv(met_all, "Raw YSI Data/met_all.csv")

#####Formatting for averages#####
met_all$DateTime<-bajhfdsaklhfa3w
  
met_all$Year<-year(met_all$DateTime)
met_all$Month<-month(met_all$DateTime)
met_all$Yday<-yday(met_all$DateTime)
met_all$Hour<-hour(met_all$DateTime)
met_all$days<-as.Date(format(met_all$DateTime,"%d-%m-2007"),format="%d-%m-%y")

#####Calculating Sums and Averages#####
met_daily_av_2010_2017<-met_all %>% 
  group_by(Year, days) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(Mean="mean", Sd="sd"), na.rm=TRUE)

met_daily_sum_2010_2017<-met_all %>% 
  group_by(Year, days) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(Sum="sum"), na.rm=TRUE)

met_monthly_sum_2010_2017<-met_all %>% 
  group_by(Year, Month) %>% 
  summarise_if(.predicate = function(x) is.numeric(x),
               .funs = c(Sum="sum"), na.rm=TRUE)
met_monthly_sum_2010_2017$Month<-as.numeric(met_monthly_sum_2010_2017$Month)
met_monthly_sum_2010_2017$Year<-as.factor(met_monthly_sum_2010_2017$Year)
met_monthly_sum_2017<-met_monthly_sum_2010_2017 %>% 
  filter(Year=="2017")
######Wind Stuff######

met_all$grouped_wind<- if_else(met_all$Wind_Dir>0 & met_all$Wind_Dir<=22, 0, if_else(
  met_all$Wind_Dir>22 & met_all$Wind_Dir<=68, 45, if_else(
    met_all$Wind_Dir>68 & met_all$Wind_Dir<=112, 90, if_else(
      met_all$Wind_Dir>112 & met_all$Wind_Dir<=158, 135, if_else(
        met_all$Wind_Dir>158 & met_all$Wind_Dir<=202, 180, if_else(
          met_all$Wind_Dir>202 & met_all$Wind_Dir<=248, 225, if_else(
            met_all$Wind_Dir>248 & met_all$Wind_Dir<=292, 270, if_else(
              met_all$Wind_Dir>292 & met_all$Wind_Dir<=338, 315, if_else(
                met_all$Wind_Dir>338 & met_all$Wind_Dir<=360, 0, 99999)))))))))

######Calc for table#####
met_calc<-met_all %>% 
  group_by(Year) %>% 
  summarise(total_rainfall = sum(Rainfall, na.rm=TRUE), max_air_temp = max(Air_Temp, na.rm=TRUE), 
            min_air_temp = min(Air_Temp, na.rm=TRUE), mean_air_temp=mean(Air_Temp, na.rm=TRUE))
write.csv(met_calc, "met_calcs.csv")


#####Pull just 2017#####
met_daily_av_2017<- met_daily_av_2010_2017 %>% 
  filter(Year == "2017")
met_daily_sum_2017<- met_daily_sum_2010_2017 %>% 
  filter(Year =="2017")
######For Depth, Precipitation and Salinity Plot
depth<-wq_2017 %>% 
  filter(Depth<=8) %>% 
  group_by(days, Hour) %>% 
  summarise(av_depth=mean(Depth))
salinity<-hourly_av_2010_2017 %>% 
  filter(Year=="2017")
salinity2<-daily_av_2010_2017 %>% 
  filter(Year=="2017")