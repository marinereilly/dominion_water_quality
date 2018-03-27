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

met_all$Year<-as.factor(met_all$Year)

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

met_all<-delete.na(met_all, 3)

met_2017<-met_all %>% 
  filter(Year=="2017")
met_2017$Year<-as.factor(met_2017$Year)

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
av_monthly_rainfall<-met_monthly_sum_2010_2017 %>% 
  select(Year, Month, Rainfall_Sum) %>% 
  group_by(Month) %>% 
  summarise(av_rainfall=mean(Rainfall_Sum), sd_rainfall=sd(Rainfall_Sum))
av_monthly_rainfall$rf2017<-met_monthly_sum_2017$Rainfall_Sum  
av_monthly_rainfall$difference<-av_monthly_rainfall$av_rainfall-av_monthly_rainfall$rf2017

met_daily_av_2010_2017$Year<-as.factor(met_daily_av_2010_2017$Year)

met_monthly_sum_2010_2017$Month<-as.numeric(met_monthly_sum_2010_2017$Month)
met_monthly_sum_2010_2017$Year<-as.factor(met_monthly_sum_2010_2017$Year)
met_monthly_sum_2017<-met_monthly_sum_2010_2017 %>% 
  filter(Year=="2017")

met_year_rain<-met_all %>% 
  group_by(Year) %>% 
  summarise(yearly_rainfall=sum(Rainfall, na.rm = TRUE))

rain_days<-met_all %>% 
  filter(Rainfall>0) %>% 
  group_by(Year) %>% 
  summarise(., raindays=n_distinct(Yday))

rain<-rain_days %>% 
  full_join(., met_year_rain)
rain$Year<-as.numeric(rain$Year)
#####Adding Season Factor#####



######Wind Stuff######

met_all$grouped_wind<- if_else(met_all$Wind_Dir>=0 & met_all$Wind_Dir<=22, 0, if_else(
  met_all$Wind_Dir>22 & met_all$Wind_Dir<=68, 45, if_else(
    met_all$Wind_Dir>68 & met_all$Wind_Dir<=112, 90, if_else(
      met_all$Wind_Dir>112 & met_all$Wind_Dir<=158, 135, if_else(
        met_all$Wind_Dir>158 & met_all$Wind_Dir<=202, 180, if_else(
          met_all$Wind_Dir>202 & met_all$Wind_Dir<=248, 225, if_else(
            met_all$Wind_Dir>248 & met_all$Wind_Dir<=292, 270, if_else(
              met_all$Wind_Dir>292 & met_all$Wind_Dir<=338, 315, if_else(
                met_all$Wind_Dir>338 & met_all$Wind_Dir<=360, 0, 99999)))))))))

wind_freq<-met_all%>% 
  group_by(Year) %>% 
  count(., grouped_wind)
wind_zero<-wind_freq %>% 
  filter(grouped_wind==0)
wind_zero$grouped_wind<-360
wind_freq<-wind_freq %>% 
  bind_rows(., wind_zero)
wind_freq_2017<-wind_freq %>% 
  filter(Year=="2017")

high_wind<-met_2017 %>% 
  filter(Wind_spd>=18)
high_wind$Yday<-as.factor(high_wind$Yday)
count(high_wind$Yday)

high_wind_frequency<-met_all %>% 
  filter(Wind_spd>=18)
high_wind_count<-high_wind_frequency %>% 
  group_by(Year) %>% 
  count(., grouped_wind)
highwind_zero<-high_wind_count %>% 
  filter(grouped_wind==0)
highwind_zero$grouped_wind=360
hw_freq<-high_wind_count %>% 
  bind_rows(., highwind_zero)
hw_2017<-hw_freq %>% 
  filter(Year=="2017")

met_all$beaufort<-if_else(1>met_all$Wind_spd, 0, 
                          if_else(4>met_all$Wind_spd, 1,
                                  if_else(7>met_all$Wind_spd, 2,
                                          if_else(12>met_all$Wind_spd, 3, 
                                                  if_else(18>met_all$Wind_spd, 4, 
                                                          if_else(23>met_all$Wind_spd, 5, 
                                                                  if_else(30>met_all$Wind_spd, 6, 
                                                                          if_else(38>met_all$Wind_spd, 7, 
                                                                                  if_else(46>met_all$Wind_spd, 8, 
                                                                                          if_else(55>met_all$Wind_spd,9,999999))))))))))

                          
wind_freq_strength<-met_all%>% 
  group_by(Year, beaufort) %>% 
  count(., grouped_wind)                          
wind_freq_strength_2017<-wind_freq_strength %>% 
  filter(Year==2017)
wind_freq_strength_2017$beaufort<-as.factor(wind_freq_strength_2017$beaufort)
wind_freq_strength_2017$direction<-if_else(wind_freq_strength_2017$grouped_wind==0, "N", 
                                      if_else(wind_freq_strength_2017$grouped_wind==45, "NE",
                                         if_else(wind_freq_strength_2017$grouped_wind==90, "E",
                                           if_else(wind_freq_strength_2017$grouped_wind==135, "SE",
                                              if_else(wind_freq_strength_2017$grouped_wind==180, "S",
                                                if_else(wind_freq_strength_2017$grouped_wind==225, "SW",
                                                  if_else(wind_freq_strength_2017$grouped_wind==270, "W",
                                                    if_else(wind_freq_strength_2017$grouped_wind==315, "NW","9999999"))))))))
wind_freq_strength_2017<-wind_freq_strength_2017[-54,]
wind_freq_strength_2017$direction<-factor(wind_freq_strength_2017$direction, levels=c("E", "SE", "S", "SW", "W", "NW", "N", "NE"))
wind_freq_strength_2017$beaufort<-factor(wind_freq_strength_2017$beaufort, levels = c("8","7", "6", "5", "4", "3", "2", "1", "0"))
ws_zero<-wind_freq_strength_2017 %>% 
  filter(grouped_wind==0)
ws_zero$grouped_wind<-360
ws_2017<-wind_freq_strength_2017 %>% 
  bind_rows(., ws_zero)
wind_freq<-na.omit(wind_freq)

met_all$Season<-if_else(met_all$Month==1|met_all$Month==2|met_all$Month==12, "Winter", 
                        if_else(met_all$Month==3|met_all$Month==4|met_all$Month==5, "Spring",
                                if_else(met_all$Month==6|met_all$Month==7|met_all$Month==8,"Summer", "Autumn")
))
met_all$Season<-as.factor(met_all$Season)


wind_freq_seasonal<-met_all%>% 
  group_by(Season,Year) %>% 
  count(., grouped_wind)
wind_zero_seasonal<-wind_freq_seasonal %>% 
  filter(grouped_wind==0)
wind_zero_seasonal$grouped_wind<-360
wind_freq_seasonal<-wind_freq_seasonal %>% 
  bind_rows(., wind_zero_seasonal)
wfs2017<-wind_freq_seasonal %>% 
  filter(Year==2017)





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


#####Water Year Calculations and Cumulative Plot#####
View(met_daily_sum_2010_2017)
day_rain<-met_daily_sum_2010_2017 %>% 
  select(Year, days, Rainfall_Sum)
day_rain$month<-month(day_rain$days)
day_rain$day<-day(day_rain$days)
day_rain$date<-paste0(day_rain$month, "-", day_rain$day, "-", day_rain$Year)
day_rain$date<-mdy(day_rain$date)
day_rain<-arrange(day_rain, date)
day_rain<-select(day_rain, date, month, day, Year, Rainfall_Sum, days)
day_rain$water_year<-if_else(day_rain$month<10, day_rain$Year, day_rain$Year+1)
day_rain<-day_rain %>% 
  group_by(water_year) %>% 
  arrange(date) %>% 
  mutate(wy_cum_rainfall = cumsum(Rainfall_Sum))
day_rain$days2<-if_else(day_rain$month<10, 
                        paste0(day_rain$month, "-", day_rain$day, "-", 2000), paste0(day_rain$month, "-", day_rain$day, "-", 1999))
day_rain$days2<-mdy(day_rain$days2)
two_week_sum <- function(x,n=14){filter(x,rep(1,n), sides=1)}
day_rain$two_week_cum_rainfall<-rollsumr(day_rain$Rainfall_Sum, k=14, fill=NA)

#####Aerial FLight Rain Calcs#####
library(readxl)
ai_flight <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/aerial_imagery_metadata.xlsx")
View(ai_flight)
ai_flight<-select(ai_flight, GBA_ref=`GBA Reference Number`,
                  date = `Date Flown`)
ai_flight$date<-mdy(ai_flight$date)
ai_flight<-ai_flight %>% 
  left_join(., day_rain, by="date")
ai_flight<- ai_flight %>% 
  select(GBA_ref, date, month, day, Year, water_year, Rainfall_Sum, wy_cum_rainfall, two_week_cum_rainfall)
