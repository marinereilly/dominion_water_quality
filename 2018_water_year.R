#####Water Year Calcs for part of 2018

library(tidyverse)
library(lubridate)
library(gg)

j_rain<-read.csv(file = "Raw YSI Data/met_2018_jan_june_rainfall.csv", header = FALSE)
View(j_rain)
j_rain<-j_rain [-(1:3),]
j_rain$V1<-ymd_hms(j_rain$V1)
j_rain$V2<-as.numeric(as.character(j_rain$V2))

col<-c("DateTime", "Rainfall")
colnames(j_rain)<-col
rain_2018<-met_all %>% 
  select(DateTime, Rainfall) %>% 
  bind_rows(., j_rain)
View(rain_2018)

rain_2018$Year<-year(rain_2018$DateTime)
rain_2018$Month<-month(rain_2018$DateTime)
rain_2018$Day<-day(rain_2018$DateTime)

r_daily_2018<- rain_2018 %>% 
  group_by(Year, Month, Day) %>% 
  summarise(., daily_rainfall=sum(Rainfall))
View(r_daily_2018)

r_daily_2018$date<-paste(r_daily_2018$Year, "-",r_daily_2018$Month,"-" ,r_daily_2018$Day, sep = "")
r_daily_2018$date<-ymd(r_daily_2018$date)
r_daily_2018$water_year<-if_else(r_daily_2018$Month<10, r_daily_2018$Year, r_daily_2018$Year+1)
r_daily_2018<-r_daily_2018 %>% 
  group_by(water_year) %>% 
  arrange(date) %>% 
  mutate(wy_cum_rainfall = cumsum(daily_rainfall))
r_daily_2018$days2<-if_else(r_daily_2018$Month<10, 
                        paste0(r_daily_2018$Month, "-", r_daily_2018$Day, "-", 2000), paste0(r_daily_2018$Month, "-", r_daily_2018$Day, "-", 1999))
r_daily_2018$days2<-mdy(r_daily_2018$days2)
r_daily_2018$Year<-as.factor(r_daily_2018$Year)


p<-r_daily_2018 %>% 
  filter(water_year!=2010) %>% 
  ggplot(., aes(x=days2, y=wy_cum_rainfall, color=as.factor(water_year), size=as.factor(water_year), ))+
  geom_step()+
  theme_minimal()+
  scale_color_manual(values = c("2018"="red3", "2017"="grey75", "2016"="grey75", "2015"="grey75", "2014"="grey75", "2013"="grey75", "2012"="grey75", "2011"="grey75"))+
  scale_size_manual(values = c("2018"=1.5, "2017"=1, "2016"=1, "2015"=1, "2014"=1, "2013"=1, "2012"=1, "2011"=1))+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", minor_breaks = NULL)+
  guides(color=guide_legend(title="Water Year"), size=FALSE)+
  annotate("text", label = "2018", x = as.Date("2000-06-01"), y = 15, size = 6, colour = "red3")+
  xlab("Date")+ylab("Cumulative Rainfall")+ggtitle("Cumulative Rainfall by Water Year")+theme(legend.position = "none")
p

year_color_highlight<-c("2018"="red3", "2017"="grey75", "2016"="grey75", "2015"="grey75", "2014"="grey75", "2013"="grey75", "2012"="grey75", "2011"="grey75")
year_size_highlight<-c("2018"=1.5, "2017"=1, "2016"=1, "2015"=1, "2014"=1, "2013"=1, "2012"=1, "2011"=1)
