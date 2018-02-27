#####This script is for exploratory graphing of weather 
#####data at Cove Point marsh from the MET Station data
#####You should have already run the join data scripts

#####start#####
#####Helpful Stack overflow question!#####
#https://stackoverflow.com/questions/26587940/ggplot2-different-legend-symbols-for-points-and-lines

#####Load Packages#####
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library(dadjoke)

#####Load data if you haven't already#####
#But honestly you should run through the weather wrangling and all wq wrangling scripts
met_all<-read.csv("Raw YSI Data/met_all.csv")
wq_all<-read.csv("Raw YSI Data/wq_2010_2017.csv")
wq_2017<-wq_all %>% 
  filter(Year=="2017")

#####Formatting data if you haven't already#####
#Again run through the wrangling scripts first. seriously. do this.
met_all$Year<-as.factor(met_all$Year)
met_daily_sum_2017$days<-ymd(met_daily_sum_2017$days)

#####Palettes#####
pal7<-c("North"="#E69F00", "Mid"="#F0E442", "South"="#009E73", "Hobo"="#56B4E9")

pal8<-c("Precipitation"="darkorchid3", "Depth"="navy")
pal9<-c("North"="#E69F00", "Mid"="#F0E442", "South"="#009E73","Depth"="navy", "Precipitation"="darkorchid3")

#####Plots#####
####Depth, Precipitation, Salinity####
legend_ord<-c("North", "Mid", "South", "Depth", "Precipitation")

z<-ggplot()+
  geom_line(data=met_daily_sum_2017, aes(x=days, y=Rainfall_Sum, color= "Precipitation"))+
  geom_point(data=salinity2, aes(x=days, y=Salinity_Mean, color=Station), size=0.6, 
             na.rm = TRUE, alpha = 0.7)+
  geom_line(data=depth, aes(x=days, y=av_depth, color="Depth"), linetype="twodash")+
  theme_minimal()+
  scale_color_manual(breaks= legend_ord, values = pal9)+
  scale_x_date(date_labels = "%b", date_breaks = "month", 
               limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")))+
  ggtitle("Depth, Precipitation and Salinity at Cove Point Marsh 2017")+
  xlab("Date")+
  ylab("Daily Cumulative Rainfall (in), Salinity (ppt), Depth (ft)")+
  guides(color = guide_legend(title = "Stations",
    override.aes = list(
      size=c(2.5, 2.5, 2.5, 1, 1), 
      shape = c(16, 16, 16, NA, NA), 
      linetype = c("blank", "blank", "blank", "twodash", "solid"), 
      alpha=1)))
z

#####Salinity and Precipitation by Station#####
y<-ggplot()+
  geom_point(data=salinity, aes(x=days, y=Salinity_Mean, color=Station), na.rm = TRUE, size=0.6)+
  geom_line(data=met_daily_sum_2017, aes(x=days, y=Rainfall_Sum, color= "Precipitation"))+
  theme_minimal()+
  scale_color_manual(breaks= legend_ord, values = pal9)+
  scale_x_date(date_labels = "%b", date_breaks = "month")+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle("Salinity and Precipitation by Station")+
  xlab("Date")+ylab("Salinity (ppt), Rainfall (in)")+
  guides(color = guide_legend(title = "Stations",
                              override.aes = list(
                                size=c(2.5, 2.5, 2.5, 1), 
                                shape = c(16, 16, 16, NA), 
                                linetype = c("blank", "blank", "blank", "solid"))))+
  facet_grid(Station~.)
y  





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
a<-met_hour %>% 
  filter(Year=="2017") %>% 
  ggplot()
b<-a+
  geom_line(aes(x=Yday, y=Av_Temp), size = .5)
b
