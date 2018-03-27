#####This script is for exploratory graphing of weather 
#####data at Cove Point marsh from the MET Station data
#####You should have already run the join data scripts

#####start#####
#####Helpful Stack overflow question!#####
#https://stackoverflow.com/questions/26587940/ggplot2-different-legend-symbols-for-points-and-lines

#####Load Packages#####
library(tidyverse)
library(cowplot)
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
  geom_hline(yintercept = 0, color= "grey69")+
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
  facet_grid(Station~., switch = "y")+
  theme(strip.text.y = element_text(angle = 180))
y

k<-ggplot()+
  geom_line(data=met_daily_sum_2017, aes(x=days, y=Rainfall_Sum))+
  scale_x_date(limits = c(as.Date("2020-08-01"), as.Date("2020-09-01")), date_breaks = "1 day", date_labels = "%d")
k
#####Monthly Precipitation#####

w<- ggplot()+
  geom_line(data=met_monthly_sum_2010_2017,aes(x=Month, y=Rainfall_Sum, color=Year), size=1, na.rm = TRUE)+
  geom_line(data=met_monthly_sum_2017,aes(x=Month, y=Rainfall_Sum, color=Year), size=2)+
  scale_color_manual(values = ypal2)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  theme_minimal()+
  ggtitle("Monthly Rainfall at Cove Point Marsh")+
  ylab("Rainfall (in)")
w  

#####Annual Temperatures 2010-2017#####
v<-ggplot()+
  geom_point(data=met_daily_av_2010_2017, aes(x=days, y=Air_Temp_Mean, color=Year), alpha=0.2)+
  geom_smooth(data=met_all, aes(x=days, y=Air_Temp, color=Year), se=FALSE, size=1)+
  geom_smooth(data=met_2017, aes(x=days, y=Air_Temp, color=Year), se=FALSE,  size=2)+
  theme_minimal()+
  scale_color_manual(values= ypal2)+
  ggtitle("Yearly Air Temperatures")+
  xlab("Date")+ylab("Air Temperature (C)")+
  scale_x_date(date_labels = "%b")
v
#####Raindays and amounts
u<-ggplot(rain) +
  geom_smooth(aes(x=Year, y = yearly_rainfall/raindays), method=lm)+
  geom_point(aes(x=Year, y = yearly_rainfall/raindays))
u
#####Wind and storm Plotting

t<-ggplot(data=wind_freq, aes(x=grouped_wind, y= n, color=Year))+
  geom_line(size=.8)+
  geom_line(data = wind_freq_2017, aes(x=grouped_wind, y= n, color=Year),linetype="solid", size=2)+
  theme_minimal()+
  scale_color_manual(values = ypal2)+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("Winds at Cove Point Marsh are Primarily Northwesterly and Southerly")
t
groan()

s<-ggplot(data=hw_freq, aes(x=grouped_wind, y= n, color=Year))+
  geom_line(size=.8)+
  geom_line(data = hw_2017, aes(x=grouped_wind, y= n, color=Year), size=2)+
  theme_minimal()+
  scale_color_manual(values = ypal2)+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("North Winds Dominated High Wind Events In 2017")
s

t_season<-ggplot(data=wind_freq_seasonal, aes(x=grouped_wind, y= n, color=Year))+
  geom_line(size=.8)+
  geom_line(data = wfs2017, aes(x=grouped_wind, y= n, color=Year),linetype="solid", size=1)+
  theme_minimal()+
  scale_color_manual(values = ypal2)+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  theme(legend.position="bottom")+
  guides(color=guide_legend(ncol = 9))+
  ggtitle("Seasonal Variability in Wind Direction")+facet_grid(.~Season)
t_season
#2017 frequency of direction by wind strength

r<-ggplot(data=wind_freq_strength_2017)+
  geom_bar(aes(x=direction, y=n, fill=beaufort), color="black", stat="identity")+
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE, option="magma", direction = -1)+
  ggtitle("2017 Wind Speed on the Beaufort Scale")+
  coord_polar(theta="x", start = 1.18682)+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  guides(fill=guide_legend(title="Beaufort Number"))
r  

#####Water Year Plots#####
p<-day_rain %>% 
  filter(water_year!=2010) %>% 
  filter(water_year!=2018) %>% 
  ggplot(., aes(x=days2, y=wy_cum_rainfall, color=as.factor(water_year), size=as.factor(water_year)))+
  geom_step()+
  theme_minimal()+scale_color_manual(values=ypal2)+
  scale_size_manual(values = c("2017"=1.5, "2016"=1, "2015"=1, "2014"=1, "2013"=1, "2012"=1, "2011"=1))+
  scale_x_date(date_labels = "%b", date_breaks = "1 month", minor_breaks = NULL)+
  guides(color=guide_legend(title="Water Year"), size=FALSE)+
  xlab("Date")+ylab("Cumulative Rainfall")+ggtitle("Cumulative Rainfall by Water Year")
p
