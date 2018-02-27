#####Make sure you have the wq_2017 and hobo dataframe formatted and loaded#####
#####Packages#####
library(dadjoke)
library(tidyverse)
library(ggpmisc)

#####Palettes#####
pal1<-c("North"="#27A6C4", "Mid"="#3858CD", "South"="#FFBF2D", "Hobo"="#FF952D")
pal2<-c("North"="#03738D", "Mid"="#0D2C9B", "South"="#E49E00", "Hobo"="#E47100")
pal3<-c("North"="#FF4F00", "Mid"="#FF9500", "South"="#0C5DA5", "Hobo"="#00AC6B")
pal4<-c("North"="#49E307", "Mid"="#06BA8D", "South"="#F30731", "Hobo"="#FF6408")
pal5<-c("North"="#DE4500", "Mid"="#DE8200", "South"="#095090", "Hobo"="#00965D")
pal6<-c("North"="#E69F00", "Mid"="#56B4E9", "South"="#009E73", "Hobo"="#F0E442")
pal7<-c("North"="#E69F00", "Mid"="#F0E442", "South"="#009E73", "Hobo"="#56B4E9")

#####2017 Plots######
###Salinity###
#Hourly average Salinity for all stations
a<-ggplot()+
  geom_point(data=salinity_hour_hobo, aes(x=date_time, y=hourly_av, color= Station),size =0.5)+
  geom_point(data=salinity_hour, aes(x=date_time, y=hourly_av, color= Station), size=0.5)+
  theme_minimal()+
  scale_colour_manual(values =pal7)+
  ggtitle("Average Hourly Salinity at Cove Point 2017")+
  ylab("Salinity (ppt)")+
  xlab("Date")+
  guides(color = guide_legend(override.aes = list(size=2.5)))
a
ggsave(filename = "2017_hourly_sal.svg")


#Daily average salinity for all stations
b<-ggplot()+
  geom_line(data=salinity_day_hobo2, aes(x=date, y=daily_av, color= Station), na.rm = FALSE, size=1)+
  geom_line(data=salinity_day2, aes(x=date, y=daily_av, color= Station), na.rm = FALSE, size =1)+
  theme_minimal()+
  scale_colour_manual(values =pal7)+
  ggtitle("Daily Average Salinity at Cove Point 2017")+
  ylab("Salinity (ppt)")+
  xlab("Date")
b

#Hourly Averages of DO and Temperature
c<-ggplot()+
  geom_point(data = do_hour, aes(x=date_time, y=hourly_av, color=Station),
             size=0.1, na.rm = FALSE, alpha=0.5)+
  geom_smooth(data=temp_hour_all, aes(x=date_time, y=hourly_av), color= "darkorchid4", na.rm = FALSE,)+
  guides(color = guide_legend(override.aes = list(size=2.5)))+
  geom_hline(yintercept = 2, color="black", linetype = "dashed")+
  scale_colour_manual(values =pal7)+
  ggtitle("2017 Hourly Dissolved Oxygen and Temperature")+
  xlab("Date")+
  ylab("DO concentration (mg/L); Water Temperature (C)")+
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(size=2.5, alpha=1)))
c  

#Hypoxia
d<-ggplot()+
  geom_point(data=hypoxia, aes(x=DateTime, y=Station, color=Station))+
  scale_colour_manual(values =pal7)+
  theme_minimal()+
  ggtitle("Date of Hypoxia by Station")+
  guides(fill=FALSE)+
  xlab("Date")
d

i<-ggplot()+
  geom_point(data=hypoxia_all, aes(x=days, y=Station, color=Station))+
  scale_colour_manual(values =pal7)+
  theme_minimal()+
  ggtitle("Hypoxia over Time")+
  xlab("Date")+
  ylab("Year")+
  scale_x_date(date_labels =  "%b")+
  facet_grid(Year~., switch = "y")+
  theme(strip.text.y = element_text(angle = 180))+
  theme(axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank())
i

##### All Years Plots #####
#### Palates for Years####
ypal1 <- c("2010"="#fff7f3", "2011"="#fde0dd", "2012"="#fcc5c0", "2013"="#fa9fb5", 
           "2014"="#f768a1", "2015"="#dd3497", "2016"="#ae017e", "2017"="#7a0177")
ypal2 <- c("2010"="#fde0dd", "2011"="#fcc5c0", "2012"="#fa9fb5", "2013"="#f768a1", 
           "2014"="#dd3497", "2015"="#ae017e", "2016"="#7a0177", "2017"="#49006a")
ypal3 <- c("2010"="#f7fcf0", "2011"="#e0f3db", "2012"="#ccebc5", "2013"="#a8ddb5", 
           "2014"="#7bccc4", "2015"="#4eb3d3", "2016"="#2b8cbe", "2017"="#08589e")
###Daily Salinity by station###
##Remove outliers as plotted so that we don't lose other data##
#two values are above 500 which seems improbable# 
#There are 6 other really high values that the 2013 reportmention occur during
#a storm. Rather than filter anything I am adjusting the scale or the y axis
e<-daily_av_2010_2017 %>% 
  ggplot()+
  geom_point(aes(x=days, y=Salinity_Mean, color=Year))+
  scale_colour_manual(values=ypal2)+
  scale_y_continuous(limits= c(0, 13))+
  scale_x_date(date_labels =  "%b")+
  theme_minimal()+
  facet_grid(Station~.)+
  xlab("Date")+ylab("Average Daily Salinity (ppt)")+
  ggtitle("2010 to 2017 salinities at Cove Point Marsh")

e
ggsave(file="2010_2017_Daily_sal.png")

###Hourly Salinity by Station###
f<-hourly_av_2010_2017 %>% 
  ggplot()+
  geom_point(aes(x=days, y=Salinity_Mean, color=Year), size =0.9, na.rm = TRUE)+
  scale_colour_manual(values=ypal2, breaks = levels(hourly_av_2010_2017$Year))+
  scale_x_date(date_labels =  "%b")+
  scale_y_continuous(limits= c(0, 15))+
  theme_minimal()+
  facet_grid(Station~.)+
  xlab("Date")+ylab("Average Hourly Salinity (ppt)")+
  ggtitle("Salinity has decreased at Cove Point Marsh since Restoration")+
  guides(color = guide_legend(override.aes = list(size=2.5)))+
  theme(strip.text.y = element_text(angle = 360))
f

ggsave(filename = "235.png")

###Timeline Style Salinity Graph###
i<-wq_all %>% 
  ggplot()+
  geom_point(aes(x=Date, y= Salinity, color=Year), na.rm=TRUE, size=0.9)+
  scale_colour_manual(values=ypal2, breaks = levels(hourly_av_2010_2017$Year))+
  scale_x_date(date_labels =  "%b %Y")+
  scale_y_continuous(limits= c(0, 15))+
  theme_minimal()+
  xlab("Date")+ylab("Average Hourly Salinity (ppt)")+
  ggtitle("Salinity has decreased at Cove Point Marsh since Restoration")+
  guides(color = guide_legend(override.aes = list(size=2.5)))
i

###Hourly Temp by Station###
g<-hourly_av_2010_2017 %>% 
  ggplot()+
  geom_smooth(aes(x=days, y=Temp_Mean, color=Year), size=1, se=FALSE)+
  scale_colour_manual(values=ypal2)+
  scale_x_date(date_labels =  "%b")+
  theme_minimal()+
  facet_grid(Station~.)+
  xlab("Date")+ylab("Smoothed Average Hourly Water Temperature (C)")+
  ggtitle("2010 to 2017 Water Temperature at Cove Point Marsh")
g
ggsave(filename = "2010_2017_Hourly_WTemp.png")

###Hourly Depth by Station###
h<-hourly_av_2010_2017 %>% 
  ggplot()+
  geom_point(aes(x=days, y=Depth_Mean, color=Year), size=0.5)+
  scale_colour_manual(values=ypal2)+
  scale_x_date(date_labels =  "%b")+
  scale_y_continuous(limits = c(0, 5))+
  theme_minimal()+
  facet_grid(Station~.)+
  xlab("Date")+ylab("Average Hourly Water Depth (m)")+
  ggtitle("2010 to 2017 Water Depth at Cove Point Marsh")
h