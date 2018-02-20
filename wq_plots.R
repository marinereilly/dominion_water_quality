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

#####Plots######
###Salinity###
#Hourly average Salinity for all stations
a<-ggplot()+
  geom_point(data=salinity_hour_hobo, aes(x=date_time, y=hourly_av, color= Station),size =0.5)+
  geom_point(data=salinity_hour, aes(x=date_time, y=hourly_av, color= Station), size=0.5)+
  theme_minimal()+
  scale_colour_manual(values =pal7)+
  ggtitle("Hourly Average Salinity at Cove Point 2017")+
  ylab("Salinity (ppt)")+
  xlab("Date")+
  guides(color = guide_legend(override.aes = list(size=2.5)))
a



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
  geom_line(data=temp_hour_all, aes(x=date_time, y=hourly_av), color= "darkorchid4", na.rm = FALSE,)+
  geom_point(data = do_hour, aes(x=date_time, y=hourly_av, color=Station), size=0.1, na.rm = FALSE,)+
  guides(color = guide_legend(override.aes = list(size=2.5)))+
  geom_hline(yintercept = 2, color="black", linetype = "dashed")+
  scale_colour_manual(values =pal7)+
  ggtitle("2017 Hourly Dissolved Oxygen and Temperature")+
  xlab("Date")+
  ylab("DO concentration (mg/L); Water Temperature (C)")+
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(size=2.5)))
c  

#Hypoxia
d<-ggplot()+
  geom_point(data=hypoxia, aes(x=DateTime, y=Station, color=Station))+
  scale_colour_manual(values =pal7)+
  theme_minimal()+
  ggtitle("Date of Hypoxia by Station")+
  xlab("Date")
d
