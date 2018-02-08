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

#####Plots######
###Salinity###
a<-ggplot()+
  geom_point(data=wq_2017, aes(x=DateTime, y=Salinity, color=Station), size=0.5)+
  theme_minimal()+
  scale_colour_manual(values =pal6)
a
