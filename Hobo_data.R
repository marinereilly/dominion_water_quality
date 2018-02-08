#####To stich together the conductivity hobo data#####
#####Packages#####
library(tidyverse)
library(dadjoke)
library(lubridate)

#####Load all of the data#####
#create column headings
head<-c("date_time", "high_cond", "temperature", "sp_cond", "salinity")

#load data and remove tags/extra column and add column headings
#metadata including sn of probe and units are removed here, but are stored in the original 
#data so can easily be retrieved if necessary

c1<-read.csv("Conductivity HOBO/2016_11_30_2017_01_19_Salinity.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c1<-c1[-c(1,2),-c(1) ]
colnames(c1)<-head 

c2<-read.csv("Conductivity HOBO/2017_01_19_2017_02_03_Salinity.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c2<-c2[-c(1,2),-c(1) ]
colnames(c2)<-head

c3<-read.csv("Conductivity HOBO/2017_02_03_2017_03_09_Salinity.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c3<-c3[-c(1,2),-c(1) ]
colnames(c3)<-head 

c4<-read.csv("Conductivity HOBO/Conductivity_2017_03_09_2017_03_29.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c4<-c4[-c(1,2),-c(1) ]
colnames(c4)<-head 

c5<-read.csv("Conductivity HOBO/Conductivity_2017_03_29_2017_04_13.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c5<-c5[-c(1,2),-c(1) ]
colnames(c5)<-head 

c6<-read.csv("Conductivity HOBO/Conductivity_2017_04_28_2017_05_18.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c6<-c6[-c(1,2),-c(1) ]
colnames(c6)<-head 

c7<-read.csv("Conductivity HOBO/Conductivity_2017_05_19_2017_06_02.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c7<-c7[-c(1,2),-c(1) ]
colnames(c7)<-head 

c8<-read.csv("Conductivity HOBO/Conductivity_2017_06_02_2017_06_13.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c8<-c8[-c(1,2),-c(1) ]
colnames(c8)<-head 

c9<-read.csv("Conductivity HOBO/Conductivity_2017_06_12_2017_06_28.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c9<-c9[-c(1,2),-c(1) ]
colnames(c9)<-head 

c10<-read.csv("Conductivity HOBO/Conductivity_2017_06_27_2017_07_12.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c10<-c10[-c(1,2),-c(1) ]
colnames(c10)<-head 

c11<-read.csv("Conductivity HOBO/Conductivity_2017_11_14_2017_11_28.csv",
             header=FALSE, dec=",", stringsAsFactors=FALSE)
c11<-c11[-c(1,2),-c(1) ]
colnames(c11)<-c("date_time", "low_cond", "full_cond", "temperature", "sp_cond", "salinity") 

#####Combine data together#####
hobo<-c1 %>% 
  bind_rows(., c2) %>% 
  bind_rows(., c3) %>%
  bind_rows(., c4) %>%
  bind_rows(., c5) %>%
  bind_rows(., c6) %>%
  bind_rows(., c7) %>%
  bind_rows(., c8) %>%
  bind_rows(., c9) %>%
  bind_rows(., c10) %>%
  bind_rows(., c11)
View(hobo)  
rm(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, head)
hobo$date_time<-  mdy_hms(hobo$date_time)
hobo<-hobo %>%
  filter(.$date_time >= "2017-01-01 00:00:00")
hobo$salinity<-as.numeric(hobo$salinity)
hobo$temperature<-as.numeric(hobo$temperature)
hobo$sp_cond<-as.numeric(hobo$sp_cond)
hobo$high_cond<-as.numeric(hobo$high_cond)
hobo$low_cond<-as.numeric(hobo$low_cond)
hobo$full_cond<-as.numeric(hobo$full_cond)
#####Check for outliers and weirdness#####
a<-ggplot(hobo)+
  geom_point(aes(x=date_time, y=salinity))+
  theme_minimal()
a

#Removing any points with a temperature of -10.03 
#as the probes don't work when the temp is that low and I'm pretty sure that 
#there is some weird malfunction
hobo<- hobo %>% 
  filter(.$temperature != -10.03|is.na(.$temperature))

#also removing all points where cond<100 as that is outside of the range of HOBO 10922142 
#which was the only hobo deployed at dominion in 2017 that had high_cond as a variable
#also removing all points where full_cond is >= 10,000 as that is outside the parameters for
#HOBO 10327675 which was deployed in Nov

hobo<-hobo %>% 
  filter(.$high_cond >=100|is.na(.$high_cond))
hobo<- hobo %>% 
  filter(.$full_cond<=10000|is.na(.$full_cond))

#####Adding a station Name so that it can be integrated with the rest of the YSI data#####
hobo$station<-"HOBO"
