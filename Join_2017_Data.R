####This Script Describes how to join the 2017 data together####
#In 2017, YSI switched the system to a new one but wasn't sure when they would actually get it accomplished or how the
#new system would work.  As a result there are several data downloads from the first system and then a bunch from
#the new system that need to be combined and wrangled before the data can be analyzed

####Packages####
library(tidyverse)
library(dadjoke)
library(lubridate)

####Loading all of the different data pieces####
####South Site####
#These are files CBL Downloaded from the old server
s1 <- read.csv("Raw YSI Data/South_1_1_2017_3_23_2017.csv", header=FALSE)
c_names<-as.vector(t(s1)[,6])
s1<-s1 [-(1:7),]
colnames(s1)<-c_names
s1<-s1 [,-c(3,8)]
View(s1)

s2<-read.csv("Raw YSI Data/South_3_23_2017_4_5_2017.csv", header=FALSE)
s2<-s2 [-(1:7),]
colnames(s2)<-c_names
s2<-s2 [,-c(3,8)]

s3<-read.csv("Raw YSI Data/South_4_4_2017_4_10_2017.csv", header=FALSE)
s3<-s3 [-(1:7),]
colnames(s3)<-c_names
s3<-s3 [,-c(3,8)]

s4<-read.csv("Raw YSI Data/South_4_9_2017_4_19_2017.csv", header=FALSE)
s4<-s4 [-(1:7),]
colnames(s4)<-c_names
s4<-s4 [,-c(3,8)]

s5<-read.csv("Raw YSI Data/South_4_19_2017_5_3_2017.csv", header=FALSE)
s5<-s5 [-(1:7),]
colnames(s5)<-c_names
s5<-s5 [,-c(3,8)]

s6<-read.csv("Raw YSI Data/South_5_3_2017_5_18_2017.csv", header=FALSE)
s6<-s6 [-(1:7),]
colnames(s6)<-c_names
s6<-s6 [,-c(3,8)]

#These are files Tom Downloaded from the old server
s7<-read.csv("Raw YSI Data/South Pt 050117 to 053017_EcoNet.csv", header = FALSE)
co_names<-as.vector(t(s7)[,4])
s7<-s7 [-(1:10),]
colnames(s7)<-co_names
s7<-s7 [,-9]


s8<-read.csv("Raw YSI Data/South Pt 060117 to 061517_EcoNet.csv", header = FALSE)
s8<-s8 [-(1:10),]
colnames(s8)<-co_names
s8<-s8[,-9]

#These are the new server files
#Report from Tom Wazniak 8/21 stated South Station didn't come online until June 23

s9<-read.csv("Raw YSI Data/export-xyle510.Dominion Power.SouthPoint.Campbell PakBus Logger.csv", header = FALSE)
colnames(s9)<-c("DateTime", "x1", "Depth", "ODO%", "ODO Conc", "Salinity", "SpCond", "Temp", "x2", "x3", "x4")
s9<-s9 [-(1:3),]
s9<-s9 %>% 
  select(-starts_with("x"))
s9[s9 == ""]<-NA
s9$DateTime<-ymd_hms(s9$DateTime)
s9<- s9 %>% 
  filter(DateTime >="2017-06-23 07:45:40")
s9<-na.omit(s9)
View(s9)

#Bind everything together
South<- s1 %>% 
  bind_rows(., s2) %>% 
  bind_rows(., s3) %>% 
  bind_rows(., s4) %>% 
  bind_rows(., s5) %>%
  bind_rows(., s6) %>%
  bind_rows(., s7) %>%
  bind_rows(., s8) %>% 
  distinct()
View(South) 
  
South$DateTime<- paste(South$Date, South$Time, sep = " ")
South$DateTime<-mdy_hm(South$DateTime)
  
South<- South %>% 
  select(-Date, -Time) %>% 
  bind_rows(., s9) %>% 
  select("DateTime", "Temp", "SpCond", "Salinity", "Depth", "ODO%", "ODO Conc") %>% 
  rename(ODO = "ODO%", ODO_Conc = "ODO Conc")

South$Station<- "South"
write.csv(South, file= "Raw YSI Data/South2017.csv")

####Mid Site####
#These are files CBL Downloaded from the old server
m1 <- read.csv("Raw YSI Data/Mid_1_1_2017_3_23_2017.csv", header=FALSE)
c_names<-as.vector(t(m1)[,6])
m1<-m1 [-(1:7),]
colnames(m1)<-c_names
m1[m1 == ""]<-NA
m1<-m1 [,-c(3,5,7,8, 10, 12, 13)]


m2<-read.csv("Raw YSI Data/Mid_3_23_2017_4_19_2017.csv", header=FALSE)
m2<-m2 [-(1:7),]
colnames(m2)<-c_names
m2<-m2 [,-c(3,5,7,8, 10, 12, 13)]

m3<-read.csv("Raw YSI Data/Mid_4_19_2017_5_3_2017.csv", header=FALSE)
m3<-m3 [-(1:7),]
colnames(m3)<-c_names
m3<-m3 [,-c(3,5,7,8, 10, 12, 13)]

m4<-read.csv("Raw YSI Data/Mid_5_3_2017_5_18_2017.csv", header=FALSE)
m4<-m4 [-(1:7),]
colnames(m4)<-c_names
m4<-m4 [,-c(3,5,7,8, 10, 12, 13)]


#These are files Tom Downloaded from the old server
m5<-read.csv("Raw YSI Data/Mid Pt 050117 to 053017_EcoNet.csv", header = FALSE)
co_names<-as.vector(t(m5)[,4])
m5<-m5 [-(1:10),]
colnames(m5)<-co_names
m5<-m5 [,-9]
View(m6)

m6<-read.csv("Raw YSI Data/Mid Pt 060117 to 063017_EcoNet.csv", header = FALSE)
m6<-m6 [-(1:10),]
colnames(m6)<-co_names
m6<-m6[,-9]

#These are the new server files
#Report from Tom Wazniak 8/21 stated Mid Station didn't come online until June 19

m7<-read.csv("Raw YSI Data/export-xyle510.Dominion Power.MidPoint.Campbell PakBus Logger.csv", header = FALSE)
colnames(m7)<-c("DateTime", "x1", "x2","x3", "Depth", "ODO%", "ODO Conc", "Salinity", "SpCond", 
                "Temp", "x4", "x5", "x6")
m7<-m7 [-(1:3),]
m7<-m7 %>% 
  select(-starts_with("x"))
m7[m7 == ""]<-NA
m7$DateTime<-mdy_hm(m7$DateTime)
m7<- m7 %>% 
  filter(DateTime >="2017-06-19 04:45:00")
m7<-na.omit(m7)
View(m7)

#Bind everything together
Mid<- m1 %>% 
  bind_rows(., m2) %>% 
  bind_rows(., m3) %>% 
  bind_rows(., m4) %>% 
  bind_rows(., m5) %>%
  bind_rows(., m6) %>% 
  distinct()
View(Mid) 

Mid$DateTime<- paste(Mid$Date, Mid$Time, sep = " ")
Mid$DateTime<-mdy_hm(Mid$DateTime)

Mid<- Mid %>% 
  select(-Date, -Time) %>% 
  bind_rows(., m7) %>% 
  select("DateTime", "Temp", "SpCond", "Salinity", "Depth", "ODO%", "ODO Conc") %>% 
  rename(ODO = "ODO%", ODO_Conc = "ODO Conc")

Mid$Station<- "Mid"

write.csv(Mid, file= "Raw YSI Data/Mid2017.csv")

####North Site####
#These are files CBL Downloaded from the old server
n1 <- read.csv("Raw YSI Data/North_1_1_2017_3_23_2017.csv", header=FALSE)
c_names<-as.vector(t(n1)[,6])
n1<-n1 [-(1:7),]
colnames(n1)<-c_names
n1[n1 == ""]<-NA
n1<-n1 [,-c(3,8)]
View(n1)

n2<-read.csv("Raw YSI Data/North_3_23_2017_4_5_2017.csv", header=FALSE)
colnames(n2)<-as.vector(t(n2)[,6])
n2<-n2 [-(1:7),]

n3<-read.csv("Raw YSI Data/North_4_5_2017_4_19_2017.csv", header=FALSE)
colnames(n3)<-as.vector(t(n3)[,6])
n3<-n3 [-(1:7),]
n3<-n3 [,-c(3,8)]

n4<-read.csv("Raw YSI Data/North_4_19_2017_5_3_2017.csv", header=FALSE)
colnames(n4)<-as.vector(t(n4)[,6])
n4<-n4 [-(1:7),]

n5<-read.csv("Raw YSI Data/North_5_3_2017_5_18_2017.csv", header=FALSE)
colnames(n5)<-as.vector(t(n5)[,6])
n5<-n5 [-(1:7),]
n5<-n5 [,-c(3,8)]

#These are files Tom Downloaded from the old server
n6<-read.csv("Raw YSI Data/North Pt 050117 to 053017_EcoNet.csv", header = FALSE)
co_names<-as.vector(t(n6)[,4])
n6<-n6 [-(1:10),]
colnames(n6)<-co_names
met6<-n6 [,c(1:2,9:17)]
n6<-n6 [,-c(9:18)]

n7<-read.csv("Raw YSI Data/North Pt 060117 to 063017_EcoNet.csv", header = FALSE)
co_names<-as.vector(t(n7)[,4])
n7<-n7 [-(1:10),]
colnames(n7)<-co_names
met7<-n7 [,c(1:2,9:17)]
n7<-n7 [,-c(9:18)]


#These are the new server files
#Report from Tom Wazniak 8/21 stated North Station didn't come online until June 19

n8<-read.csv("Raw YSI Data/export-xyle510.Dominion Power.NorthPoint.Campbell PakBus Logger.csv", header = FALSE)
colnames(n8)<-c("DateTime", "x1", "Depth", "ODO%", "ODO Conc", "Salinity", "SpCond", 
                "Temp", "x4")
n8<-n8 [-(1:3),]
n8<-n8 %>% 
  select(-starts_with("x"))
n8[n8 == ""]<-NA
n8$DateTime<-ymd_hms(n8$DateTime)
n8<- n8 %>% 
  filter(DateTime >="2017-06-19 04:45:00")
n8<-na.omit(n8)
View(n8)

#Bind everything together
North<- n1 %>% 
  bind_rows(., n2) %>% 
  bind_rows(., n3) %>% 
  bind_rows(., n4) %>% 
  bind_rows(., n5) %>%
  bind_rows(., n6) %>% 
  bind_rows(., n7) %>% 
  distinct()
View(North) 

North$DateTime<- paste(North$Date, North$Time, sep = " ")
North$DateTime<-mdy_hm(North$DateTime)

North<- North %>% 
  select(-Date, -Time) %>% 
  bind_rows(., n8) %>% 
  select("DateTime", "Temp", "SpCond", "Salinity", "Depth", "ODO%", "ODO Conc") %>% 
  rename(ODO = "ODO%", ODO_Conc = "ODO Conc")

North$Station<- "North"

write.csv(North, file= "Raw YSI Data/North2017.csv")

####MET STATION ####
#These are files CBL Downloaded from the old server
met1<-read.csv("Raw YSI Data/Met_1_1_2017_3_23_2017.csv", header = FALSE)
m_names<-as.vector(t(met1)[,6])
met1<-met1 [-(1:7),]
colnames(met1)<-m_names

met2<-read.csv("Raw YSI Data/Met_3_23_2017_4_5_2017.csv", header = FALSE)
met2<-met2 [-(1:7),]
colnames(met2)<-m_names

met3<-read.csv("Raw YSI Data/Met_4_5_2017_4_19_2017.csv", header = FALSE)
met3<-met3 [-(1:7),]
colnames(met3)<-m_names

met4<-read.csv("Raw YSI Data/Met_4_19_2017_5_3_2017.csv", header = FALSE)
met4<- met4 [-(1:7),]
colnames(met4)<-m_names

met5<-read.csv("Raw YSI Data/Met_5_3_2017_5_18_2017.csv", header = FALSE)
met5<-met5 [-(1:7),]
colnames(met5)<-m_names

#met6 and met7 were made above when the North Site was wrangled from data provided by YSI from the old system
View(met6)
View(met7)

#This is from the new system
#Report from Tom Wazniak 8/21 stated North Station didn't come online until June 19

met8<-read.csv("Raw YSI Data/export-xyle510.Dominion Power.NorthPoint.csv", header = FALSE)
colnames(met8)<-c("DateTime", "Air Temp", "BP", "Hail Intensity", "Rain Fall", "RH", "Wind Dir", 
                        "Wind Speed", "Wind Speed Max", "Wind Speed Min", "x1", "x2")
met8<-met8 [-(1:3),]
met8<- met8 %>% 
  select(-starts_with("x"))
met8$DateTime<-ymd_hms(met8$DateTime)
met8<- met8 %>% 
  filter(DateTime >="2017-06-19 04:45:00")
View(met8)

#Bind everything together
MET<- met1 %>% 
  bind_rows(., met2) %>% 
  bind_rows(., met3) %>% 
  bind_rows(., met4) %>% 
  bind_rows(., met5) %>%
  bind_rows(., met6) %>% 
  bind_rows(., met7) %>% 
  distinct()
View(MET) 

MET$DateTime<- paste(MET$Date, MET$Time, sep = " ")
MET$DateTime<-mdy_hm(MET$DateTime)

MET<- MET %>% 
  select(-Date, -Time) %>% 
  bind_rows(., met8) %>% 
  select("DateTime", "Air Temp", "RH", "BP", "Rain Fall", "Hail Intensity", 
         "Wind Dir", "Wind Speed Min", "Wind Speed", "Wind Speed Max")%>% 
  rename(Air_Temp = "Air Temp", Rainfall = "Rain Fall", Hail = "Hail Intensity", 
         Wind_Dir = "Wind Dir", Wind_Min = "Wind Speed Min", Wind_Max = "Wind Speed Max", Wind_spd = "Wind Speed" )

  



write.csv(MET, file= "Raw YSI Data/MET2017.csv", row.names=FALSE)


####Reload saved CSV to get everything into a format that makes sense####
Mid<-read.csv("Raw YSI Data/Mid2017.csv")
Mid$DateTime<-ymd_hms(Mid$DateTime)
####Plot to Check to see if it is all screwy####
a<-Mid %>% 
  ggplot(aes(x= DateTime))
ab<-a+
  geom_point(aes(y = SpCond), color="blue")+
  geom_point(aes(y = Temp), color="red")+
  geom_point(aes(y = Salinity), color="orange")+
  geom_point(aes(y = ODO_Conc), color="green")+
  geom_point(aes(y = ODO), color="purple")+
  geom_point(aes(y = Depth), color="pink")

####Join all Water Quality Stations for 2017####
North<-read.csv("Raw YSI Data/North2017.csv")
Mid<-read.csv("Raw YSI Data/Mid2017.csv")
South<-read.csv("Raw YSI Data/South2017.csv")

wq_2017<-North %>% 
  bind_rows(., Mid) %>%  
  bind_rows(., South) %>% 
  select(-X)
wq_2017$DateTime<-ymd_hms(wq_2017$DateTime)
wq_2017$Station<-factor(wq_2017$Station, levels = c("Hobo", "North", "Mid", "South"))
write.csv(wq_2017, "Raw YSI Data/wq_2017", row.names=FALSE)

#remove crazy outlier point that had a reading of 100 degrees celsius
wq_2017<-wq_2017 %>% 
  filter(!Temp>=50)

rm(s1, s2, s3, s4, s5, s6, s7, s8, s9)
rm(n1, n2, n3, n4, n5, n6, n7, n8)
rm(met1, met2, met3, met4, met5, met6, met7, met8)
rm(m1, m2, m3, m4, m5, m6, m7)
rm(hobo1)
rm(a, ab)

