####library####
library(dplyr)
library(lubridate)
library(ggplot2)

#####load HOBO Water Level data#####
#May be missing Nov 19 - Nov 30 Look later
wl1 <- read.delim("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/WL_12_19_2017_02_14_2018.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE)
wl1<-wl1[-1,-1]
wl_head<-c("datetime", "kPA", "temp", "barokPA", "depth")
colnames(wl1)<-wl_head
wl1$kPA<-as.character(wl1$kPA)
wl1$temp<-as.character(wl1$temp)
wl1$barokPA<-as.character(wl1$barokPA)
wl1$depth<-as.character(wl1$depth)
View(wl1)

wl2 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_02_28_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl2<-wl2[-c(1:2),-1]
colnames(wl2)<-wl_head
wl2<-wl2[!is.na(wl2$depth),]
View(wl2)

wl3 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_03_15_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl3<-wl3[-c(1:2),-1]
colnames(wl3)<-wl_head
wl3<-wl3[!is.na(wl3$depth),]
View(wl3)

wl4 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/10051488_CPWeir_04_23_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl4<-wl4[-c(1:2),-1]
colnames(wl4)<-wl_head
wl4<-wl4[!is.na(wl4$depth),]
View(wl4)

wl5 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_05_11_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl5<-wl5[-c(1:2),-1]
colnames(wl5)<-wl_head
wl5<-wl5[!is.na(wl5$depth),]
View(wl5)

wl6 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_05_24_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl6<-wl6[-c(1:2),-1]
colnames(wl6)<-wl_head
wl6<-wl6[!is.na(wl6$depth),]
View(wl6)

wl7 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_07_02_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl7<-wl7[-c(1:2),-1]
colnames(wl7)<-wl_head
wl7<-wl7[!is.na(wl7$depth),]
View(wl7)

wl8 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_09_06_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl8<-wl8[-c(1:2),-c(1,7:10)]
colnames(wl8)<-wl_head
View(wl8)

wl9 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_09_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl9<-wl9[-c(1:2),-c(1,7:10)]
colnames(wl9)<-wl_head
wl9<-wl9[!is.na(wl9$depth),]
View(wl9)

wl10 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_10_04_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl10<-wl10[-c(1:2),-1]
colnames(wl10)<-wl_head
View(wl10)

wl11 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/2018_10_19_Weir.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl11<-wl11[-c(1:2),-1]
colnames(wl11)<-wl_head
View(wl11)

wl12 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_11_19_2018.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl12<-wl12[-c(1:2),-1]
colnames(wl12)<-wl_head
View(wl12)

wl13 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/CPWeir_01_18_2019.csv", header=FALSE, na.strings="", stringsAsFactors=FALSE)
wl13<-wl13[-c(1:2),-1]
wl13$datetime<-paste0(wl13$V2, " ", wl13$V3)
colnames(wl13)<-c("date", "time", "kPA", "temp", "barokPA", "depth", "datetime")
View(wl13)
#####Join Hobo Water Level Data####

hobo_wl<-full_join(wl1,wl2) %>% 
  full_join(., wl3) %>% 
  full_join(., wl4) %>% 
  full_join(., wl5) %>% 
  full_join(., wl6) %>% 
  full_join(., wl7) %>% 
  full_join(., wl8) %>% 
  full_join(., wl9) %>% 
  full_join(., wl10) %>% 
  full_join(., wl11) %>% 
  full_join(., wl13) %>% 
  select(-date, -time)
View(hobo_wl)
saveRDS(hobo_wl, "hobo_wl_2019.rds")
