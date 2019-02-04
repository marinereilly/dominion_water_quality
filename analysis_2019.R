####library####
library(dplyr)
library(lubridate)
library(ggplot2)

####load HOBO data####
wl1 <- read.delim("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/hobo/data/2018/csv/WL_12_19_2017_02_14_2018.csv", header=FALSE, comment.char="#", stringsAsFactors=FALSE)
wl1<-wl1[-1,-1]
wl_head<-c("datetime", "kPA", "temp", "barokPA", "depth")
colnames(wl1)<-wl_head
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
