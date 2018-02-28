#####Packages#####
#####Packages#####
library(tidyverse)
library(dadjoke)
library(lubridate)

#####Load data#####
t1 <- read.csv("tidal_data/CO-OPS__8577330__wl (2).csv", stringsAsFactors=FALSE)
t2 <-read.csv("tidal_data/CO-OPS__8577330__wl (3).csv", stringsAsFactors=FALSE)
t3 <-read.csv("tidal_data/CO-OPS__8577330__wl (4).csv", stringsAsFactors=FALSE)
t4 <-read.csv("tidal_data/CO-OPS__8577330__wl (5).csv", stringsAsFactors=FALSE)
t5 <-read.csv("tidal_data/CO-OPS__8577330__wl (15).csv", stringsAsFactors=FALSE)
t6 <-read.csv("tidal_data/CO-OPS__8577330__wl (8).csv", stringsAsFactors=FALSE)
t7 <-read.csv("tidal_data/CO-OPS__8577330__wl (16).csv", stringsAsFactors=FALSE)
t8 <-read.csv("tidal_data/CO-OPS__8577330__wl (9).csv", stringsAsFactors=FALSE)
t9 <-read.csv("tidal_data/CO-OPS__8577330__wl (11).csv", stringsAsFactors=FALSE)
t10 <-read.csv("tidal_data/CO-OPS__8577330__wl (18).csv", stringsAsFactors=FALSE)
t11 <-read.csv("tidal_data/CO-OPS__8577330__wl (12).csv", stringsAsFactors=FALSE)
t12 <-read.csv("tidal_data/CO-OPS__8577330__wl (14).csv", stringsAsFactors=FALSE)

tide <-t1 %>% 
  bind_rows(., t2) %>% 
  bind_rows(., t3) %>% 
  bind_rows(., t4) %>% 
  bind_rows(., t5) %>% 
  bind_rows(., t6) %>% 
  bind_rows(., t7) %>% 
  bind_rows(., t8) %>% 
  bind_rows(., t9) %>% 
  bind_rows(., t10) %>% 
  bind_rows(., t11) %>% 
  bind_rows(., t12) 
rm(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)

tide$Date.Time<-ymd_hm(tide$Date.Time)

