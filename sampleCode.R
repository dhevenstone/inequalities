#Goal, sample code for upload
#Date: 10.09.2020

rm(list = ls(all = TRUE))

library(data.table)
library(dplyr)
library(fst)
library(bit64)
install.packages("stringi")
library(stringi)
library("ggplot2")


#set directory where you saved data 
  setwd("/mnt/smb.hdd.rbd/S/sosi-famunemp-shared/dataForAnalysis/")

#read the data
  fread("sampleMarriedFake.csv", sep = ",", header= TRUE)-> ourData

#function to take a substring from the right
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

#create child birth year 
  ourData$yrchldB<-substrRight(ourData$bday_kid, 4)
  ourData$yrchldB<-as.integer(ourData$yrchldB)

#create clock for count from child birth year 
  ourData$clock <- ourData$year- ourData$yrchld 

#subset data for female parents 
  ourDataMoms<-ourData[parent==1 & sex.ego=="F"]

#plot income 
incByClock1 <- ourDataMoms %>% group_by(clock) %>% summarise_at(.vars=vars(incempl.ego),.funs=mean)
p<-
  ggplot(incByClock1, aes(x=clock, y=incempl.ego) )+
  theme(legend.position=c(.80, .20))+
  geom_line() +xlab("years since 1st birth")+ylab("earned income")+xlim(-5, 19)+
  theme_bw()
p


