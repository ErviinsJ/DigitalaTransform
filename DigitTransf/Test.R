RawData <- read.csv("C:/Users/Ervins/Downloads/uber-raw-data-aug14.csv")
View(RawData)

####
plot(RawData$Lat,RawData$Lon,
     xlab='Lat',
     ylab = 'Lon',
     pch = 19)

####
install.packages('rworldmap',dependencies=TRUE) 
library(rworldmap)
newmap <- getMap(resolution = "low")

plot(newmap, xlim = c(-75, -73), ylim = c(38, 42), asp = 1)



points(RawData$Lon, RawData$Lat, col = "red", cex = .6)

install.packages('ggmap',dependencies=TRUE)
install.packages('ggplot2',dependencies=TRUE)
install.packages('rlang',dependencies=TRUE)
library(ggplot2)
library(ggmap)
map <- get_map(location = 'North America', zoom = 4)



##########

library(lubridate)
date<-ymd_hms(RawData[5,1])
wday(date)
date<-RawData[5,]$Date.Time

date<-Data[,1]
time<-Data[,4]
Data[6:8,5]


weekdays(RawData$Date.Time[5,1])

i<-0



for(w in 1:7){
  
  for(h in c(8,12,17,21)){
    
    for(l in 1:10){
      hour<- h
      day <- w
      lat<- round(runif(1, 40.7300, 40.7500),2)
      lon<- round(runif(1, -73.9999, -73.9700),2)
      
      count0 <- 0
      count1 <- 0
      ch1 <- 0
      ch2 <- 0
      ch5 <- 0
      
      for (i in 1:nrow(Data)){
        if (Data$Weekday[i]==day & round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
          if(Data$hour[i]==hour){
            ch1 <- ch1+1
            ch2 <- ch2+1
            ch5 <- ch5+1
          }else if(Data$hour[i]==(hour+1)){
            ch2 <- ch2+1
            ch5 <- ch5+1
          }else if(Data$hour[i]==(hour+2)|Data$hour[i]==(hour+3)|Data$hour[i]==(hour+4)){
            ch5 <- ch5+1
          }
        }
      }
      
      
      for (i in 1:nrow(Data)){
        if ((Data$hour[i]>=hour)&(Data$Weekday[i]==day)& round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
          count0 <- count0 +1
        }else if ((Data$hour[i]<=12) &(Data$Weekday[i]==(day+1)%%7)& round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
          count1 <- count1 +1
        }
      }
      
      df <- rbind(df, c(ch1, ch2, ch5, count0, count1))
    }
  }
}
############

park_time_estimate <- function(hour, day, lat, lon) {
  
  lat <- round(lat, 2)
  lon <- round(lon, 2)
  
  count0 <- 0
  count1 <- 0
  ch1 <- 0
  ch2 <- 0
  ch5 <- 0
  
  for (i in 1:nrow(Data)){
    if (Data$Weekday[i]==day & round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
      if(Data$hour[i]==hour){
        ch1 <- ch1+1
        ch2 <- ch2+1
        ch5 <- ch5+1
      }else if(Data$hour[i]==(hour+1)){
        ch2 <- ch2+1
        ch5 <- ch5+1
      }else if(Data$hour[i]==(hour+2)|Data$hour[i]==(hour+3)|Data$hour[i]==(hour+4)){
        ch5 <- ch5+1
      }
    }
  }
  
  
  for (i in 1:nrow(Data)){
    if ((Data$hour[i]>=hour)&(Data$Weekday[i]==day)& round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
      count0 <- count0 +1
    }else if ((Data$hour[i]<=12) &(Data$Weekday[i]==(day+1)%%7)& round(Data$Lat[i], 2)==lat & round(Data$Lon[i],2)==lon){
      count1 <- count1 +1
    }
  }
  
  ##pagaidām dala ar dienu skaitu datu kopā 
  
  ch1 <- (ch1/12)*100
  ch2 <- (ch2/12)*100
  ch5 <- (ch5/12)*100
  count0 <- (count0/12)*100
  count1 <- (count1/12)*100
  
  if(ch1>100){ch1 <- 100}
  if(ch2>100){ch2 <- 100}
  if(ch5>100){ch5 <- 100}
  if(count0>100){count0 <- 100}
  if(count1>100){count1 <- 100}
  
  
  
  ## Būtu nepieciešams vēl pieejamo mašīnu skaits apgamabalā, kas samazina varbūtību 
  
  returnList <- list("stundas %" = ch1, "2 stundas %" = ch2, "5 stundas %" = ch5, "dienas %" = count0, "nākamās dienas %" = count1)
  return(returnList)
}


park_time_estimate(16,3, 40.7521, -73.9832)





df <- data.frame("ch1" = c(1), "ch2" = c(3), "ch5" = c(4), "d1"= c(15),"d2" = c(8))


##############



install.packages('openxlsx',dependencies=TRUE)
library(openxlsx)

write.xlsx(df, file = "writeXLSX2.xlsx", colNames = TRUE, borders = "surrounding")
