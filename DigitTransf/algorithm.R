
park_time_estimate(16,3, 40.7521, -73.9832)

##################

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


