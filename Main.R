#set working directory
setwd("C:/Users/Eamonn/Documents/Programing/R")

#packages
library(ggplot2)
library(chron)

#read csv's
avl1 <- read.csv("data/DublinBusGps/siri.20130101.csv")
avl2 <- read.csv("data/DublinBusGps/siri.20130102.csv")


shapes <- read.csv("data/dublinbusgtfs/shapes.txt")
routes <- read.csv("data/dublinbusgtfs/routes.txt")
gtfsStops <- read.csv("data/dublinbusgtfs/stops.txt")

#clean/subset data

#rename columns
colnames(shapes) <- c("shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")
colnames(avl1) <- c("timestamp","lineId","Direction","JourneyPatternID","timeframe","vehicleJourneyID","operator","congestion","lon","lat","delay","blockID","vehicleID","stopID","atStop")
colnames(avl2) <- c("timestamp","lineId","Direction","JourneyPatternID","timeframe","vehicleJourneyID","operator","congestion","lon","lat","delay","blockID","vehicleID","stopID","atStop")

#bind two data sets together
avl <-  rbind(avl1[,],avl2[,])


#format timestamp
datetime<-as.POSIXct((avl$timestamp)*0.000001, origin="1970-01-01", tz = "GMT")
date<-as.Date(datetime,format='%m/%d/%Y')
time<-strftime(datetime, format="%H:%M:%S")
#add new columns to avl datafram, time and date
avl$date <- date
#convert time to "times" frormat
avl$time <- chron(times=time)
#rename columns
colnames(avl) <- c("timestamp","lineId","Direction","JourneyPatternID","timeframe","vehicleJourneyID","operator","congestion","lon","lat","delay","blockID","vehicleID","stopID","atStop","date","time")


#retriever the AVL records for a single bus
oneVehicle<- avl[avl$vehicleID=='33521',]
#further subset - just avl records from a single bus when at a stop
atStop <- oneVehicle[oneVehicle$atStop==1,]



#get all unique stops and store them in a vector
stops<-unique(atStop$stopID)
#create a list to hold further smaller subset dataframes
busStops <- list()
for(row in stops){
  #print(row)
  #get atStop rows that match one stop
  aBusStop <- atStop[atStop$stopID == row,]
  
  #further subset data - separate different 
  #vehicle journeys from individual stops
  #this will loop through for every unique journey 
  journeys<-unique(aBusStop$vehicleJourneyID)
  for(journey in journeys){
    journeyStop <- aBusStop[aBusStop$vehicleJourneyID == journey,]
    
    #if journey stop variable only
    #has one recor its ignored - likely not legit stop
    if(nrow(journeyStop)>2){
      id<-paste(row,journey,sep="")
      #append timestopped to dataframe
      #calc time
      #get first time
      time1<- head(journeyStop$time,1)
      
      
      #create a timestopped column. 
      #It will append the total time the bus has been stopeed to every record
      #set $timestopped to the vector $time - the first time of the stop
      journeyStop$timeStopped<-(journeyStop$time-time1)
      journeyStop$initialDelay<- head(journeyStop$delay,1)
      #append to subset dataframe containing
      #one stop by one bus one journey to list of stops 
      busStops[[id]]<- journeyStop
    }
    
  }
}

#create new datafram containing each busstop including total time stopped
busStopsTable <- data.frame()
for(i in busStops){
  #bind the tail record of each object(i) in busStops to busStopsTable
   busStopsTable <- rbind(busStopsTable,tail(i,1))
}

uniqueStopID <- unique(busStopsTable$stopID)
meanStopTimes <- c()
numberOfStops <- c()
lons <- c()
lats <- c()
for(i in uniqueStopID){
  #get vector of time stooped for one stop id
  these <- busStopsTable[busStopsTable$stopID == i,]
  #stor these in vector
  meanStopTimes<- append(meanStopTimes,mean(these$timeStopped))
  numberOfStops<- append(numberOfStops,length(these$timeStopped))
  
  #get the lat long coors - they should be all the same
  #get mean in case there is variance
  lons<- append(lons,mean(these$lon))
  lats<- append(lats,mean(these$lat))
}

#new data frame containing stop id, number of stops, avg stop time, lat,lon
stoppingTimes<-data.frame(stopID =uniqueStopID,noOfStops=numberOfStops, avgStopDuration = meanStopTimes,lon=lons, lat=lats)                             

                             
#plots


p <- ggplot() +
  geom_path(data=shapes, aes(shape_pt_lon, shape_pt_lat, group= shape_id), size=.2, alpha=.1) +
  geom_point(data=oneVehicle, aes(lon,lat), size=.1, colour="blue", alpha =.3)+
  geom_point(data=stoppingTimes[stoppingTimes$avgStopDuration >'00:05:00',], aes(lon, lat),size = 3,  colour="red")+
  geom_point(data=stoppingTimes[stoppingTimes$avgStopDuration <'00:05:00' & stoppingTimes$avgStopDuration >'00:02:00',], aes(lon, lat),size = 3,  colour="yellow")+
  geom_point(data=stoppingTimes[stoppingTimes$avgStopDuration <'00:02:00',], aes(lon, lat),size = 3,  colour="green")+
  coord_equal()
p










