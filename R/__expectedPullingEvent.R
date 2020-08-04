expectedPullingEvent<-function(BindParkingEvent,CurrentDateTime)
{
  # Author: Fabrizio Miorelli
  # Licence: GNU General Public License v3.0 with restrictions
  # Purpose: only educational, training.
  #
  # Language: R
  # Packages: rbase, RODBC
  #
  #
  # This file is a part of the project ParkingValet+. The aim is to develop some applications for training and personal learning purposes. 
  # I do not intend to provide any part of code or script or file for commercial use. 
  # I do not own any responsability for the misuse of the scripts/data/files in this project nor I do own any resposability for the data in it.
  #
  #
  # The aim of this script is to develop a procedure for retrieving expected pulling events based on the date of pulling in the parking events
  #
  # INPUT: BindParkingEvent (dataframe, newly created parking events), CurrentDateTime (dataframe, system Date time)
  # oUTPUT: out (dataframe) with ParkingEventID, PullingDateTime, BindParkingEvent
  
  day<-format(strptime(CurrentDateTime, "%Y-%m-%d %H:%M:%S"),'%d')
  month<-format(strptime(CurrentDateTime, "%Y-%m-%d %H:%M:%S"),'%m')
  year<-format(strptime(CurrentDateTime, "%Y-%m-%d %H:%M:%S"),'%Y')
  
  n<-nrow(BindParkingEvent)
  
  out<-data.frame(ParkingEventID=integer(n), ParkingLot=integer(n), Pulling_DateTime=as.POSIXct("2000-01-01 01:01:01"))
  
  for(i in 1:nrow(BindParkingEvent))
  {
    dayP<-format(strptime(BindParkingEvent$Pulling_DateTime[[i]], "%Y-%m-%d %H:%M:%S"),'%d')
    monthP<-format(strptime(BindParkingEvent$Pulling_DateTime[[i]], "%Y-%m-%d %H:%M:%S"),'%m')
    yearP<-format(strptime(BindParkingEvent$Pulling_DateTime[[i]], "%Y-%m-%d %H:%M:%S"),'%Y')
    
    
    if(dayP==day & monthP==month & yearP==year)
    {
      out$ParkingEventID[[i]]<-BindParkingEvent$ParkingEventID[[i]]
      out$ParkingLot[[i]]<-BindParkingEvent$ParkingLotID[[i]]
      out$Pulling_DateTime[i]<-as.character(as.POSIXct(BindParkingEvent$Pulling_DateTime[[i]]))
      
    }
  }
  
  out<-subset(out, out$ParkingEventID!=0)
  return(out)
}
