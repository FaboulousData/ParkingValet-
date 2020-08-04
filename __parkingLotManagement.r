ParkingLotManagement<-function(db, CurrentDateTime, ParkParkingEvent, BindParkingEvent, CurrentParkPullingEvent)
{
  # Author: Fabrizio Miorelli
  # Licence: GNU General Public License v3.0 with restrictions
  # Usage: training, learning
  #
  # Language: R
  # Packages: rbase
  #
  #
  # This file is a part of the project ParkingValet+. The aim is to develop some applications for training and personal learning purposes. 
  # I do not intend to provide any part of code or script or file for commercial use. 
  # I do not own any responsability for the misuse of the scripts/data/files in this project nor I do own any resposability for the data in it.
  #
  #
  # The aim of this script is to manage parking and pulling events, assigning free parking lots and recording occupied parking lots
  #
  # INPUT: db (RODBC database connection), CurrenDateTime (POSIXct date and time), BindParkingEvent (data-frame, all ParkingEvents), CurrentParkingEvent (data-frame, current parking events)
  # oUTPUT: ParkParkingEvent 
  
  
  
  ParkingLot<-sqlQuery(db, 'SELECT * FROM Park.ParkingLot')
  
  
 
  ## 1) ASSIGNING PARKING LOTS: if CurrentParkPullingEvent is NULL (not yet created), assign available parking Lots
  
  if(is.null(CurrentParkPullingEvent))
  {
    
    # 1.1) FIRST DAY OF OPENING: assigning available parkingLots (all parkingLots are still available) 
    
    if(ParkParkingEvent$ParkingEventID[1]==1)
    {
      n<-nrow(ParkParkingEvent)
      x<-sample(ParkingLot$ParkingLotID, n)
      ParkParkingEvent$ParkingLotID<-x
      print(ParkParkingEvent)
      tables<-sqlTables(db)
      
      if("OccupiedParkingLot" %in% tables$TABLE_NAME)
      {
        sqlDrop(db, sqtable="Park.OccupiedParkingLot")
      }
      
      OccupiedParkingLot<-subset(ParkParkingEvent, select=c(ParkingLotID, Pulling_DateTime))
      OccupiedParkingLot$Pulling_DateTime<-as.character(OccupiedParkingLot$Pulling_DateTime)
      
      sqlSave(db, OccupiedParkingLot, tablename="Park.OccupiedParkingLot", verbose=TRUE, rownames=FALSE)
      
      print('--- FIRST DAY OF OPENING: all parking lots were available ---')
      
      exportResults(CurrentDateTime, OccupiedParkingLot,'OccupiedParkingLot_FIRSTDAY')
      return(ParkParkingEvent)
    }
    
    
    # 1.2) SUBSEQUENT DAYS OF OPENING: assigning available parkingLots (extract occupied parkingLots and consider only available parkingLots)
    
    if(ParkParkingEvent$ParkingEventID[1]!=1)
    {
      m<-nrow(ParkParkingEvent)
      occupiedParkingLot<-sqlQuery(db, 'SELECT opl.ParkingLotID FROM Park.OccupiedParkingLot AS opl ORDER BY opl.ParkingLotID ASC')
      availableParkingLot<-subset(ParkingLot, !(ParkingLot$ParkingLotID %in% occupiedParkingLot$ParkingLotID), select=ParkingLotID)

      # Available parking lots are insufficient
      if(m>nrow(availableParkingLot))
      {
        k<-nrow(availableParkingLot)
        
        ParkParkingEvent<-ParkParkingEvent[k,]
        x<-sample(availableParkingLot$ParkingLotID,m)
        ParkParkingEvent$ParkingLotID<-x
        print(cat('--- SUBSEQUENT DAYS OF OPENING: only the first [',eval(k),'] ParkingEvents rows have been considered ---'))
        print(cat('--- [',eval(m-k),'] ParkingEvents have been REJECTED ---'))
        
        # creating data.frame with new occupied parkingLot, PullingDates and append it to the Park.OccupiedParkingLot table
        dates<-subset(ParkParkingEvent, ParkParkingEvent$ParkingLotID %in% x, select=Pulling_DateTime)  #@ordine
        dates<-as.POSIXct(dates$Pulling_DateTime)
        
        y<-data.frame(ParkingLotID=integer(length(x)), Pulling_DateTime=as.POSIXct('1900-01-01 01:01:01'))
        y$ParkingLotID<-x
        y$Pulling_DateTime<-dates
        
        sqlSave(db, y, tablename="Park.OccupiedParkingLot", verbose=TRUE, rownames=FALSE, append=TRUE)
        exportResults(CurrentDateTime, y,'OccupiedParkingLot_firstLoad')
        
        return(ParkParkingEvent)
        
      }
      
      # available parking lots are sufficient
      if(m<=nrow(availableParkingLot))
      {
        # assigning available ParkingLots
        x<-sample(availableParkingLot$ParkingLotID,m)
        ParkParkingEvent$ParkingLotID<-x
        print('--- SUBSEQUENT DAYS OF OPENING: all parkingEvent rows have been considered')
        
        # creating data.frame with new occupied parkingLot, PullingDates and append it to the Park.OccupiedParkingLot table
        dates<-subset(ParkParkingEvent, ParkParkingEvent$ParkingLotID %in% x, select=Pulling_DateTime)  #@ordine
        dates<-as.POSIXct(dates$Pulling_DateTime)
        
        y<-data.frame(ParkingLotID=integer(length(x)), Pulling_DateTime=as.POSIXct('1900-01-01 01:01:01'))
        y$ParkingLotID<-x
        y$Pulling_DateTime<-dates
        
        sqlSave(db, y, tablename="Park.OccupiedParkingLot", verbose=TRUE, rownames=FALSE, append=TRUE)
        exportResults(CurrentDateTime, y,'OccupiedParkingLot_firstLoad')
        
        return(ParkParkingEvent)
      }
    }
    
    
  }
  
  
  # 2) REMOVE EMPTY PARKING LOTS FROM OccupiedParkingLots: if CurrentParkPullingEvent is NOT NULL (it has already been created), update occupiedParkingLot removing vacated ParkingLots (from CurrentParkPullingEvent) 
  
  if(!(is.null(CurrentParkPullingEvent) & is.null(BindParkingEvent)))
  {
    availableParkingLot<-subset(BindParkingEvent, BindParkingEvent$ParkingEventID %in% CurrentParkPullingEvent$ParkingEventID, select=ParkingLotID)
    
    # retrieve pulling parking lots and update occupiedParking Lots without them
    occupiedParkingLot<-sqlQuery(db, 'SELECT * FROM Park.OccupiedParkingLot AS opl ORDER BY opl.ParkingLotID ASC')
    occupiedParkingLot<-subset(occupiedParkingLot, !(occupiedParkingLot$ParkingLotID %in% availableParkingLot$ParkingLotID))
    sqlDrop(db, sqtable="Park.OccupiedParkingLot")
    sqlSave(db, occupiedParkingLot, tablename="Park.OccupiedParkingLot", verbose=TRUE, rownames=FALSE, append=TRUE)
    exportResults(CurrentDateTime, occupiedParkingLot,'OccupiedParkingLot_update')
  }
}