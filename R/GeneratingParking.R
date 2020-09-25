'============================================================================================================================================================='
'======================================== ParkingValet+: creating, managing and exporting parking events =================================================='
'============================================================================================================================================================='
#
# Author: Fabrizio Miorelli
# Licence: GNU General Public License v3.0 with restrictions
# Purpose: only educational, training.
#
# Language: R
# Packages: rbase, RODBC
#
# Functions: 
# expectedPullingEvent.r
# parkingLotManagement.r
# exportResults.r"
# invoiceIssuing.r"
# biasedSampling.r"
#
#
# The aim of this project is to develop some application for training and personal learning purposes. 
# I do not intend to provide any part of code or script or file for commercial use. 
# I do not own any responsibility for the misuse of the scripts/data/files in this project nor I do own any resposibility for the data in it.
#
#
#
#
# The aim of this script is to generate daily parking events for a simulated parking lot management system based on a MSSQL database, a SSIS ETL procedure for importing
# data and a PowerBI visualization tool for data analysis.
# The basic idea was taken from an excercise published on edX (course code DAT251x) and has been further developed with some personal features and hypothesis.
# Here below are the main features of the script.
#
# 1) I made use of a BIASED SAMPLING PROCEDURE (based on a gamma distribution with random shape parameters) to generate fake attributes and data. 
#    I'm not sure about the properties of the gamma distribution incrementally as n increases (lim n-->+inf). 
#    The choice of using a biased sampling procedure was due to the properties of the 'sample' function of R, which randomly picks values assigning
#    a uniform distribution (equally distributed events)
#
# 2) the script has been developed to manage:
#   - daily parking events (all at the same time)
#   - daily management of pulling events (taking into account only the day of pulling. Hours, minutes or seconds have not been considered)
#   - I do not choose how to place a car in a specific parking lots. This could be an idea for further improvements.
#   - daily management of occupied and empty parking lots (at the end of the day, once the pulling events have been created)
#   - an invoicing procedure based only on fixed rates which takes into account randomly generated quantity (linking the actual amount of time 
#   - to effective quantity/time of parking might be an idea for further improvements)


'#########################'
'0) Preliminary tasks'
'#########################'

rm(list=ls())


#setting work directory
setwd('E:/Progetti/ParkingValet+')

# loading libraries 
library(RODBC)


# loading Functions
source("__expectedPullingEvent.r")
source("__parkingLotManagement.r")
source("__exportResults.r")
source("__invoiceIssuing.r")
source("__biasedSampling.r")


# establishing db connection
## !! INSERT YOUR 
db<-odbcDriverConnect(connection='driver={SQL SERVER};
                                  server=DESKTOP-V6A73DH;
                                  database=ParkingValetPlus;
                                  uid=sa;
                                  pwd=1984;
                                  trusted_connection=TRUE',
                                  rows_at_time=1, case='nochange')


'#############################################################'
'1) PARKCUSTOMERVEICHLES: creating the CustomerVeichles table'
'#############################################################'

Customers<-sqlQuery(db, 'SELECT * FROM Park.Customers')
Veichles<-sqlQuery(db, 'SELECT * FROM Park.Veichles')


## How many days back would you like to generate the parking events?
########################################################################

daysback<-0

## How many ParkingEvents would you like to generate?
########################################################################
CurrentDate<-Sys.time()
x<-seq(30,200,10)

npae_prob<-read.csv2(file="2020-08-23_SamplingProbabilities-nPullingEevent.csv", header=FALSE, sep=",")
colnames(npae_prob)<-x
rownames(npae_prob)<-c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')

dayname<-weekdays.POSIXt(CurrentDate)

# For the sampling of n, assigning different probabilities vectors
sampling_prob=switch(
  dayname,
  'Sunday'= as.numeric(as.vector(npae_prob[1,])),
  'Monday'= as.numeric(as.vector(npae_prob[2,])),
  'Tuesday'= as.numeric(as.vector(npae_prob[3,])),
  'Wednesday'= as.numeric(as.vector(npae_prob[4,])),
  'Thursday'= as.numeric(as.vector(npae_prob[5,])),
  'Friday'= as.numeric(as.vector(npae_prob[6,])),
  'Saturday'= as.numeric(as.vector(npae_prob[7,])),
)

n<-sample(x,1,prob=sampling_prob)


## Creating ParkCustomerVeichles
#########################################################################

# Retrieving data from CustomerVeichles table
CustomersVeichles<-sqlQuery(db, 'SELECT * FROM Park.CustomersVeichles AS cv ORDER BY cv.CustomersVeichlesID ASC')
CustomersVeichlesID<-CustomersVeichles$CustomersVeichlesID

# Retrieving the last ID associated with CustomerVeichles records
lastID<-CustomersVeichlesID[[length(CustomersVeichlesID)]]
ID_new_first<-lastID+1

if(length(CustomersVeichlesID)==0 || is.null(lastID)==TRUE)
{
  ID_new_first<-1
}


ParkCustomersVeichles<-data.frame(CustomersVeichlesID=integer(n), CustomersID=integer(n), VeichlesID=integer(n))

# Sampling from Customer and Veichles tables
x<-biasedSampling(n,Customers$CustomersID,'asc','gamma',repl=FALSE)
y<-biasedSampling(n,Veichles$VeichlesID,'asc','gamma',repl=FALSE)

# Assigning values to ParkCustomerVeihcles dataframe
ParkCustomersVeichles$CustomersVeichlesID<-seq(ID_new_first,ID_new_first+n-1,1)
ParkCustomersVeichles$CustomersID<-x
ParkCustomersVeichles$VeichlesID<-y



'########################################################################################'
'2) PARKPARKINGEVENT: querying the databse to gather data for new ParkParkingEvent events'
'########################################################################################'

# QUerying the database to retrieve ParkingEvents, CSRv, Drivers and ParkingLot tables
ParkingEvent<-sqlQuery(db, 'SELECT * FROM Park.ParkingEvent AS pe ORDER BY pe.ParkingEventID ASC')
CSRv<-sqlQuery(db, 'SELECT * FROM Park.CSRv AS cs ORDER BY cs.CSRvID ASC')
Drivers<-sqlQuery(db, 'SELECT * FROM Park.Drivers AS dr ORDER BY dr.DriversID ASC')
ParkingLot<-sqlQuery(db, 'SELECT * FROM Park.ParkingLot AS pl ORDER BY pl.ParkingLotID ASC')


# Retrieving the last ID associated with ParkingEvents records and defining the first ID to be used
ParkingEventID<-ParkingEvent$ParkingEventID
lastID<-ParkingEventID[[length(ParkingEventID)]]
ID_new_first<-lastID+1

if(length(lastID)==0 || !(exists('lastID')))
{
  ID_new_first<-1
}

# Initialize the ParkParkingEvent dataframe
ParkParkingEvent<-data.frame(ParkingEventID=integer(n), ParkingLotID=integer(n), CustomersVeichlesID=integer(n), DriversID=integer(n), CSRvID=integer(n), CustodyLogID=integer(n), Parking_DateTime=character(n),	Pulling_DateTime=character(n), Entry_code=integer(n),	CheckFront=character(n),	CheckRear=character(n),	CheckRight=character(n),	CheckLeft=character(n),	Promocode=integer(n),	Privacy=character(n))

# Assigning ParkingEventIDs to the ParkParkingEvent dataframe
x<-seq(ID_new_first,ID_new_first+n-1,1)
ParkParkingEvent$ParkingEventID<-x

# Assigning CustomerVeichles to ParkParkingEvent
ParkParkingEvent$CustomersVeichlesID<-ParkCustomersVeichles$CustomersVeichlesID


# Sampling n DriversID, CSRv and assigning values to ParkParkingEvent
x<-biasedSampling(n,Drivers$DriversID,'asc','gamma',repl=FALSE)   
y<-biasedSampling(n,CSRv$CSRvID,'asc','gamma',repl=FALSE)
ParkParkingEvent$DriversID<-x
ParkParkingEvent$CSRvID<-y


# Filling Check--, Privacy, Promocode
ParkParkingEvent$CheckFront<-'Front'
ParkParkingEvent$CheckRear<-'Rear'
ParkParkingEvent$CheckRight<-'Right'
ParkParkingEvent$CheckLeft<-'Right'
ParkParkingEvent$Privacy<-sample(c('Yes','No'),n,replace=TRUE)




# Creating Parking_DateTimes and random Pulling_times
x<-Sys.time()-as.difftime(daysback,units='days')
y<-x+as.difftime(40*sample(1:200,n, replace=TRUE),units="mins")
y<-y+as.difftime(40*sample(-200:200,n,replace=TRUE),units="secs")

CurrentDateTime<-x

ParkParkingEvent$Parking_DateTime<-as.character(as.POSIXct(rep(x,n)))
ParkParkingEvent$Pulling_DateTime<-as.character(as.POSIXct(y))


# Generating random (uniform) Entry codes
ParkParkingEvent$Entry_code<-sample(100000000:999999999,n)


## PARKING LOT MANAGEMENT: assigning available parking lots and updating occupied pakring lot table
##################################################################################################
ParkParkingEvent<-ParkingLotManagement(db, CurrentDateTime, ParkParkingEvent, NULL, NULL)





'####################################################################################################################'
'3) PARKPULLINGEVENT: creating expected PullingEvents, issuing receipts and find available parkingLots'
'####################################################################################################################'

# Initialize dataframe: CurrentParkPullingEvent for currrent pulling events (expected pullingEvents for currentDate)
PullingEvent<-sqlQuery(db,'SELECT * FROM Park.PullingEvent ORDER BY PullingEventID ASC')

# Binding togheter stored ParkingEvents and new created parkingEvents to find all expected pulling events for the CurrentDate
# Applying the function __expectedPullingEvent to retrieve current date expected pulling events from total pulling events
ParkingEvent<-sqlQuery(db, 'SELECT * FROM Park.ParkingEvent')
BindParkingEvent<-rbind(ParkingEvent, ParkParkingEvent)

# dataframe with ParkingEventID, ParkingLotID and Date
outID<-expectedPullingEvent(BindParkingEvent, CurrentDateTime)


## CONTINUE ONLY IF outID has some results
if(nrow(outID)>=1)
{
  n<-nrow(outID)

  last_ID<-PullingEvent$PullingEventID[nrow(PullingEvent)]
  first_ID<-last_ID+1

  if(is.na(first_ID) || length(first_ID)==0)
  {
    first_ID<-1
  }

  ID<-seq(first_ID,first_ID+n-1,1)

  CurrentParkPullingEvent<-data.frame(PullingEventID=integer(n), ParkingEventID=integer(n),	CSRvID=integer(n),	InvoiceID=integer(n),	DriversID=integer(n),	Pulling_DateTime=as.POSIXct('1900-01-01 01:01:01'),	ExitCode=integer(n),	CheckFront=character(n),	CheckRear=character(n),	CheckRight=character(n),	CheckLeft=character(n),	Feedback=character(n),	Rating=character(n))


  # Filling CurrentParkPullingEvent
  for(i in 1:n)
  {
    CurrentParkPullingEvent$ParkingEventID[i]<-outID$ParkingEventID[i]
    CurrentParkPullingEvent$Pulling_DateTime[i]<-as.character(outID$Pulling_DateTime[i])
    CurrentParkPullingEvent$ExitCode[i]<-sample(100000000:999999999,1)
    CurrentParkPullingEvent$PullingEventID[i]<-ID[i]
    CurrentParkPullingEvent$CSRvID[i]<-biasedSampling(1,CSRv$CSRvID,'asc','gamma',repl=FALSE)
    CurrentParkPullingEvent$DriversID[i]<-biasedSampling(1,Drivers$DriversID,'asc','gamma',repl=FALSE)
    CurrentParkPullingEvent$CheckFront[i]<-'Front'
    CurrentParkPullingEvent$CheckRear[i]<-'Rear'
    CurrentParkPullingEvent$CheckRight[i]<-'Right'
    CurrentParkPullingEvent$CheckLeft[i]<-'Left'
    CurrentParkPullingEvent$Feedback[i]<-'-'
    CurrentParkPullingEvent$Rating[i]<-'-'
  
  }

  CurrentParkPullingEvent<-subset(CurrentParkPullingEvent, CurrentParkPullingEvent$PullingEventID!=0)
  CurrentParkPullingEvent



  '####################################################################################################################'
  '3) FISCALINVOICE: generating perceived services, issuing receipt and generating payment log'
  '####################################################################################################################'


  # calling the InvoiceIssuing Function: returns the FiscalInvoice dataframe and the FiscalInvoiceDetail dataframe (as a list)
  Fiscal<-invoiceIssuing(db, CurrentParkPullingEvent,BindParkingEvent)


  # updating FiscalInvoice with Subtotal, status and adding VAT if type of invoice is appropriate
  FiscalInvoice<-Fiscal$FiscalInvoice
  FiscalInvoiceDetail<-Fiscal$FiscalInvoiceDetail
  
  # delisting Sellprice (the attribute was built as a list of list)
  FiscalInvoiceDetail$SellPrice<-as.numeric(FiscalInvoiceDetail$SellPrice)

  FiscalInvoice$SubTotal<-Fiscal$Total$SubTotal

  FiscalInvoice$Status<-'Pending'

  for(i in 1:nrow(FiscalInvoice))
  {
    if(FiscalInvoice$TypeOfInvoiceID[i] %in% c(1,2,4,5))
    {
      FiscalInvoice$Taxes[i]<-FiscalInvoice$SubTotal[i]*0.22
    }
  }
  
  # assigning InvoiceID to CurrentParkParkingEvent
  CurrentParkPullingEvent$InvoiceID<-FiscalInvoice$InvoiceID

  '####################################################################################################################'
  '4) PAYMENTLOG: generating payment logs for invoices'
  '####################################################################################################################'

  # For each Invoice generating a PaymentLog
  PaymentLog<-sqlQuery(db, 'SELECT PaymentLogID FROM Fiscal.PaymentLog ORDER BY PaymentLogID ASC')
  lastID<-PaymentLog$PaymentLogID[nrow(PaymentLog)]
  ID_new_first<-lastID+1

  if(is.null(lastID) || length(ID_new_first)<1)
  {
    ID_new_first<-1
  }

  # Number of log to be generated
  n<-nrow(FiscalInvoice)

  ID<-seq(ID_new_first,ID_new_first-1+n)

  # Initializing PaymentLogs dataframe and filling it
  FiscalPaymentLog<-data.frame(PaymentLogID=integer(n),PaymentDateTime=as.POSIXct('1900-01-01 01:01:01'),SWIFTcode=character(n),Type=character(n),Outcome=character(n),Amount=double(n))
  
  FiscalPaymentLog$PaymentLogID<-ID
  FiscalPaymentLog$PaymentDateTime<-as.character(CurrentDateTime)
  FiscalPaymentLog$SWIFTcode<-rep(paste(paste(sample(LETTERS,4),sep="",collapse=''),paste(sample(LETTERS,2),sep="",collapse=''),paste(sample(LETTERS,2),sep="",collapse=''),sep=''),n)
  FiscalPaymentLog$Type<-sample(c('POS','VISA','MAESTRO','AMEX'),n,replace=TRUE)
  FiscalPaymentLog$Outcome<-sample(c('CONFIRMED','REJECTED'),n,replace=TRUE)
  FiscalPaymentLog$Amount<-FiscalInvoice$SubTotal
  
  # Assigning FiscalPaymentLogID to FiscalInvoice
  FiscalInvoice$PaymentLogID<-FiscalPaymentLog$PaymentLogID


  
  '####################################################################################################################'
  '5) CUSTODYLOG'
  '####################################################################################################################'
  CustodyLog<-sqlQuery(db,'SELECT * FROM Park.CustodyLog ORDER BY CustodyLogID ASC')
  lastID<-CustodyLog$CustodyLogID[nrow(CustodyLog)]
  ID_new_first<-lastID+1
  n<-nrow(ParkParkingEvent)
  
  if(is.null(lastID) || length(ID_new_first)<1)
  {
    ID_new_first<-1
  }
  ID<-seq(ID_new_first,ID_new_first+n-1)
  
  ParkCustodyLog<-data.frame(CustodyLogID=integer(n), DateTime_IN=integer(n), DateTime_Out=integer(n), Section=character(n), Locker=integer(n))
  
  ParkCustodyLog$CustodyLogID<-ID
  ParkCustodyLog$DateTime_IN<-ParkParkingEvent$Parking_DateTime
  ParkCustodyLog$DateTime_Out<-ParkParkingEvent$Pulling_DateTime
  ParkCustodyLog$Section<-'Not implemented'
  ParkCustodyLog$Locker<-sample(c(1,2,3,4),n,replace=TRUE)
  
  # Assigning CustodyLogID to ParkParkingEvent
  ParkParkingEvent$CustodyLogID<-ParkCustodyLog$CustodyLogID
  
  
  ## Updating the Park.OccupiedParkingLot: sending BindParkingEvent, CurrentParkPullingEvent to ParkingLotManagement in order to free actual pulling parking lots
  ####################################################################################################################
  ParkingLotManagement(db, CurrentDateTime, NULL, BindParkingEvent, CurrentParkPullingEvent)
  
  
  
  
  
  
  
  
  # adjusting the date to be a character
  CurrentParkPullingEvent$Pulling_DateTime<-as.character(CurrentParkPullingEvent$Pulling_DateTime)
  FiscalInvoice$IssueDate<-as.character(FiscalInvoice$IssueDate)
  
  
  # Controlling for Payment_Log
  n<-nrow(FiscalPaymentLog)
  
  for(i in 1:n)
  {
    if(FiscalPaymentLog$Outcome[i]=='CONFIRMED')
    {
      FiscalInvoice$Status[i]<-'CONFIRMED'
    }
  }
  
  exportResults(CurrentDateTime,ParkCustomersVeichles,'1_CustomersVeichles') 
  exportResults(CurrentDateTime,ParkCustodyLog,'2_CustodyLog')
  exportResults(CurrentDateTime,ParkParkingEvent,'3_ParkingEvent')
  exportResults(CurrentDateTime,FiscalPaymentLog,'4_PaymentLog') 
  exportResults(CurrentDateTime,FiscalInvoice,'5_Invoice')
  exportResults(CurrentDateTime,CurrentParkPullingEvent,'6_PullingEvent')
  exportResults(CurrentDateTime,FiscalInvoiceDetail,'7_InvoiceDetail')

}

close(db)
