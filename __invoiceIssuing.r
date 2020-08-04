invoiceIssuing<-function(db, CurrentParkPullingEvent,BindParkingEvent)
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
  # The aim of this script is to produce invoices for the ParkingValet+ database
  #
  # INPUT: db (RODBC database connection), CurrentParkPullingEvent (data.frame - Pulling Events), BindParkingEvent (data.frame - all parking events)
  # oUTPUT: out (fictious fiscal invoices, amount, subtotals)
  
  #================= Current Parking Event is not empty
  # checking if there are some pulling event: IF NOT, SKIP to the second if 
  
  if(nrow(CurrentParkPullingEvent)>=1 && !(is.na(CurrentParkPullingEvent$ParkingEventID[1])))
  {
    
    ######## INVOICE
    
    Service<-sqlQuery(db, 'SELECT * FROM Park.Service')
    Invoice<-sqlQuery(db, 'SELECT InvoiceID FROM Fiscal.Invoice ORDER BY InvoiceID ASC')
    Company<-sqlQuery(db, 'SELECT CompanyID, VATcode FROM Fiscal.Company')
    TypeOfInvoice<-sqlQuery(db, 'SELECT TypeOfInvoiceID FROM Fiscal.TypeOfInvoice')
    InvoiceDetail<-sqlQuery(db, 'SELECT InvoiceDetailID FROM Fiscal.InvoiceDetail ORDER BY InvoiceDetailID ASC')
  
    
    # n° of invoice to be issued each of them related to CurrentParkPullingEvent
    n<-nrow(CurrentParkPullingEvent)
  
    # initializing the FiscalInvoice dataframe: number of rows=number of currentParkPullingEvent
    FiscalInvoice<-data.frame(InvoiceID=integer(n),CompanyID=integer(n),TypeOfInvoiceID=integer(n),PaymentLogID=integer(n),VATcode=character(n),IssueDate=as.POSIXct('1900-01-01 01:01:01'),Status=character(n),SubTotal=numeric(n),Taxes=numeric(n),Netto=numeric(n))
  
  
    # Generating the sequence of InvoiceID to be used (starting from the last InvoiceID generated)
    last_id<-Invoice$InvoiceID[nrow(Invoice)]
    first_id<-last_id+1
    
    if(is.na(first_id) || length(first_id)==0)
    {
      first_id<-1
    }
    InvoiceID<-seq(first_id,first_id+n-1,1)
  
    
    # assigning values to the FiscalInvoice data.frame
    FiscalInvoice$InvoiceID<-InvoiceID
    FiscalInvoice$CompanyID<-biasedSampling(n,Company$CompanyID,'desc','gamma',repl=FALSE)
    FiscalInvoice$TypeOfInvoiceID<-biasedSampling(n,TypeOfInvoice$TypeOfInvoiceID,'desc','gamma',repl=FALSE)
    
    # Applico un ciclo for in quanto la funzione subset senza for non restituisce valori duplicati nel caso in cui siano presenti dati ridondanti
    for(i in 1:n)
    {
      FiscalInvoice$VATcode[i]<-subset(Company, Company$CompanyID %in% FiscalInvoice$CompanyID[i], select=VATcode)$VATcode
    }
    
    FiscalInvoice$IssueDate<-subset(BindParkingEvent, BindParkingEvent$ParkingEventID %in% CurrentParkPullingEvent$ParkingEventID, select=Parking_DateTime)$Parking_DateTime #@order

    ########
    
    
    
    
    
    
    
    ######## INVOICE DETAIL
    
    # Defining  how many InvoiceDetail for each Invoice
    n<-nrow(FiscalInvoice)
    m<-sample(1:4,n,replace=TRUE)
    z<-sum(m)
    
    
    # initializing a dataframe that will contain 
    y<-data.frame(invoiceDetailID=integer(z), InvoiceID=integer(z))
    
    
    # generating the sequence of FiscalInvoiceDetailID to be used
    last_id<-InvoiceDetail$InvoiceDetailID[nrow(InvoiceDetail)]
    first_id<-last_id+1
    
    if(is.na(first_id) || length(first_id)==0)
    {
      first_id<-1
    }
    
    
    # creating the sequence of InvoiceDetailID for each InvoiceID
    nr<-sum(m)
    id<-seq(first_id,first_id+nr-1)
    y$invoiceDetailID<-id
    
    if(n==1)
    {
      y$InvoiceID<-FiscalInvoice$InvoiceID
    }
    '--------'
    if(n>1)
    {
      k<-NULL
      
      for(i in 1:n)
      {
        x<-as.data.frame(rep(FiscalInvoice$InvoiceID[i], m[i]))
        k<-rbind(k,x)
      }
      
      k<-k[[1]]
      #names(k)<-"InvoiceID"
      y$InvoiceID<-k
    }
  
    
    # Initializing InvoiceDetail and filling it (with y values)
    
    FiscalInvoiceDetail<-data.frame(InvoiceDetailID=integer(z), InvoiceID=integer(z), ServiceID=integer(z), SellPrice=double(z), Quantity=integer(z), Discount=double(z), TotalDetail=double(z))
    
    FiscalInvoiceDetail$InvoiceDetailID<-y$invoiceDetailID
    FiscalInvoiceDetail$ServiceID<-biasedSampling(nr,Service$ServiceID,'desc','gamma',repl=FALSE)
    
    
    # filling with INvoiceID: if z>1 do de-listinfg (NOT SURE, PROBABLY A BUG IN DATA.FRAME function)
    if(z==1)
    {
      FiscalInvoiceDetail$InvoiceID<-as.numeric(y$InvoiceID)
    }
    
    if(z>1)
    {
      FiscalInvoiceDetail$InvoiceID<-as.numeric(y$InvoiceID)
    }
    
    
    for(i in 1:nrow(FiscalInvoiceDetail))
    {
      FiscalInvoiceDetail$SellPrice[i]<-subset(Service, Service$ServiceID==FiscalInvoiceDetail$ServiceID[i], select=UnitPrice)
      k<-1:10
      FiscalInvoiceDetail$Quantity[i]<-max(biasedSampling(2,k,'desc','gamma',repl=FALSE))
    }
    
    
    # inizializing and filling the Total dataframe, with the subtotal of the details
    Total=data.frame(InvoiceID=integer(nrow(FiscalInvoice)), SubTotal=double(nrow(FiscalInvoice)))
    Total$InvoiceID<-FiscalInvoice$InvoiceID
    
    for(i in 1:nrow(FiscalInvoice))
    {
      price<-subset(FiscalInvoiceDetail, FiscalInvoiceDetail$InvoiceID == FiscalInvoice$InvoiceID[i], select=SellPrice)
      quantity<-subset(FiscalInvoiceDetail, FiscalInvoiceDetail$InvoiceID == FiscalInvoice$InvoiceID[i], select=Quantity)
      Total$SubTotal[i]<-sum(as.numeric(price$SellPrice)*quantity$Quantity)
    }
    
    # Returning the results
    out<-list('FiscalInvoice' = FiscalInvoice, 'FiscalInvoiceDetail' = FiscalInvoiceDetail, 'Total' = Total)
    return(out)
  }
  ########
  
  
  
  
  
  
  #================= Current Parking Event is empty
  if(nrow(CurrentParkPullingEvent)>0 & (is.na(CurrentParkPullingEvent$ParkingEventID[1])))
  {
    stop(cat('No parking events expected for ',as.character(BindParkingEvent$Parking_DateTime[1]),''))
    return(NULL)
  }
  
  
}
    
    