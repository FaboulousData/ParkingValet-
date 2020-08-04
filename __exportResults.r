exportResults<-function(date, objectname, name)
{
  # Author: Fabrizio Miorelli
  # Licence: GNU General Public License v3.0
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
  # The aim of this script is to develop a procedure for writing R objects/variables to a specific folder naming the files with a date
  #
  # INPUT: date (POSIXct date and time), objectname (the name of the variable), name (the name of the file)
  # oUTPUT: -
  
  
  ## Defining time and date variables to use when assigning the file name
  Year<-format(date, format="%Y")
  Month<-format(date, format="%m")
  Day<-format(date, format="%d")
  
  data<-paste(Year,Month,Day, sep="")
  
  
  # Write the txt file to destination
  write.table(objectname,paste('H:/BACKUP/Documenti/Formazione - Master/Microsoft TSQL/Progetti/ParkingValet+/Data/',data,'-',substitute(name),'.txt',sep=""), sep="|", dec=".", row.names=FALSE, col.names=TRUE, quote=TRUE)
} 