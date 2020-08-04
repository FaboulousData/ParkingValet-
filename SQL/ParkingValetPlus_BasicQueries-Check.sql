/* Total Detail for each Company City*/
SELECT co.City, sum(id.TotalDetail)
FROM Fiscal.InvoiceDetail AS id
INNER JOIN Fiscal.Invoice AS i
ON i.InvoiceID = id.InvoiceID
INNER JOIN Fiscal.Company AS co
ON co.CompanyID = i.CompanyID
GROUP BY co.City;

/* Number of Pulling events for each driver surname*/
SELECT d.Surname, COUNT(pe.PullingEventID) 'Number of pulling'
FROM Park.Drivers AS d
INNER JOIN Park.PullingEvent AS pe
ON pe.DriversID = d.DriversID
GROUP BY d.Surname;

/* Total Invoice for each ParkingLotID and ParkingArea*/
SELECT pl.Parking_area, sum(id.TotalDetail) AS Total
FROM Park.ParkingLot AS pl
INNER JOIN Park.ParkingEvent AS pe
ON pe.ParkingLotID = pl.ParkingLotID
INNER JOIN Park.PullingEvent AS pu
ON pu.ParkingEventID = pe.ParkingEventID
INNER JOIN Fiscal.Invoice AS i
ON i.InvoiceID = pu.InvoiceID
INNER JOIN Fiscal.InvoiceDetail AS id
ON id.InvoiceID = i.InvoiceID
GROUP BY pl.Parking_area
ORDER BY Total;

/* Check Invoice total with Invoice Total Detail*/
SELECT i.InvoiceID, i.SubTotal, sum(id.TotalDetail)
FROM Fiscal.Invoice AS i
INNER JOIN Fiscal.InvoiceDetail AS id
ON i.InvoiceID = id.InvoiceID
GROUP BY i.InvoiceID, i.SubTotal;


/* Free Parking Lot */
SELECT ParkingLotID FROM Park.ParkingLot
WHERE ParkingLotID NOT IN 
	(SELECT ParkingLotID FROM Park.OccupiedParkingLot);

/* Occupied ParkingLotID sorted by date */
SELECT * FROM Park.OccupiedParkingLot
ORDER BY Pulling_DateTime ASC;