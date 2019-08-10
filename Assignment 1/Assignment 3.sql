CREATE TABLE temp (SELECT `InvoiceNo`, `Description` 
FROM dataset04.OnlineRetail WHERE `UnitPrice` > 0 AND `Quantity` > 0 AND `CustomerID` <> 0 AND `InvoiceNo` <> 0 AND `StockCode` <> "POST")