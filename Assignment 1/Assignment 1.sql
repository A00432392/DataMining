#Assignment 1
-- PRODUCT_QUANTITY is the Sum of Quantities, it will adjust the aggregation of the amount of product brought and returned by the customer.
-- DISTINCT_PRODUCT is the count of unique product brought by the customer
-- TOTAL_REVENUE is the Total amount spent by the customer, it’s the product of the UnitPrice and Quantity
-- InvoiceNo is the count of unique invoices, it can give the number of visits made by the Customer
-- PER_VISIT_COST is to remove the customer whose purchase quantity is either negative or zero
-- Product Cluster
SELECT 
`CustomerID`, 
SUM(`Quantity`) AS PRODUCT_QUANTITY, 
COUNT(distinct `StockCode`) AS DISTINCT_PRODUCT,
SUM(`UnitPrice` * `Quantity`) as TOTAL_REVENUE,
COUNT(distinct `InvoiceNo`) as VISITS,
SUM(`UnitPrice` * `Quantity`) / COUNT(distinct `InvoiceNo`) as PER_VISIT_COST
FROM `OnlineRetail` 
GROUP BY CustomerID 
ORDER BY TOTAL_REVENUE DESC 
LIMIT 2000;

-- The Stock code is the product item’s unique number.
-- TOTAL_REVENUE is the revenue an item has generated.
-- BASKETS is the number of times the items are purchased.
-- DISTINCT_CUSTOMERS identifies the unique customers who purchased that particular item.
-- QUANTITY_PER_CUSTOMER identifies the average quantity considered by a customer.


SELECT `StockCode`,
SUM(`UnitPrice` * `Quantity`) as TOTAL_REVENUE,
COUNT(distinct `InvoiceNo`) as BASKETS,
COUNT(distinct `CustomerID`) as DISTINCT_CUSTOMERS,
ROUND(SUM(Quantity)/COUNT(distinct `CustomerID`), 2) as QUANTITY_PER_CUSTOMER
FROM `OnlineRetail`
GROUP BY StockCode
ORDER BY TOTAL_REVENUE DESC
LIMIT 2000;


#Assignment 2




#Assignment 3



#Assignment 4