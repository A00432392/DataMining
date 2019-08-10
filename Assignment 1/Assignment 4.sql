#Assignment4
use dataset04;

SELECT `Value` FROM `MT123electricity` WHERE `RecordDateTime` BETWEEN "2014-01-01" AND "2015-01-01";

SELECT SUM(`Value`) FROM `MT123electricity` WHERE `RecordDateTime` BETWEEN "2014-01-01" AND "2015-01-01" 
GROUP BY CAST(`RecordDateTime` as date), EXTRACT(HOUR FROM `RecordDateTime`);

SELECT SUM(`Value`) FROM `MT123electricity` WHERE `RecordDateTime` BETWEEN "2014-01-01" AND "2015-01-01" 
GROUP BY CAST(`RecordDateTime` as date), EXTRACT(DAY FROM `RecordDateTime`);











select * from MT123electricity;

#Monthly
select YEAR(RecordDateTime) as C_Year, Month(RecordDateTime) as C_Month, round(sum(Value),0) as Value  from MT123electricity
group by C_Year, C_Month;

#Day
select YEAR(RecordDateTime) as C_Year, Month(RecordDateTime) as C_Month, Day(RecordDateTime) as C_Day,  round(sum(Value),0) as Value  from MT123electricity
group by C_Year, C_Month, C_Day;

#Hourly
select YEAR(RecordDateTime) as C_Year, Month(RecordDateTime) as C_Month, Day(RecordDateTime) as C_Day, Hour(RecordDateTime) as C_Hour, round(sum(Value),0) as Value  
from MT123electricity
group by C_Year, C_Month, C_Day, C_Hour;

#Weekly
SELECT YEAR(RecordDateTime) as Yearly , WEEK(RecordDateTime) as Weekly, sum(value)
FROM MT123electricity
GROUP BY Yearly, Weekly;