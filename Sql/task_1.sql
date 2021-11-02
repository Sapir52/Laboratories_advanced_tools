
/*Deleting the tables*/
drop table Growers;
drop table Processors;
drop table Shippings;
/********************************************************************************************/
/*Create a table named Growers*/
create table  Growers
(
    company  VARCHAR2(1) PRIMARY KEY,
    country VARCHAR2(10),
    amount Number(10),
    arabicaShare Number(3),
    robustaShare Number(2)
);
/********************************************************************************************/
/*Create a table named Processors*/
create table  Processors
(
    company  VARCHAR2(2),
    country VARCHAR2(11),
    yearlyCapacity Number(10),
    numberOfRoastingDegrees Number(1),
    PRIMARY KEY (company,country)
);
/********************************************************************************************/
/*Create a table named Shippings*/
create table  Shippings
(
    grower  VARCHAR2(1),
    processorName VARCHAR2(2),
    processorCountry VARCHAR2(11),
    amount Number(5),
    "date" DATE,
    PRIMARY KEY (grower,processorName,processorCountry,"date")
);

/********************************************************************************************/
/*Enter values into a table named  Growers*/
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('A','VIETNAM',120000,10,90);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('B','COLOMBIA',700000,80,20);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('C','COLOMBIA',350000,85,15);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('D','INDONESIA',680000,50,50);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('E','BRAZIL',1000000,90,10);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('F','BRAZIL',5000000,80,20);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('G','HONDURAS',430000,100,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('H','BRAZIL',711000,80,20);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare)
VALUES('I','INDONESIA',280000,40,60);
/********************************************************************************************/
/*Enter values into a table named  Processors*/
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('AA','US',1000000,5);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('BB','NETHERLANDS',500000,3);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('CC','ITALY',700000,5);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('AA','JAPAN',55000,4);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('BB','ITALY',650000,5);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('CC','CROATIA',50000,3);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('DD','FRANCE',155000,3);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('AA','UK',80000,2);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('CC','SLOVENIA',65000,3);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('BB','FRANCE',200000,4);
INSERT INTO Processors(company,country,yearlyCapacity,numberOfRoastingDegrees)
VALUES('DD','US',750000,5);
/********************************************************************************************/
/*Enter values into a table named  Shippings*/
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('A','AA','US',10000,to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('B','AA','JAPAN',30000,to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('C','BB','NETHERLANDS',20000,to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('A','BB','ITALY',40000,to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('B','CC','SLOVENIA',10000,to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('C','DD','FRANCE',10000,to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('D','DD','US',20000,to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('D','AA','US',40000,to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('H','DD','FRANCE',40000,to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('I','BB','ITALY',20000,to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('H','BB','NETHERLANDS',10000,to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"date")
VALUES('I','BB','ITALY',30000,to_date('23/08/2019', 'DD/MM/YYYY'));
/********************************************************************************************/
/*tar a*/
select distinct  G1.COUNTRY
from SHIPPINGS S1, GROWERS G1
where  S1.GROWER=G1.COMPANY and   S1."date"!=to_date('17/09/2019', 'DD/MM/YYYY')
    minus
select distinct  G1.COUNTRY
from SHIPPINGS S1, GROWERS G1
where  S1.GROWER=G1.COMPANY and   S1."date"=to_date('17/09/2019', 'DD/MM/YYYY');
/********************************************************************************************/
/*tar b*/
select  G1.COMPANY
from   GROWERS G1
where  (AMOUNT*ARABICASHARE)/100= (select min((AMOUNT*ARABICASHARE)/100) from GROWERS)  ;
/********************************************************************************************/
/*tar c*/
select G1.COMPANY, G1.COUNTRY
from GROWERS G1
where G1.COMPANY in( select S1.GROWER from SHIPPINGS S1, SHIPPINGS S2
     where S1.AMOUNT != S2.AMOUNT and S1.GROWER = S2.GROWER  and   S1.AMOUNT + S2.AMOUNT in (select max(S1.AMOUNT +S2.AMOUNT)  from SHIPPINGS S1, SHIPPINGS S2  
        where  S1.AMOUNT != S2.AMOUNT and S1.GROWER = S2.GROWER ));
/********************************************************************************************/
/*tar d*/
select G1.COUNTRY
from GROWERS G1
    minus
select G1.COUNTRY
from SHIPPINGS S1, GROWERS G1
where G1.COMPANY=S1.GROWER  and S1.AMOUNT =(select max(S3.AMOUNT) from SHIPPINGS S3);
/********************************************************************************************/
/*tar e*/
select  distinct S1.GROWER, S2.PROCESSORNAME
from SHIPPINGS S1, SHIPPINGS S2 
where S1.GROWER!= S2.PROCESSORNAME
    minus
select   S1.GROWER, S1.PROCESSORNAME
from SHIPPINGS S1 ;
/********************************************************************************************/


