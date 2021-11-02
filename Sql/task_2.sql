/*Deleting the tables*/
DROP TABLE Shippings;
DROP TABLE Growers;
DROP TABLE Processors;
DROP TRIGGER  triger_shipped;
/********************************************************************************************/
/*Create a table named Growers*/
create table  Growers
(
    company  VARCHAR2(1),
    country VARCHAR2(10),
    amount Number(10) DEFAULT 0,
    arabicaShare Number(3),
    robustaShare Number(2),
    shipped Number(10)  DEFAULT 0,
    CONSTRAINT pk_company PRIMARY KEY(company),
    CONSTRAINT growers_country CHECK (country= 'US' OR country= 'UK' OR country= 'NETHERLANDS' OR country='ITALY' OR country= 'FRANCE' OR country= 'BRAZIL' OR country= 'SLOVENIA' OR country='CROATIA' OR country='JAPAN' OR country='INDONESIA' OR country='ETHIOPIA' OR country='HONDURAS' OR country='VIETNAM' OR country='CAMBODIA' OR country='GANA' OR country='COLOMBIA' )

);
/********************************************************************************************/
/*Create a table named Processors*/
create table  Processors
(
    company  VARCHAR2(2),
    country VARCHAR2(11),
    "capacity" Number(10) DEFAULT 0,
    numOfRoastingDegrees Number(1),
    CONSTRAINT pk_processors  PRIMARY KEY (company,country),
     CONSTRAINT Processors_country CHECK (country= 'US' OR country= 'UK' OR country= 'NETHERLANDS' OR country='ITALY' OR country= 'FRANCE' OR country= 'BRAZIL' OR country= 'SLOVENIA' OR country='CROATIA' OR country='JAPAN' OR country='INDONESIA' OR country='ETHIOPIA' OR country='HONDURAS' OR country='VIETNAM' OR country='CAMBODIA' OR country='GANA' OR country='COLOMBIA' )
);
/********************************************************************************************/
/*Create a table named Shippings*/
create table  Shippings
(
    grower  VARCHAR2(1),
    processorName VARCHAR2(2),
    processorCountry VARCHAR2(11),
    amount Number(5) NOT NULL,
    "type"  VARCHAR2(7),
    shipDate DATE ,
   CONSTRAINT  pk_shippings PRIMARY KEY (grower,processorName,processorCountry,shipDate),
   CONSTRAINT fk_proc FOREIGN  KEY(processorName,processorCountry) REFERENCES Processors(company,country),
    CONSTRAINT fk_grow FOREIGN  KEY(grower) REFERENCES Growers(company)
);
/********************************************************************************************/
ALTER TABLE Shippings ADD CHECK (shipDate>= to_date('01/01/2019', 'DD/MM/YYYY'));
ALTER TABLE Shippings ADD CHECK ("type"= 'Arabica' OR "type"= 'Robusta');

/********************************************************************************************/
/* tar2*/
CREATE TRIGGER triger_shipped 
BEFORE INSERT ON Shippings FOR EACH ROW
BEGIN
    UPDATE Growers sh
    SET shipped =  (SELECT SUM (amount) FROM Shippings WHERE Shippings.GROWER= sh.company);
END;
/
/********************************************************************************************/
/*Enter values into a table named  Growers*/
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('A','VIETNAM',120000,10,90,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('B','COLOMBIA',700000,80,20,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('C','COLOMBIA',350000,85,15,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('D','INDONESIA',680000,50,50,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('E','BRAZIL',1000000,90,10,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('F','BRAZIL',5000000,80,20,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('G','HONDURAS',430000,100,0,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('H','BRAZIL',711000,80,20,0);
INSERT INTO Growers(company,country,amount,arabicaShare,robustaShare,shipped)
VALUES('I','INDONESIA',280000,40,60,0);
/********************************************************************************************/
/*Enter values into a table named  Processors*/
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('AA','US',1000000,5);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('BB','NETHERLANDS',500000,3);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('CC','ITALY',700000,5);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('AA','JAPAN',55000,4);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('BB','ITALY',650000,5);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('CC','CROATIA',50000,3);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('DD','FRANCE',155000,3);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('AA','UK',80000,2);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('CC','SLOVENIA',65000,3);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('BB','FRANCE',200000,4);
INSERT INTO Processors(company,country,"capacity",numOfRoastingDegrees)
VALUES('DD','US',750000,5);
/********************************************************************************************/
/*Enter values into a table named  Shippings*/
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('A','AA','US',10000,'Robusta',to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('B','AA','JAPAN',30000,'Arabica',to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('C','BB','NETHERLANDS',20000,'Arabica',to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('A','BB','ITALY',40000,'Arabica',to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('B','CC','SLOVENIA',10000,'Arabica',to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('C','DD','FRANCE',10000,'Robusta',to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('D','DD','US',20000,'Robusta',to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('D','AA','US',40000,'Robusta',to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('H','DD','FRANCE',40000,'Arabica',to_date('17/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('I','BB','ITALY',20000,'Arabica',to_date('15/09/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('H','BB','NETHERLANDS',10000,'Arabica',to_date('23/08/2019', 'DD/MM/YYYY'));
INSERT INTO Shippings(grower,processorName,processorCountry,amount,"type",shipDate)
VALUES('I','BB','ITALY',30000,'Arabica',to_date('23/08/2019', 'DD/MM/YYYY'));
/********************************************************************************************/
/* tar3 */
SELECT grower  ,SUM (amount) 
FROM Shippings 
GROUP BY grower;

select *
from growers;

select*
from Processors;

select*
from Shippings;

/********************************************************************************************/
/* tar4 a*/
/* using loop */
SET VERIFY OFF;
SET SERVEROUTPUT ON;
ACCEPT dataVal PROMPT 'Please enter Date: ';
DECLARE
        getData DATE :=to_date(' &dataVal','DD/MM/YYYY');
        CURSOR SH IS SELECT amount,processorcountry FROM Shippings WHERE shipDate = getData;
        dataAmount shippings.amount%TYPE;
        dataProcessorcountry shippings.processorcountry%TYPE;
BEGIN
        OPEN SH;
         DBMS_OUTPUT.PUT_LINE('This report is for date= '||getData||':');
        LOOP
                FETCH SH INTO dataAmount,dataProcessorcountry;
                  EXIT WHEN SH%NOTFOUND;
                DBMS_OUTPUT.PUT_LINE(dataProcessorcountry|| '  |  ' ||dataAmount);
              
        END LOOP;
        CLOSE SH;
END;
/
/***************************************/
/* tar4 b*/
/* using for */
ACCEPT dataVal2 PROMPT 'Please enter Date: ';
DECLARE    
   CURSOR SH2 IS SELECT processorCountry , amount, shipDate FROM shippings; 
BEGIN
    dbms_output.put_line('This report for date' || '=' || '&dataVal2');
   FOR val IN SH2 LOOP
       IF val.shipDate = '&dataVal2' THEN
         dbms_output.put_line(val.processorCountry || '  | ' || val.amount);
       END IF;
 END LOOP;
END;
/


/********************************************************************************************/
/* tar5*/
CREATE OR REPLACE  FUNCTION SalAction(country2  VARCHAR) 
RETURN NUMBER IS 
sal NUMBER; 
BEGIN
    SELECT max(numOfRoastingDegrees) s INTO sal
    FROM Processors  WHERE Processors.country =country2;    
    RETURN sal;  
END ;
/
ACCEPT country PROMPT 'Please enter country'
DECLARE 
    val NUMBER;
BEGIN
    val:=SalAction('&country'); 
    dbms_output.put_line('&country | '|| val); 
END;
/

/********************************************************************************************/
/* tar6*/
ACCEPT g PROMPT 'Please enter grower name: ';
DECLARE
    grower VARCHAR2(1) := '&g';
BEGIN
   Shippi(grower);
END;
/

CREATE OR REPLACE PROCEDURE Shippi (grower IN VARCHAR2) IS
temp Shippings%ROWTYPE;
CURSOR g IS 
        SELECT * FROM Shippings;
BEGIN
    FOR x IN g LOOP
            IF x.grower = grower THEN
                DBMS_OUTPUT.PUT_LINE(x.processorName|| '  |  ' ||x.processorCountry);
            END IF;
    END LOOP;
END;
/


