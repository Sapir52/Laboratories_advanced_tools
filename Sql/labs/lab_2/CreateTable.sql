create table Products
(
    Maker VARCHAR2(1),
    Model Number(4)not null PRIMARY KEY ,
    Type VARCHAR2(10)
);

create table Pc
(
    Model Number(4)not null PRIMARY KEY ,
    Speed Number(4),
    Ram Number(4),
    Hd Number(4),
    price Number(4)
);

create table Laptop
(
    Model Number(4)not null PRIMARY KEY ,
    Speed Number(4),
    Ram Number(4),
    Hd Number(4),
    Screen Number(3,1),
    price Number(4)
);

create table Printer
(
    Model Number(4)not null PRIMARY KEY ,
    Color VARCHAR2(10),
    Type VARCHAR2(10),
    price Number(4)
);