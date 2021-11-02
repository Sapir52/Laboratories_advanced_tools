create table Students
(
    Id NUMBER(6),
    LastName VARCHAR2(20),
    FirstName VARCHAR2(10),
    Address VARCHAR2(10),
    BirthDay DATE,
    GroupId NUMBER(3)
);   

insert into Students(Id,LastName,FirstName,Address,BirthDay,GroupId)
values(101,'Solin','Dan','BeerSheva',to_date('01/02/1985','dd/mm/yyyy'),11);
alter table  Students add AvgMark NUMBER(5,2) DEFAULT 0;
alter table Students drop column AvgMark;
alter table  Students add AvgMark NUMBER(5,2) DEFAULT 0;
insert into Students(Id,LastName,FirstName,Address,BirthDay,GroupId,AvgMark)
values(102,'Tal','Ruti','Tel-Aviv',to_date('10/7/1988','dd/mm/yyyy'),12,17);
insert into Students(Id,LastName,FirstName,Address,BirthDay,GroupId,AvgMark)
values(103,'Kohen','Yosi','Dimona',to_date('01/8/1987','dd/mm/yyyy'),11,80);
insert into Students(Id,LastName,FirstName,Address,BirthDay,GroupId,AvgMark)
values(104,'Toys','Vered','Tel-Aviv',to_date('15/09/1988','dd/mm/yyyy'),12,90);
alter table Students modify Address varchar2(15);
create table Students2 as select Id,LastName,FirstName,Address,BirthDay,GroupId,AvgMark from Students;
delete from  Students where BirthDay='10-JUL-88';
update students2 set GroupId=10 where GroupId=11;
rename Students2 to Students_new;
drop table Students_new;
