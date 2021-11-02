select * from PC;
--a
select avg(speed) from pc;

--b
select avg(speed) from laptop
where price > 2500;

--c
select avg (pc.price) from pc, products
where pc.model = products.model
and products.maker = 'A' ;

--d1
select avg (t.price) from ( 
select (pc.price) from pc, products where pc.model = products.model and products.maker = 'D' 
union
select (l.price) from laptop l, products where l.model = products.model and products.maker = 'D' 
) t;

--e
select speed,avg(price) from pc group by speed;

--f
select maker, avg(screen) from products, laptop where laptop.model = products.model group by maker order by maker;

--g
select maker, count(model) countt from products group by maker having count(model) >=3 order by countt desc;

--h
select maker, maxPrice, model from 
(select maker,   max(price) as maxPrice from products, pc where pc.model = products.model group by maker order by maker) t, pc
where pc.price = t.maxPrice;
/* just maker and price */
select maker,   max(price) as maxPrice from products, pc where pc.model = products.model group by maker order by maker

--i
select speed,avg(price) from (select speed, price from pc where speed >= 900 )group by speed;

--j
/* take pc models of manufactures who make printers, and equal join with pc models */
select avg(hd) from pc,  (
select model from products where type like 'PC' and maker in (
select distinct maker from products where type = 'PRINTER') ) t
where t.model = pc.model;