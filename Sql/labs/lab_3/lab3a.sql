---q1
select model, speed, hd from pc where price >1600 and hd in (10,20);
---q2
SELECT model,speed megahertz,hd gigabytes from pc
where price>1600;

---q3
select distinct maker from products where type ='PC';
---q4
select model, ram,  screen from laptop where price > 2000 and (screen > 14 or ram > 64);
---q5
select  maker, speed FROM products p, laptop l WHERE l.model = p.model and hd >=10;

---q6
select model from pc where speed > (select  speed from pc where model = 1001);
-- select pc1.model from pc pc1, pc pc2 where pc1.speed >pc2.speed and pc2.model = 1001

---q7
select l.model, l.price from laptop l, products p where p.maker = 'A' and p.model = l.model
  union
select pr.model, pr.price from printer pr, products p where p.maker = 'A' and pr.model=p.model
  union
select c.model, c.price from pc c, products p where p.maker = 'A' and c.model = p.model;
---q8
select l.model, l.speed, l.price from laptop l where price>2000 and speed>100
union
select pc.model, pc.speed, pc.price from pc where price>2000 and speed>100;
---q9
select p.maker from laptop l, products p where l.model=p.model and l.price>2000 and l.speed>100
intersect
select p.maker from pc, products p where pc.model=p.model and pc.price>2000 and pc.speed>100;
---q10
select distinct maker from products
where type = 'LAPTOP' 
and maker not in (select maker from products where type = 'PC');
--
select distinct maker from products where type like 'LAPTOP'
minus
select distinct maker from products where type like 'PC';

---q11
SELECT hd, COUNT(*)
FROM pc
GROUP BY hd
HAVING COUNT(*) >= 2;
--
select pc1.model, pc1.hd from pc pc1, pc pc2 where pc1.model != pc2.model and pc1.hd = pc2.hd;
---q12
select  pc1.model pc1_model, pc2.model pc2_model, pc1.speed speed, pc1.ram ram
from pc pc1, pc pc2 /* use '<' to eliminate duplicates! */
where pc1.speed =pc2.speed and pc1.ram = pc2.ram and pc1.model < pc2.model);