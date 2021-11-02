SET SERVEROUTPUT ON
--Q1
--BEGIN
--update emp  Set emp.sal=emp.sal*1.15 where deptno=10;
--IF(SQL%FOUND) THEN
--DBMS_OUTPUT.put_line(TO_CHAR(SQL%ROWCOUNT) || ' row updated');
--ELSE
--DBMS_OUTPUT.put_line('no data');
--END;
SELECT *
FROM EMP;


--Q2
SET SERVEROUTPUT ON
DECLARE
jobold emp.job%TYPE:='&old';
jobnew emp.job%TYPE:='&new';
BEGIN
--DBMS_OUTPUT.put_line('Enter the old job');
--DBMS_OUTPUT.put_line('Enter the new job');
update emp  Set emp.job=jobnew where job=jobold;
IF SQL%FOUND THEN 
DBMS_OUTPUT.put_line(TO_CHAR(SQL%ROWCOUNT)||'rows updated');
END IF;
END;


---THE SOLUTION
DECLARE
	l_old_job emp.job%TYPE := '&1';
	l_new_job emp.job%TYPE := '&2';
BEGIN
	UPDATE emp
	SET job = l_new_job
	WHERE job = l_old_job;
	IF SQL%FOUND THEN
		DBMS_OUTPUT.put_line(TO_CHAR(SQL%ROWCOUNT)||
		' employee(s) changed job');
	ELSE
		DBMS_OUTPUT.put_line('No employee found with job'||
		' of '||l_old_job);
	END IF;
END;

SELECT *
FROM DEPT;

--Q3
SET SERVEROUTPUT ON
DECLARE
CURSOR  dept_cur
IS
SELECT DEPTNO,DNAME,LOC
FROM DEPT;
BEGIN
FOR rec IN dept_cur
LOOP
DBMS_OUTPUT.put_line(rec.DEPTNO);    ---importent to pay attention to rec this is my row 
DBMS_OUTPUT.put_line(rec.DNAME);
DBMS_OUTPUT.put_line(rec.LOC);
END LOOP;
END;


