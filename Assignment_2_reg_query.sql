USE charity;
set @s=0;
set @i=0;
SELECT a1.contactid, DATEDIFF(a2.actdate, a1.actdate) reg
	FROM (SELECT @s:= @s+1 serialnb, contactid, actdate
			FROM acts
            ORDER BY contactid, actdate) AS a1
	JOIN (SELECT @i:= @i+1 serialnb, contactid, actdate
			FROM acts
            ORDER BY contactid, actdate) AS a2
	WHERE a1.contactid = a2.contactid AND a2.serialnb-a1.serialnb=1;