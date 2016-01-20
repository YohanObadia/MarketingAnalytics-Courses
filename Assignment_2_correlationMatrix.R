library(RODBC)
db = odbcConnect("myodbc", uid="root", pwd="")
sqlQuery(db, "USE charity")

#Extract data from database
query = "SELECT   t.contactid,
            	    IF(DATEDIFF(20120915, MAX(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MAX(a.actdate)) / 365) AS 'recency',
                  COUNT(a.amount) AS 'frequency',
              		IF(AVG(a.amount) IS NULL, 0, AVG(a.amount)) AS 'avgamount',
              		IF(DATEDIFF(20120915, MIN(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MIN(a.actdate)) / 365) AS 'firstdonation',
                  IF(c.counter IS NULL, 0, 1) AS 'loyal',
              		SUBSTRING(zipcode,1,2) AS 'zipcode',
              		IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS 'gender',
              		IF(MONTH(a.actdate) IS NULL, 0, MONTH(a.actdate)) AS month
	        FROM actions AS t
    
          LEFT JOIN contacts AS ct
	        ON ct.contactid = t.contactid
    
          LEFT JOIN 	(SELECT * FROM Acts
                					WHERE (ActType='DO')
                					AND (ActDate<=20120915)
				              ) AS a
          ON a.contactid = t.contactid
    
          LEFT JOIN 	(SELECT ContactId, COUNT(Amount) AS counter
                					FROM Acts
                					WHERE (ActDate >= 20120915) AND
                						    (ActDate <  20131101) AND
                						    (ActType = 'DO')
                					GROUP BY ContactId
              				) AS c
          ON c.contactid = t.contactid
                
        WHERE (zipcode REGEXP '^-?[0-9]+$') AND (t.MessageId LIKE '12F13%')
  	    GROUP BY 1;"

datainitial = sqlQuery(db, query)

# Close the connection
odbcClose(db)

sinusrecenc <- data.frame(model.matrix( ~ recency*sin(0.3*recency)^4, data=datainitial))
exponentiellefirst <- data.frame(model.matrix( ~ exp(firstdonation/3), data=datainitial))
logfrequ <- data.frame(model.matrix( ~ log(frequency+0.1), data=datainitial))

matint = cbind(sinusrecenc[,4], exponentiellefirst[,2])
matfin = cbind(matint,logfrequ[,2])

cor(matfin)
