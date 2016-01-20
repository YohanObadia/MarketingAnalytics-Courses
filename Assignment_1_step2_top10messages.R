library(RODBC)
library(ggplot2)
library(igraph)
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

############################################################################################
# TOP 10 messages
############################################################################################


query13 = "SELECT bestmsg, nbsentmsg, totamount, nbdonations, (totamount/nbsentmsg) As avgtotamount
              FROM(
              		SELECT a.messageid As bestmsg, COUNT(b.messageid) As nbsentmsg, COUNT(a.messageid) As nbdonations, SUM(amount) As totamount
                    FROM charity.acts As a
                    RIGHT JOIN charity.actions As b
                    ON a.messageid = b.messageid
                    GROUP BY a.messageid
                  ) As q
              WHERE (nbsentmsg>1000) AND (nbsentmsg<>nbdonations)
              ORDER BY avgtotamount DESC
              LIMIT 10"
top10msg = sqlQuery(db, query13)