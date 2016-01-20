library(RODBC)
library(ggplot2)
library(igraph)
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

############################################################################################
# How many contacts are there in the database?
############################################################################################

query01    ="SELECT COUNT(*) FROM contacts"
nbcontact  = sqlQuery(db, query01)
# returns 24696

query02  ="SELECT COUNT(identity)
FROM(SELECT DISTINCT ContactId as identity FROM acts) As q"
nbdonors = sqlQuery(db, query02)
# returns 18945

############################################################################################
# Global Average and amount donation DO? PA? 
############################################################################################

# total amount collected 2003-2013
query03    ="SELECT SUM(amount) FROM acts;"
Globam  = sqlQuery(db, query03)
# returns 4458082 => 4.46 billion€

# total amount collected 2003-2013 DO only
query04  ="SELECT SUM(amount) FROM acts WHERE ActType LIKE 'DO';"
GlobamDO = sqlQuery(db, query04)
# returns 3877558 => 3.9million€

# total amount collected 2003-2013 PA only
query05  ="SELECT SUM(amount) FROM acts WHERE ActType LIKE 'PA';"
GlobamPA = sqlQuery(db, query05)
# returns 580524.5 => 508,000 €

# Average Donation amount 2003-2013 DO only
query06  ="SELECT AVG(amount) FROM acts WHERE ActType LIKE 'DO';"
GlobAverDO = sqlQuery(db, query06)
# returns 81.27348 => 81.3€

# Average Donation amount 2003-2013 PA only
query07  ="SELECT AVG(amount) FROM acts WHERE ActType LIKE 'PA';"
GlobAverPA = sqlQuery(db, query07)
# returns 18.74353 => 18.7€

############################################################################################
# AMOUNT PER YEAR, ALL, DO, PA
############################################################################################
query1  = "SELECT YEAR(actdate) as actd, SUM(amount) as am FROM acts
GROUP BY YEAR(actdate);"
donyear = sqlQuery(db, query1)

query2 = "SELECT YEAR(actdate) as actd, SUM(amount) as amDO FROM acts acts
WHERE ActType LIKE 'DO' GROUP BY YEAR(actdate);"
donyearDO = sqlQuery(db, query2)

query3 = "SELECT YEAR(actdate) as actd, SUM(amount) as amPA FROM acts acts
WHERE ActType LIKE 'PA' GROUP BY YEAR(actdate);"
donyearPA = sqlQuery(db, query3)

# merge the three tables query 1, query 2, query 3
donyeartot <- merge(donyear,donyearDO,by='actd')
donyeartotf <- merge(donyeartot,donyearPA,by='actd',all=TRUE)

# replace the NA values in the table by 0 (there are NA because no PA donation before 2006)
donyeartotf[is.na(donyeartotf)] <- 0

# plot the three colums together: total donation, total PA and total DO depending on year
ggplot(donyeartotf, aes(actd) +                    
    geom_line(aes(y=am), colour="red") + 
  geom_line(aes(y=amDO), colour="green") +
  geom_line(aes(y=amPA), colour="blue")+
  xlab("Year") + ylab("Total Donation Amount")+
  ggtitle('Total Donation amount from 2003 to 2013') +
  theme(plot.title=element_text(size=20,face="bold",vjust=2))+
  theme(legend.title=element_text(size=20,face="bold",vjust=2))


############################################################################################
# AVERAGE AMOUNT PER YEAR, ALL, DO, PA
############################################################################################

query6  ="SELECT YEAR(actdate) as acty, AVG(amount) as av FROM acts GROUP BY YEAR(actdate)"
avgdon  = sqlQuery(db, query6)

query7  ="SELECT YEAR(ActDate) as acty, AVG(Amount) as avPA FROM acts WHERE ActType LIKE 'PA' GROUP BY ActType, YEAR(ActDate) ORDER BY ActType;"
avgdonPA  = sqlQuery(db, query7)

query8  ="SELECT YEAR(ActDate) as acty, AVG(Amount) as avDO FROM acts WHERE ActType LIKE 'DO' GROUP BY ActType, YEAR(ActDate) ORDER BY ActType;"
avgdonDO  = sqlQuery(db, query8)

# merge the three tables query 6, query 7, query 8
avgdontot <- merge(avgdon,avgdonDO,by='acty',all=TRUE)
avgdontotf <- merge(avgdontot,avgdonPA,by='acty',all=TRUE)

# replace the NA values in the table by 0 (there are NA because no PA donation before 2006)
avgdontotf[is.na(avgdontotf)] <- 0

# plot the three colums together: average donation, average PA and average DO depending on year
ggplot(avgdontotf, aes(acty)) +                    
  geom_line(aes(y=av), colour="red") + 
  geom_line(aes(y=avDO), colour="green") +
  geom_line(aes(y=avPA), colour="blue")+
  xlab("Year") + ylab("Average Donation Amount (€)")+
  ggtitle('Average Donation amount from 2003 to 2013') +
  theme(plot.title=element_text(size=20,face="bold",vjust=2))+
  theme(legend.title=element_text(size=20,face="bold",vjust=2))
  

############################################################################################
# NEW DONORS EVERY YEAR
############################################################################################

#Total donors
query10="SELECT q.firstgiftyear, COUNT(q.id) As newdonors
            FROM (SELECT c.ContactId AS id, YEAR(MIN(a.ActDate)) AS firstgiftyear
            FROM contacts AS c
            JOIN acts AS a
            ON c.ContactId = a.ContactId
            GROUP BY c.ContactId) As q
            GROUP BY q.firstgiftyear"

donnorevo= sqlQuery(db, query10)

#DO donors
query11="SELECT q.firstgiftyear, COUNT(q.id) As newdonorsDO
            FROM (SELECT c.ContactId AS id, YEAR(MIN(a.ActDate)) AS firstgiftyear
            FROM contacts AS c
            JOIN acts AS a
            ON c.ContactId = a.ContactId AND acttype='DO'
            GROUP BY c.ContactId) As q
            GROUP BY q.firstgiftyear"

donnorevoDO= sqlQuery(db, query11)

#PA donors
query12="SELECT q.firstgiftyear, COUNT(q.id) As newdonorsPA
            FROM (SELECT c.ContactId AS id, YEAR(MIN(a.ActDate)) AS firstgiftyear
            FROM contacts AS c
            JOIN acts AS a
            ON c.ContactId = a.ContactId AND a.acttype='PA'
            GROUP BY c.ContactId) As q
            GROUP BY q.firstgiftyear"

donnorevoPA= sqlQuery(db, query12)

# merge the three tables query 10, query 11, query 12
donnorevotot <- merge(donnorevo,donnorevoDO,by='firstgiftyear',all=TRUE)
donnorevototf <- merge(donnorevotot,donnorevoPA,by='firstgiftyear',all=TRUE)

# replace the NA values in the table by 0 (there are NA because no PA donation before 2006)
donnorevototf[is.na(donnorevototf)] <- 0

# plot the three colums together: number of new donnor all, DO and PA depending on year
ggplot(donnorevototf, aes(firstgiftyear)) +                    
  geom_line(aes(y=newdonors), colour="red") + 
  geom_line(aes(y=newdonorsDO), colour="green") +
  geom_line(aes(y=newdonorsPA), colour="blue")+
  xlab("Year") + ylab("Number of new donors")+
  ggtitle('New donors per year from 2003 to 2013') +
  theme(plot.title=element_text(size=20,face="bold",vjust=2))+
  theme(legend.title=element_text(size=20,face="bold",vjust=2))

# note that sum of newdonors = 18945, which is exactly the number of donors



############################################################################################
# How many donors have been active each and every year?
############################################################################################

query13="SELECT q.numberyears, COUNT(id) as numberpeople
            FROM (SELECT DISTINCT c.ContactId AS id, COUNT(DISTINCT YEAR(a.ActDate)) AS numberyears
                    FROM contacts AS c
                    JOIN acts AS a
                    ON c.ContactId = a.ContactId
                    GROUP BY c.ContactId) As q
            GROUP BY q.numberyears
            ORDER BY q.numberyears;
            "
activ= sqlQuery(db, query13)

ggplot(data=activ, aes(x=factor(numberyears),y=numberpeople, fill=factor(numberpeople)))+
  geom_bar(stat="identity") + xlab("Frequency of donation in years")+ ylab("number of people") +
  scale_fill_discrete(name="Scale") +
  ggtitle('Image of the donors loyalty') +
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=18,face="bold"),
        plot.title=element_text(size=20,face="bold",vjust=2))

