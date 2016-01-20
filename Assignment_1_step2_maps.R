library(RODBC)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(classInt)
library(maptools)
db = odbcConnect("myodbc", uid="root", pwd="")
sqlQuery(db, "USE charity")

##########################################################################################
# create a correspondance table: numero minéralogique / département. Do it before, on mysql
# I tried to do it directly from R but some problems...
##########################################################################################

france <- map(database="france")


##########################################################################################
# Average for DO
##########################################################################################

query1="SELECT q.depname, r.Aver
FROM (Select departement_code as Zip, departement_nom as depname
From departements
Group by 1) as q
JOIN (SELECT LEFT(contacts.ZipCode,2) as Zip,
  	AVG(acts.Amount) as Aver
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'DO'
GROUP by 1) as r
ON q.Zip = r.Zip
GROUP BY q.depname"

table1=sqlQuery(db, query1)

matche<-match.map(france,table1$depname,exact=TRUE)
gray.colors<-function(n) gray(rev(0:(n-1)/1.5)/n)
colors<-heat.colors(max(table1$Aver))[table1$Aver[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)


##########################################################################################
# Average for PA
##########################################################################################

query2="SELECT q.depname, r.Aver
FROM (Select departement_code as Zip, departement_nom as depname
From departements
Group by 1) as q
JOIN (SELECT LEFT(contacts.ZipCode,2) as Zip,
AVG(acts.Amount) as Aver
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'PA'
GROUP by 1) as r
ON q.Zip = r.Zip
GROUP BY q.depname"

table2=sqlQuery(db, query2)

matche<-match.map(france,table2$depname,exact=TRUE)
gray.colors<-function(n) gray(rev(0:(n-1)/1.5)/n)
colors<-cm.colors(max(table2$Aver))[table2$Aver[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)

##########################################################################################
# Number of donations DO
##########################################################################################

query3="SELECT q.depname, r.Numdon
FROM (Select departement_code as Zip, departement_nom as depname
From departements
Group by 1) as q
JOIN (SELECT LEFT(contacts.ZipCode,2) as Zip,
COUNT(acts.Amount) as Numdon
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'DO'
GROUP by 1) as r
ON q.Zip = r.Zip
GROUP BY q.depname"

table3=sqlQuery(db, query3)

matche<-match.map(france,table3$depname,exact=TRUE)
gray.colors<-function(n) gray(rev(0:(n-1)/10)/n)
colors<-terrain.colors(max(table3$Numdon))[table3$Numdon[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)

##########################################################################################
# Number of donations PA
##########################################################################################

query4="SELECT q.depname, r.Numdon
FROM (Select departement_code as Zip, departement_nom as depname
From departements
Group by 1) as q
JOIN (SELECT LEFT(contacts.ZipCode,2) as Zip,
COUNT(acts.Amount) as Numdon
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'PA'
GROUP by 1) as r
ON q.Zip = r.Zip
GROUP BY q.depname"

table4=sqlQuery(db, query4)

matche<-match.map(france,table4$depname,exact=TRUE)
gray.colors<-function(n) gray(rev(0:(n-1)/10)/n)
colors<-terrain.colors(max(table4$Numdon))[table4$Numdon[matche]]
map(database="france", fill=TRUE,col=colors,resolution=0)