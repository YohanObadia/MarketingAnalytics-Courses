library(RODBC)
library(ggplot2)
library(igraph)
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

############################################################################################
# Gender analysis
############################################################################################

#In the database :
query8  = "SELECT IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS sex, COUNT(DISTINCT(contactid)) AS numberpeople
FROM contacts
GROUP BY sex
ORDER BY COUNT(DISTINCT(contactid)) DESC;"
nbsex   = sqlQuery(db, query8)

ggplot(data=nbsex, aes(x=factor(sex),y=numberpeople, fill=factor(numberpeople)))+
  geom_bar(stat="identity") + xlab("Gender")+ ylab("number of people") +
  scale_fill_discrete(name="Scale") +
  ggtitle('People by gender in the database') +
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=18,face="bold"),
        plot.title=element_text(size=20,face="bold",vjust=2))

#Donators and donation amount :
query9    = "SELECT IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS sex, COUNT(DISTINCT(a.contactid)) As numberpeoplesexdon, SUM(a.amount) AS Donations
FROM contacts AS c, acts AS a
WHERE c.contactid = a.contactid
GROUP BY sex
ORDER BY COUNT(DISTINCT(a.contactid)) DESC;"
nbsexdon  = sqlQuery(db, query9)
avggendon = nbsexdon[1:4,3]/nbsexdon[1:4,2]
nbsexdon  = cbind(nbsexdon, avggendon)

ggplot(data=nbsexdon, aes(x=factor(sex),y=numberpeoplesexdon, fill=factor(numberpeoplesexdon)))+
  geom_bar(stat="identity") + xlab("Gender")+ ylab("number of people") +
  scale_fill_discrete(name="Scale") +
  ggtitle('People by gender in the database') +
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=18,face="bold"),
        plot.title=element_text(size=20,face="bold",vjust=2))

#Actions and number of actions :
query10   = "SELECT IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS sex, COUNT(DISTINCT(a.contactid)) AS nbcontacted, COUNT(a.messageid) AS nbmsg
FROM charity.contacts AS c, charity.actions AS a 
WHERE c.contactid = a.contactid 
GROUP BY sex
ORDER BY COUNT(DISTINCT(a.contactid)) DESC;"
nbsexmsg  = sqlQuery(db, query10)

#Concatenate all previous results :
sexanalysis = cbind(nbsex, nbsexdon[,2:4],nbsexmsg[,2:3])