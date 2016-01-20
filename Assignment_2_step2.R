####################################################################
# AVERAGE DONATION AMOUNT PREDICTION
####################################################################


#Load packages
library(RODBC)
library(BTYD)
library(ggplot2)
library(AER)
library(censReg)

#Predicting average donation amount
#Open ODBC Connection
db = odbcConnect("myodbc", uid="root", pwd="")
sqlQuery(db, "USE charity")

# Extract data from database
query = " SELECT t.ContactId,
          IF(COUNT(f.Amount)=0,0,1) AS 'loyal',
          SUM(IF(f.Amount IS NULL, 0, f.Amount)) AS 'dv',
          IF(COUNT(a.Amount)=0, 0, (DATEDIFF(20120915, MAX(a.ActDate))))/365 AS 'recency',
          IF(COUNT(a.Amount)=0, 0, AVG(a.Amount)) AS 'averagegift',
          IF(COUNT(a.Amount)=0, 0, MAX(a.Amount)) AS 'maxgift',
          IF(COUNT(a.Amount)=0, 0, MIN(a.Amount)) AS 'mingift',
          COUNT(a.Amount) AS 'count',
  		    IF(SUBSTRING(ct.zipcode,1,2) IS NULL, 99, SUBSTRING(ct.zipcode,1,2)) AS 'zipcode',
			    IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS 'gender'
       
          FROM actions AS t

          LEFT JOIN (SELECT * FROM Acts
			      WHERE (ActType='DO')
            AND (ActDate<=20120915)) AS a
          ON a.ContactId=t.ContactId

          LEFT JOIN contacts AS ct
          ON ct.ContactId = t.ContactId

          LEFT JOIN (SELECT ContactID, Amount FROM ACTS
                      WHERE (MessageId LIKE '12F13%')) f
          ON f.ContactId=t.ContactId
          
          WHERE (t.MessageId LIKE '12F13%')
          GROUP BY t.ContactId"

donationdata=sqlQuery(db,query)
print(head(donationdata))

# Close the connection
odbcClose(db)

# NOTE:  Some donors (6, if I remember correctly) gave 2 donations in response to this single solicitation. So, we want to sum it up
#MOreover, Just to ensure that if somebody has not donated after being solicited for this MessageId "%12F13%, we get a '0' in the variable column instead of a zero value

# Create dummy variables
binomzipcode <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=donationdata))
binomgender <- data.frame(model.matrix( ~ gender - 1, data=donationdata))

# Concatenate dummy variables
binomdata = cbind(binomzipcode[,2:97], binomgender[,1:3])

# Concatenate every data to be used
data = cbind(donationdata[,1:8], binomdata)

rownames(data) = data$ContactId
data = data[, -1]


#test model
initialmodel<-glm(dv ~ . - loyal, data=data,family=poisson(link="log"),control=list(maxit=200))
summary(initialmodel)
#graphical check for data fit
qplot(x=fitted.values(initialmodel),y=donationdata$dv,ylim=c(0,100),xlim=c(0,100))
print(logLik(initialmodel))

#Include predicted donations in dataset
donationdata$predicteddonation=fitted.values(initialmodel)