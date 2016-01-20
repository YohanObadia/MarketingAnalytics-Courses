#Load packages
library(RODBC)
library(BTYD)
library(ggplot2)
library(AER)
library(censReg)
library(igraph)
library(nnet)

#########################################################################################
#PROBABILITY MODEL
#########################################################################################

# Connect to MySQL
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

# Extract calibration data from database
query = "SELECT   t.contactid as contactid,
IF(DATEDIFF(20120915, MAX(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MAX(a.actdate)) / 365) AS 'recency',
COUNT(a.amount) AS 'frequency',
IF(AVG(a.amount) IS NULL, 0, AVG(a.amount)) AS 'avgamount',
IF(DATEDIFF(20120915, MIN(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MIN(a.actdate)) / 365) AS 'firstdonation',
IF(c.counter IS NULL, 0, 1) AS 'loyal',
IF(SUBSTRING(ct.zipcode,1,2) IS NULL, 1, SUBSTRING(ct.zipcode,1,2)) AS 'zipcode',
IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS 'gender',
IF(MONTH(a.actdate) IS NULL, 0, MONTH(a.actdate)) AS month
FROM actions AS t

LEFT JOIN contacts AS ct
ON ct.contactid = t.contactid

LEFT JOIN   (SELECT * FROM Acts
WHERE (ActType='DO')
AND (ActDate<=20120915)
) AS a
ON a.contactid = t.contactid

LEFT JOIN 	(SELECT contactid, COUNT(Amount) AS counter
FROM Acts
WHERE (ActDate >= 20120915) AND
(ActDate <  20131101) AND
(ActType = 'DO')
GROUP BY contactid
) AS c
ON c.contactid = t.contactid

WHERE (zipcode REGEXP '^-?[0-9]+$') AND (t.MessageId LIKE '12F13%')
GROUP BY 1;"

data = sqlQuery(db, query)

# Close the connection
odbcClose(db)

# Create dummy variables
binomzipcode <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=data))
binommonth <- data.frame(model.matrix( ~ factor(month) - 1, data=data))
binommonth <-binommonth[,3:13]
binomgender <- data.frame(model.matrix( ~ gender - 1, data=data))

# Concatenate dummy variables
binomdata = cbind(binomgender[,1:3], binommonth[,])

# Concatenate every data to be used without reg standard deviation because it is not working...
data = cbind(data[,1:6], binomdata)

###################################################
###################################################
#Pareto model that could have have been added to improve the model a bit but it was only marginal
#while adding a lot of complexity to the model. Therefore we are leaving it out on purpose.
###################################################
###################################################
# include the Pareto values (WARNING : The loop computation takes a lifetime to run... It is worth trying to improve it)
#paretomatrix <- matrix(ncol=2, nrow=6665)
#names(paretomatrix) <- c("contactid", "pproba")

#for (i in 1:nrow(data))
#{
#  for (j in 1:nrow(pareto))
#    if(data$contactid[i]==pareto$ContactId[j])
#    {
#      paretomatrix[i,1]=data$contactid[i]
#      paretomatrix[i,2]=pareto$ltv[j]
#    }
#}

# Add the pproba to the data table to be able to run the model with it
#data  = cbind(data, paretomatrix[,2])
###################################################
###################################################

# Assign contact id as row names, remove id from data
rownames(data) = data$ContactId
data = data[, -1]

# In-sample, probability model
library(nnet)
prob.model = multinom(formula = loyal ~  . + exp(firstdonation/3) + recency*sin(0.3*recency)^4 + log(frequency+0.1), data = data)

#########################################################################################
#DONATION AMOUNT MODEL
#########################################################################################

# Connect to MySQL
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

# Extract data from database
query = " SELECT t.ContactId as contactid,
SUM(IF(f.Amount IS NULL, 0, f.Amount)) AS 'dv',
IF(COUNT(a.Amount)=0, 0, (DATEDIFF(20120915, MAX(a.ActDate))))/365 AS 'recency',
IF(COUNT(a.Amount)=0, 0, AVG(a.Amount)) AS 'avgamount',
IF(COUNT(a.Amount)=0, 0, MAX(a.Amount)) AS 'maxamount',
IF(COUNT(a.Amount)=0, 0, MIN(a.Amount)) AS 'minamount',
COUNT(a.Amount) AS 'count',
IF(SUBSTRING(ct.zipcode,1,2) IS NULL, 1, SUBSTRING(ct.zipcode,1,2)) AS 'zipcode',
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

WHERE (zipcode REGEXP '^-?[0-9]+$') AND (t.MessageId LIKE '12F13%')
GROUP BY t.ContactId"

donationdata=sqlQuery(db,query)
print(head(donationdata))

# Close the connection
odbcClose(db)

# Create dummy variables
binomzipcode2 <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=donationdata))
binomgender2 <- data.frame(model.matrix( ~ gender - 1, data=donationdata))

# Concatenate dummy variables
binomdata2 = cbind(binomzipcode2[,2:95], binomgender2[,1:3])

# Concatenate every data to be used
data2 <- cbind(donationdata[,1:7], binomdata2)

rownames(data2) <- data2$ContactId
data2 <- data2[, -1]


#test model if you want to
#initialmodel<-glm(dv ~ . - loyal, data=data,family=poisson(link="log"),control=list(maxit=200))
#summary(initialmodel)

# In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...
z = which(!is.na(data2$dv))
print(head(data2[z, ]))
amount.model = glm(dv ~ . , data=data2,family=poisson(link="log"),control=list(maxit=200))


#########################################################################################
#########################################################################################
#PREDICTION DATA = HOLDOUT
#########################################################################################
#########################################################################################


#########################################################################################
#PROBABILITY MODEL
#########################################################################################

db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity_holdout")

# Extract data from database
query = "SELECT   t.contactid as contactid,
IF(DATEDIFF(20120915, MAX(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MAX(a.actdate)) / 365) AS 'recency',
COUNT(a.amount) AS 'frequency',
IF(AVG(a.amount) IS NULL, 0, AVG(a.amount)) AS 'avgamount',
IF(DATEDIFF(20120915, MIN(a.actdate)) IS NULL, 0, DATEDIFF(20120915, MIN(a.actdate)) / 365) AS 'firstdonation',
IF(SUBSTRING(ct.zipcode,1,2) IS NULL, 1, SUBSTRING(ct.zipcode,1,2)) AS 'zipcode',
IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS 'gender',
IF(MONTH(a.actdate) IS NULL, 0, MONTH(a.actdate)) AS month
FROM actions AS t

LEFT JOIN contacts AS ct
ON ct.contactid = t.contactid

LEFT JOIN   (SELECT * FROM Acts
WHERE (ActType='DO')
AND (ActDate<=20120915)
) AS a
ON a.contactid = t.contactid


WHERE (zipcode REGEXP '^-?[0-9]+$')
GROUP BY 1;"

newdata = sqlQuery(db, query)

# Close the connection
odbcClose(db)

# Create dummy variables
binomzipcodeout <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=newdata))
  #Cut the data to fit the model
  binomzipcodeout <- cbind(binomzipcodeout[,1:8],binomzipcodeout[,10:95], binomzipcodeout[97])
binommonthout <- data.frame(model.matrix( ~ factor(month) - 1, data=newdata))
  #Remove the fake 0 column
  binommonthout <- binommonthout[,3:13]
binomgenderout <- data.frame(model.matrix( ~ gender - 1, data=newdata))

# Concatenate dummy variables
binomdataout = cbind(binomgenderout[,1:3], binommonthout[,])

# Concatenate every data to be used without reg standard deviation because it is not working...
dataout = cbind(newdata[,1:5], binomdataout)

#########################################################################################
#DONATION AMOUNT MODEL
#########################################################################################

# Connect to MySQL
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity_holdout")

# Extract data from database
query = "SELECT t.ContactId as contactid,

SUM(IF(f.Amount IS NULL, 0, f.Amount)) AS 'dv',
IF(COUNT(a.Amount)=0, 0, (DATEDIFF(20120915, MAX(a.ActDate))))/365 AS 'recency',
IF(COUNT(a.Amount)=0, 0, AVG(a.Amount)) AS 'avgamount',
IF(COUNT(a.Amount)=0, 0, MAX(a.Amount)) AS 'maxamount',
IF(COUNT(a.Amount)=0, 0, MIN(a.Amount)) AS 'minamount',
COUNT(a.Amount) AS 'count',
IF(SUBSTRING(ct.zipcode,1,2) IS NULL, 1, SUBSTRING(ct.zipcode,1,2)) AS 'zipcode',
IF(prefix = 'M' OR prefix = 'MM', 'Men', IF(prefix = 'ME' OR prefix = 'MLLE' OR prefix = 'MLLES' OR prefix = 'MME' OR prefix = 'MMES', 'Female', IF(prefix= 'M MME' OR prefix = 'MME M', 'Couple', 'Other'))) AS 'gender'

FROM actions AS t

LEFT JOIN (SELECT * FROM Acts
WHERE (ActType='DO')
AND (ActDate<=20120915)) AS a
ON a.contactid=t.contactid

LEFT JOIN contacts AS ct
ON ct.contactid = t.contactid

          LEFT JOIN (SELECT ContactID, Amount FROM ACTS
                      WHERE (MessageId LIKE '12F13%')) f
          ON f.ContactId=t.ContactId

WHERE (zipcode REGEXP '^-?[0-9]+$')

GROUP BY t.contactid"

donationdataout=sqlQuery(db,query)

# Close the connection
odbcClose(db)

# Create dummy variables
binomzipcodeout2 <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=donationdataout))
  #Cut the data to fit the model
  binomzipcodeout2 <- cbind(binomzipcodeout2[,1:8],binomzipcodeout2[,10:95], binomzipcodeout2[97])

binomgenderout2 <- data.frame(model.matrix( ~ gender - 1, data=donationdataout))

# Concatenate dummy variables
binomdataout2 = cbind(binomzipcodeout2[,2:95], binomgenderout2[,1:3])

# Concatenate every data to be used
dataout2 = cbind(donationdataout[,1:7], binomdataout2)


##############################################
##############################################
# Predictions to be done
##############################################
##############################################


# Out-of-sample predictions
# Create the out table storing the results
out = data.frame(contactid = dataout2$contactid)
out$probs = predict(object = prob.model, newdata = dataout, type = "probs")
out$amount = predict(object = amount.model, newdata = dataout2)
out$score  = out$probs * out$amount

# Show results
print(head(out))

# Who is likely to be worth more than 0.75 EUR?
z = which(out$score > 0.75)
print(length(z))