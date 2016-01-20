####################################################################
# PROBABILITY ESTIMATION
####################################################################


#############
#Before starting, just so you know, at the end of the file, you can find the failed 
#attempt to create the Standard deviation of time gap between 2 donations.
#############

# Load the package
library(RODBC)
library(ggplot2)
library(igraph)
library(nnet)
db = odbcConnect("myodbc", uid="root", pwd="")
sqlQuery(db, "USE charity")

# Extract data from database
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

# Create dummy variables
binomzipcode <- data.frame(model.matrix( ~ factor(zipcode) - 1, data=datainitial))
binommonth <- data.frame(model.matrix( ~ factor(month) - 1, data=datainitial))
binommonth <-binommonth[,3:13]
binomgender <- data.frame(model.matrix( ~ gender - 1, data=datainitial))

# Concatenate dummy variables
binomdata = cbind(binomgender[,1:3], binommonth[,], binomzipcode[,2:95])

#Data from the Pareto model
pareto <- read.csv("/Users/clementmondesert/desktop/clvdata.csv", header = TRUE)

# Concatenate every data to be used without reg standard deviation because it is not working...
data = cbind(datainitial[,1:6], binomdata)

# include the Pareto values (WARNING : The loop computation takes a lifetime to run... It is worth trying to improve it)
paretomatrix <- matrix(ncol=2, nrow=6665)
names(paretomatrix) <- c("contactid", "pproba")

for (i in 1:nrow(data))
{
  for (j in 1:nrow(pareto))
  if(data$contactid[i]==pareto$ContactId[j])
  {
    paretomatrix[i,1]=data$contactid[i]
    paretomatrix[i,2]=pareto$ltv[j]
  }
}

# Add the pproba to the data table to be able to run the model with it
data  = cbind(data, paretomatrix[,2])

# Assign contact id as row names, remove id from data
rownames(data) = data$ContactId
data = data[, -1]

# Logit model (removing march month for colinarity purpose)
model = multinom(formula = loyal ~  . - avgamount + exp(firstdonation/3) + recency*sin(0.3*recency)^4 + log(frequency+0.1), data = data)

# Multi-Fold Cross-Validation
nfold = 5
nobs = nrow(data)
index = rep(1:nfold, length.out = nobs)
probs = rep(0, nobs)
for (i in 1:nfold) {
  # Assign in-sample and out-of-sample observations
  insample = which(index != i)
  outsample = which(index == i)
  # Run model on in-sample data only
  submodel = multinom(formula = loyal ~ . - avgamount + exp(firstdonation/3) + recency*sin(0.3*recency)^4 + log(frequency+0.1), data[insample, ])
  # Obtain predicted probabilities on out-of-sample data
  probs[outsample] = predict(object = submodel, newdata = data[outsample, ], type = "probs")
}

# Build the Gain Chart
  # Rank order target variable in decreasing order of (predicted) probability
  target = data$loyal[order(probs, decreasing=TRUE)] / sum(data$loyal)
  gainchart = c(0, cumsum(target))

  # Create a random selection sequence
  random = seq(0, to = 1, length.out = length(data$loyal))

  # Create the "perfect" selection sequence
  perfect = data$loyal[order(data$loyal, decreasing=TRUE)] / sum(data$loyal)
  perfect = c(0, cumsum(perfect))

  # Plot gain chart, add random line
  plot(gainchart)
  lines(random)
  lines(perfect)

# Get coefficients, standard errors
coeff = t(summary(model)$coefficients)
stder = t(summary(model)$standard.errors)
zvalues = coeff / stder
pvalues = (1 - pnorm(abs(zvalues), 0, 1)) * 2

# Print results
print("coefficients:")
print(coeff)
print("standard deviations:")
print(stder)
print("p-values")
print(pvalues)


# Compute 5%, 10%, and 25% lift and improvement
q = c(0.01, 0.05, 0.10, 0.25)
x = quantile(gainchart, probs = q)
z = quantile(perfect,   probs = q)

print("Hit rate:")
print(x)
print("Lift:")
print(x/q)
print("Improvement:")
print((x-q)/(z-q))



#############
#Here is the failed attempt to create the Standard deviation of time gap between 2 donations.
#############



# Create the reg variable (regularity of donation) by importing a csv file from an sql query and computin the reg standard deviation (fail)
regvar <- read.csv("C:/Users/yohan/Dropbox/Essec/Marketing Analytics/Session 6/Assignment 2 - Group 1/reg_query.csv")

nrowdata <- nrow(datainitial)
nrowreg <- nrow(datainitial)
regsd <- matrix(,nrowdata,3)
colnames(regsd) <- c("ContactID", "Std", "Mean");
intermed <- vector(mode="numeric", length=0)

i=1
j=1
n=1
count=1
cid=0

for (i in 278:nrowdata)
{
  count=count+1
  
  if (datainitial$frequency[i]==1)
  {
    regsd[i,1] <- datainitial[i,1]
    cid <- datainitial[i,1]
    regsd[i,2] <- 0
    regsd[i,3] <- 0
  }
  
  else
  {
    for (j in 1:nrowreg)
    {
      if(datainitial$contactid[i]==regvar$ContactID[j])
      {
        p=1
        n=j
        cid <- datainitial[i,1]
        while(regvar$ContactID[j]==regvar$ContactID[n])
        {
          intermed[p] <- regvar$reg[n]
          p <- p+1
          n <- n+1
        }
        regsd[i,1] <- datainitial[i,1]
        regsd[i,2] <- sd(intermed)
        regsd[i,3] <- mean(intermed)
        intermed <- intermed[-1:-p]
        break
      }   
    }
  }
}