####################################################################
# PARETO NBD
####################################################################

#Load packages
library(RODBC)
library(BTYD)
library(ggplot2)
library(AER)
library(censReg)

#Open ODBC Connection
db = odbcConnect("myodbc", uid="root", pwd="")
sqlQuery(db, "USE charity")

# Extract data from database
query = "SELECT 
          t.ContactId,
          IF(COUNT(a.Amount)=0, 0, (DATEDIFF(20120915,MIN(a.ActDate))))/365 AS 'T.cal',
          IF(COUNT(a.Amount)=0, 0, (DATEDIFF(MAX(a.ActDate),MIN(a.ActDate))))/365 AS 't.x',
          IF(COUNT(a.Amount)=0, 0, (COUNT(a.Amount)-1)) AS 'x'

        FROM actions t

        LEFT JOIN (SELECT * FROM Acts
        			WHERE (ActType='DO')
              AND (ActDate<=20120915)) AS a
        
        ON a.ContactId=t.ContactId
        
        WHERE (t.MessageId LIKE '12F13%')
        GROUP BY t.ContactId;"
clvdata=sqlQuery(db,query)
print(head(clvdata))

#Model

# Close the connection
odbcClose(db)

# Estimate parameters for the Pareto-NBD process
params = pnbd.EstimateParameters(clvdata)
print(params)
print(pnbd.cbs.LL(params,clvdata))

# Plot heterogeneity in drop-out and purchase processes
pnbd.PlotDropoutRateHeterogeneity(params)
pnbd.PlotTransactionRateHeterogeneity(params)

# Plot goodness of fit (10 purchases)
pnbd.PlotFrequencyInCalibration(params, clvdata, 10)

# Get individual P(Alive) estimates
for (i in 1:length(clvdata$x))
  {
    clvdata$ltv[i] = pnbd.ConditionalExpectedTransactions(params, T.star = 0.2, clvdata$x[i], clvdata$t.x[i], clvdata$T.cal[i])
  }
print(clvdata[1:10, ])

qplot(clvdata$ltv)