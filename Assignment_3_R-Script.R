#Load packages
library(RODBC)
library(smbinning)
library(ggplot2)
library(splines)

#Open ODBC Connection
db = odbcConnect("MySQL_R_Connection", uid="root", pwd="")
sqlQuery(db, "USE meteo")

#Load data (we exclude unlikely/impossible temperature and humidity data)
query= "SELECT (hour*60 + minute) as minute,
              quotation,
              pbauto,
              pbmoto,
              pbrachat,
              pbperso,
              pbtrav,
              rev,
              temp,
              humidity,
              pressure,
              weathertype
        FROM visitsub
        WHERE (temp>-20) AND (temp<25) and (humidity<=100)
        LIMIT 3000000;"

visit=sqlQuery(db,query)

#Smbinning analysis of visit
str(visit) # Quick description of the data
table(visit$quotation) # Tabulate target variable


#We start with a global analysis before going a bit deeper


################
#Minute analysis
################

# Package application
result=smbinning(df=visit,y="quotation",x="minute",p=0.05) # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree from partykit

# Plots
par(mfrow=c(2,2))
boxplot(visit$minute~visit$quotation,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")
mtext("Time on quotation (minute)",3)
smbinning.plot(result,option="dist",sub="Time on quotation (minute)")
smbinning.plot(result,option="badrate",sub="Time on quotation (minute)")
smbinning.plot(result,option="WoE",sub="Time on quotation (minute)")

qplot(visit$minute, visit$quotation, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.02))


#####################
#Temperature analysis
#####################

# Package application
result=smbinning(df=visit,y="quotation",x="temp",p=0.05) # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree from partykit

# Plots
par(mfrow=c(2,2))
boxplot(visit$temp~visit$quotation,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")
mtext("Weather on quotation (temperature)",3)
smbinning.plot(result,option="dist",sub="Weather on quotation (temperature)")
smbinning.plot(result,option="badrate",sub="Weather on quotation (temperature)")
smbinning.plot(result,option="WoE",sub="Weather on quotation (temperature)")

qplot(visit$temp, visit$quotation, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.03))


##################
#Humidity analysis
##################

# Package application
result=smbinning(df=visit,y="quotation",x="humidity",p=0.05) # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree from partykit

# Plots
par(mfrow=c(2,2))
boxplot(visit$humidity~visit$quotation,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")
mtext("Weather on quotation (humidity)",3)
smbinning.plot(result,option="dist",sub="Weather on quotation (humidity)")
smbinning.plot(result,option="badrate",sub="Weather on quotation (humidity)")
smbinning.plot(result,option="WoE",sub="Weather on quotation (humidity)")

qplot(visit$humidity, visit$quotation, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.03))

################
#Amount analysis
################

## Extract a new data set with only quotation demands to avoid bias.
## There was some data for which quotation=1 and amount IS NULL.
query="SELECT productid,
              (hour*60 + minute) as minute,
              amount,
              pbauto,
              pbmoto,
              pbrachat,
              pbperso,
              pbtrav,
              rev,
              temp,
              humidity
              FROM visitsub
              WHERE (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

quotamount=sqlQuery(db,query)

## Draw the graphs of quotation amount and minute/temp/humidity
c <- ggplot(quotamount, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3))

c <- ggplot(quotamount, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3))

c <- ggplot(quotamount, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3))

######
## Introduce the analysis per product type
#####

###################
#Frequency analysis
###################

# PBAUTO
qplot(visit$minute, visit$pbauto, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$temp, visit$pbauto, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$humidity, visit$pbauto, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

#PBMOTO : too fiew observations to notice any effect
#    qplot(visit$minute, visit$pbmoto, data=visit) + 
#      stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
#      coord_cartesian(ylim = c(0, 0.003))
    
#    qplot(visit$temp, visit$pbmoto, data=visit) + 
#      stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
#      coord_cartesian(ylim = c(0, 0.003))
    
#    qplot(visit$humidity, visit$pbmoto, data=visit) + 
#      stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
#      coord_cartesian(ylim = c(0, 0.003))

#PBPERSO
qplot(visit$minute, visit$pbperso, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.02))

qplot(visit$temp, visit$pbperso, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.02))

qplot(visit$humidity, visit$pbperso, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.02))

#PBRACHAT
qplot(visit$minute, visit$pbrachat, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$temp, visit$pbrachat, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$humidity, visit$pbrachat, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

#PBTRAV
qplot(visit$minute, visit$pbtrav, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$temp, visit$pbtrav, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

qplot(visit$humidity, visit$pbtrav, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.003))

#REV
qplot(visit$minute, visit$rev, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.01))

qplot(visit$temp, visit$rev, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.01))

qplot(visit$humidity, visit$rev, data=visit) + 
  stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 3)) +
  coord_cartesian(ylim = c(0, 0.01))



################
#Amount analysis
################

#PBAUTO
query="SELECT amount,
              (hour*60 + minute) as minute,
              temp,
              humidity
          FROM visitsub
          WHERE (productid=PBAUTO) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

pbautodata=sqlQuery(db,query)

c <- ggplot(pbautodata, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBAUTO")

c <- ggplot(pbautodata, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBAUTO")

c <- ggplot(pbautodata, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBAUTO")

#PBMOTO (Not enough data)
#query="SELECT amount,
#              (hour*60 + minute) as minute,
#              temp,
#              humidity
#          FROM visitsub
#          WHERE (productid=PBMOTO) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"
#
#pbmotodata=sqlQuery(db,query)
#
#c <- ggplot(pbmotodata, aes(minute, amount))
#c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
#  ggtitle("Amount analysis for PBMOTO")
#
#c <- ggplot(pbmotodata, aes(temp, amount))
#c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
#  ggtitle("Amount analysis for PBMOTO")
#
#c <- ggplot(pbmotodata, aes(humidity, amount))
#c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
#  ggtitle("Amount analysis for PBMOTO")

#PBPERSO
query="SELECT amount,
              (hour*60 + minute) as minute,
              temp,
              humidity
        FROM visitsub
        WHERE (productid=PBPERSO) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

pbpersodata=sqlQuery(db,query)

c <- ggplot(pbpersodata, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBPERSO")

c <- ggplot(pbpersodata, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBPERSO")

c <- ggplot(pbpersodata, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBPERSO")


#PBTRAV
query="SELECT amount,
              (hour*60 + minute) as minute,
              temp,
              humidity
          FROM visitsub
          WHERE (productid=PBTRAV) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

pbtravdata=sqlQuery(db,query)

c <- ggplot(pbtravdata, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBTRAV")

c <- ggplot(pbtravdata, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBTRAV")

c <- ggplot(pbtravdata, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBTRAV")

#PBRACHAT
query="SELECT amount,
              (hour*60 + minute) as minute,
              temp,
              humidity
          FROM visitsub
          WHERE (productid=PBRACHAT) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

pbrachatdata=sqlQuery(db,query)

c <- ggplot(pbrachatdata, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBRACHAT")

c <- ggplot(pbrachatdata, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBRACHAT")

c <- ggplot(pbrachatdata, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for PBRACHAT")

#REV
query="SELECT amount,
              (hour*60 + minute) as minute,
              temp,
              humidity
          FROM visitsub
          WHERE (productid=REV) and (temp>-20) AND (temp<25) and (quotation=1) and not(isnull(amount)) and (humidity<=100);"

revdata=sqlQuery(db,query)

c <- ggplot(revdata, aes(minute, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for REV")

c <- ggplot(revdata, aes(temp, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for REV")

c <- ggplot(revdata, aes(humidity, amount))
c + stat_smooth(method = "glm", formula = y ~ ns(x,3)) + 
  ggtitle("Amount analysis for REV")