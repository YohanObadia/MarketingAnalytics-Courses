library(RODBC)
library(ggplot2)
library(igraph)
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

##########################################
# HIERARCHICAL SEGMENTATION
##########################################

query = "SELECT ContactId,
            DATEDIFF(20131101, MAX(ActDate)) / 365 AS 'recency',
            COUNT(Amount) AS 'frequency',
            AVG(Amount) AS 'avgamount',
            DATEDIFF(20131101, MIN(ActDate)) / 365 AS 'firstdonation'
            FROM acts
            WHERE ActType = 'DO'
            GROUP BY 1
            ORDER BY 1;"

data = sqlQuery(db, query)

memory.limit(5000)

# Assign contact id as row names, remove id from data
rownames(data) = data$ContactId
data = data[, -1]
# Compute distance metrics on standardized data
d = dist(scale(data))
# Perform hierarchical clustering on distance metrics
# The method will return a list with plenty of information
c = hclust(d, method="ward.D")
# Plot the dendogram
plot(c)

# Cut at 5 segments
members = cutree(c, k=5)
# Show profile of each segment
for (i in 1:5) {
  print(colMeans(data[members == i, ]))
}