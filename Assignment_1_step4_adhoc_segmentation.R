library(RODBC)
library(ggplot2)
library(igraph)
db = odbcConnect("R-MySQL Connection", uid="root", pwd="1Yundai&1")
sqlQuery(db, "USE charity")

############################################################################################
# Ad Hoc Segmentation
############################################################################################

#Period 0 --> 1
period ="SELECT period0.Segment As finalsegment, period1.Segment As originsegment, COUNT(period1.Segment) As nbpeople
FROM segments period0,
segments period1
WHERE (period0.ContactId = period1.ContactId) AND
(period1.PeriodId = 1) AND
(period0.PeriodId = 0)
GROUP BY 1, 2
ORDER BY 1, 2;"

period10 = sqlQuery(db, period)

#Period 1 --> 2
period ="SELECT period1.Segment As finalsegment, period2.Segment As originsegment, COUNT(period2.Segment) As nbpeople
FROM segments period1,
segments period2
WHERE (period1.ContactId = period2.ContactId) AND
(period1.PeriodId = 1) AND
(period2.PeriodId = 2)
GROUP BY 1, 2
ORDER BY 1, 2;"

period21 = sqlQuery(db, period)

#Period 2 --> 3
period ="SELECT period2.Segment As finalsegment, period3.Segment As originsegment, COUNT(period3.Segment) As nbpeople
FROM segments period2,
segments period3
WHERE (period2.ContactId = period3.ContactId) AND
(period2.PeriodId = 2) AND
(period3.PeriodId = 3)
GROUP BY 1, 2
ORDER BY 1, 2;"

period32 = sqlQuery(db, period)

#Period 3 --> 4
period ="SELECT period3.Segment As finalsegment, period4.Segment As originsegment, COUNT(period4.Segment) As nbpeople
FROM segments period3,
segments period4
WHERE (period3.ContactId = period4.ContactId) AND
(period3.PeriodId = 3) AND
(period4.PeriodId = 4)
GROUP BY 1, 2
ORDER BY 1, 2;"

period43 = sqlQuery(db, period)

#Period 4 --> 5
period ="SELECT period4.Segment As finalsegment, period5.Segment As originsegment, COUNT(period5.Segment) As nbpeople
FROM segments period4,
segments period5
WHERE (period4.ContactId = period5.ContactId) AND
(period4.PeriodId = 4) AND
(period5.PeriodId = 5)
GROUP BY 1, 2
ORDER BY 1, 2;"

period54 = sqlQuery(db, period)

#Period 5 --> 6
period ="SELECT period5.Segment As finalsegment, period6.Segment As originsegment, COUNT(period6.Segment) As nbpeople
FROM segments period5,
segments period6
WHERE (period5.ContactId = period6.ContactId) AND
(period5.PeriodId = 5) AND
(period6.PeriodId = 6)
GROUP BY 1, 2
ORDER BY 1, 2;"

period65 = sqlQuery(db, period)

#Period 6 --> 7
period ="SELECT period6.Segment As finalsegment, period7.Segment As originsegment, COUNT(period7.Segment) As nbpeople
FROM segments period6,
segments period7
WHERE (period6.ContactId = period7.ContactId) AND
(period6.PeriodId = 6) AND
(period7.PeriodId = 7)
GROUP BY 1, 2
ORDER BY 1, 2;"

period76 = sqlQuery(db, period)

#Period 7 --> 8
period ="SELECT period7.Segment As finalsegment, period8.Segment As originsegment, COUNT(period8.Segment) As nbpeople
FROM segments period7,
segments period8
WHERE (period7.ContactId = period8.ContactId) AND
(period7.PeriodId = 7) AND
(period8.PeriodId = 8)
GROUP BY 1, 2
ORDER BY 1, 2;"

period87 = sqlQuery(db, period)

#Period 8 --> 9
period ="SELECT period8.Segment As finalsegment, period9.Segment As originsegment, COUNT(period9.Segment) As nbpeople
FROM segments period8,
segments period9
WHERE (period8.ContactId = period9.ContactId) AND
(period8.PeriodId = 8) AND
(period9.PeriodId = 9)
GROUP BY 1, 2
ORDER BY 1, 2;"

period98 = sqlQuery(db, period)

#Period 9 --> 10
period ="SELECT period9.Segment As finalsegment, period10.Segment As originsegment, COUNT(period10.Segment) As nbpeople
FROM segments period9, 
segments period10
WHERE (period9.ContactId = period10.ContactId) AND
(period9.PeriodId = 9) AND
(period10.PeriodId = 10)
GROUP BY 1, 2
ORDER BY 1, 2;"

period109 = sqlQuery(db, period)

#Concatenate by rows for all period in descending order
totperiod = rbind(period10, period21, period32, period43, period54, period65, period76, period87, period98, period109)

################
################
# NODES
################
################

#Details for this query: we exclude finalsegment = NA 
#to remove all period where people did not start to give 
#and I group on originsegment to  have the NEW Segment

graph = "SELECT originsegment, SUM(numberpeople)
FROM segmentmoves
WHERE (finalsegment<>'<NA>')
GROUP BY 1;"

nodes = sqlQuery(db, graph)


################
################
# EDGES
################
################

#Details for this query: We only take the originsegment and 
#final segment when they are different to evaluate movements 
#between different segments 

graph = "SELECT originsegment, finalsegment, SUM(numberpeople)
FROM segmentmoves
WHERE (finalsegment<>'<NA>') AND (originsegment <> finalsegment)
GROUP BY 1, 2;"

edges = sqlQuery(db, graph)

test=edges[,1:2]
###############################
#DRAW THE NODES AND EDGES GRAPH
###############################

relations <- data.frame(from=edges[,1],
                        to=edges[,2])


#For directed arrows, set directed to TRUE, and for undirected arrows, set it to FALSE
g <- graph.data.frame(relations, directed=TRUE)
V(g)$names = nodes[,1]

#Define sizes for nodes and edges
node.size=setNames((nodes[,2])/500,V(g)$names)
weight=(edges[,3])/300

#Plot the graph
plot(g, edge.width=weight, vertex.size=node.size)