# We're going to divide the past in periods
# Create a table to store period information
CREATE TABLE periods (
  PeriodId INTEGER NOT NULL,
  FirstDay DATE NOT NULL,
  LastDay DATE NOT NULL,
  PRIMARY KEY (PeriodId)
)
ENGINE = MyISAM;

# Define 11 periods
# Period 0 = the most recent ("today")
INSERT INTO periods
VALUES ( 0, 20121101, 20131031),
       ( 1, 20111101, 20121031),
       ( 2, 20101101, 20111031),
       ( 3, 20091101, 20101031),
       ( 4, 20081101, 20091031),
       ( 5, 20071101, 20081031),
       ( 6, 20061101, 20071031),
       ( 7, 20051101, 20061031),
       ( 8, 20041101, 20051031),
       ( 9, 20031101, 20041031),
       (10, 20021101, 20031031);


# Create a segment table
# It will store to which segment each donor belonged
# in each period
CREATE TABLE segments (
  Sq INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
  ContactId INTEGER UNSIGNED NOT NULL,
  PeriodId INTEGER NOT NULL,
  Segment VARCHAR(9),
  PRIMARY KEY (Sq),
  INDEX IdxContactId(ContactId),
  INDEX IdxPeriodId(PeriodId)
)
ENGINE = MyISAM;


# This will create a placeholder for all
# contact-by-period possible combinations
INSERT INTO segments (ContactId, PeriodId)
SELECT a.ContactId, p.PeriodId
FROM acts a,
     periods p
GROUP BY 1, 2;

#For all donations superior to 1000€ I assume that it is a more important information than being a new donor or an auto donor
UPDATE
  segments s,
  (SELECT ContactId, PeriodId, SUM(Amount) AS generosity
   FROM   charity.acts a, charity.periods p
   WHERE  (a.ActDate <= p.LastDay) AND
          (a.ActDate >= p.FirstDay) AND
          (a.ActType LIKE 'DO')
   GROUP BY 1, 2) AS d
SET
  s.Segment = "GOLD"
WHERE
  (s.ContactId = d.ContactId) AND
  (s.PeriodId = d.PeriodId) AND
  (generosity >= 1000);

# Create the NEW segment which we consider more important than being an auto donor or a bottom/middle/top donor
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM periods p,
        (SELECT ContactId, MIN(ActDate) AS FirstAct
         FROM charity.acts
         GROUP BY 1) AS f
   WHERE (f.FirstAct <= p.LastDay) AND
         (f.FirstAct >= p.FirstDay)) AS d
SET
  s.Segment = "NEW"
WHERE
  (s.ContactId = d.ContactId) AND
  (s.PeriodId = d.PeriodId) AND
  (s.Segment IS NULL);


# Create the AUTO segment
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   charity.acts a, charity.periods p
   WHERE  (a.ActDate <= p.LastDay) AND
          (a.ActDate >= p.FirstDay) AND
          (a.ActType LIKE 'PA')) AS d
SET
  s.Segment = "AUTO"
WHERE
  (s.ContactId = d.ContactId) AND
  (s.PeriodId = d.PeriodId) AND
  (s.Segment IS NULL);


# Createthe BOTTOM/MIDDLE/TOP segment
# We the results of the hierarchical segmentation and restructure it in order to define possible interesting strategies
UPDATE
  segments s,
  (SELECT ContactId, PeriodId, SUM(Amount) AS generosity
   FROM   charity.acts a, charity.periods p
   WHERE  (a.ActDate <= p.LastDay) AND
          (a.ActDate >= p.FirstDay) AND
          (a.ActType LIKE 'DO')
   GROUP BY 1, 2) AS d
SET
  s.Segment = IF(generosity < 50, "BOTTOM", IF(generosity < 200, "MIDDLE","TOP"))
WHERE
  (s.ContactId = d.ContactId) AND
  (s.PeriodId = d.PeriodId) AND
  (s.Segment IS NULL);

# Create the WARM segment
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  (Segment LIKE "NEW")    OR
          (Segment LIKE "AUTO")   OR
          (Segment LIKE "BOTTOM") OR
          (Segment LIKE "MIDDLE") OR
          (Segment LIKE "TOP")) AS a
SET
  s.Segment = "WARM"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);

# Create the WARM GOLD segment to specifically follow the track of GOLD donors
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  (Segment LIKE "GOLD")) AS a
SET
  s.Segment = "WARMGOLD"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);

# Create the COLD segment
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  Segment LIKE "WARM") AS a
SET
  s.Segment = "COLD"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);

# Create the COLD GOLD segment
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  Segment LIKE "WARM GOLD") AS a
SET
  s.Segment = "COLD GOLD"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);

# Create the LOST segment. We applyed this query until there was no change anymore.
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  (Segment LIKE "COLD") OR
          (Segment LIKE "LOST")) AS a
SET
  s.Segment = "LOST"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);

# Create the LOST GOLD segment. No one was in this segment.
UPDATE
  segments s,
  (SELECT ContactId, PeriodId
   FROM   segments
   WHERE  (Segment LIKE "COLD GOLD") OR
          (Segment LIKE "LOST GOLD")) AS a
SET
  s.Segment = "LOST GOLD"
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = a.PeriodId - 1) AND
  (s.Segment IS NULL);


# Count segment members per period
SELECT PeriodId, Segment, COUNT(*)
FROM segments
GROUP BY 1, 2
ORDER BY 2, 1 DESC;


# In which segments were donors last period,
# and where are they now?

# ------> See requests in R : Assignment_1_step4_adhoc_segmentation.R

#################################################################
#Create the segmentmoves table to track aggregated segments moves 
#################################################################

CREATE TABLE segmentmoves (
  Sq INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
  originsegment VARCHAR(9),
  finalsegment VARCHAR(9),
  numberpeople INTEGER NOT NULL,
  PRIMARY KEY (Sq)
  )
ENGINE = MyISAM;

LOAD DATA LOCAL INFILE 'C:/Users/yohan/Dropbox/Essec/Marketing Analytics/Session 3/segmentswitch.txt' INTO TABLE segmentmoves;

#####################################
# Dynamic analysis
#####################################

# Report the financial contribution of each segment at every periods
SELECT
  YEAR(a.actdate),
  s.Segment,
  COUNT(DISTINCT(s.ContactId)) AS 'numdonors',
  COUNT(a.Amount)              AS 'numdonations',
  CEILING(AVG(a.Amount))       AS 'avgamount',
  CEILING(SUM(a.Amount))       AS 'totalgenerosity'
FROM
  segments s,
  periods p,
  acts a
WHERE
  (s.ContactId = a.ContactId) AND
  (s.PeriodId = p.PeriodId) AND
  (a.ActDate >= p.FirstDay) AND
  (a.ActDate <= p.LastDay)
GROUP BY 1,2
ORDER BY 1,2 DESC;