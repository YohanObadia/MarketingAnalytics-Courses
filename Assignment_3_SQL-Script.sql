######################################
#Create the main data we will be using
######################################

USE meteo;

#Create the table to be used for analysis
create table visit (
 ruid VARCHAR(40) DEFAULT NULL,
 visitdate DATETIME DEFAULT NULL,
 url TEXT DEFAULT NULL,
 quotation INT UNSIGNED DEFAULT NULL,
 quotationid BIGINT UNSIGNED DEFAULT NULL,
 amount INT UNSIGNED DEFAULT NULL,
 quotationae INT UNSIGNED DEFAULT NULL,
 eulerianid BIGINT UNSIGNED DEFAULT NULL,
 productid VARCHAR(30) DEFAULT NULL,
 city VARCHAR(100) DEFAULT NULL,
 weathertype VARCHAR(30) DEFAULT NULL,
 temp INT DEFAULT NULL,
 pressure INT DEFAULT NULL,
 humidity INT DEFAULT NULL,
 KEY Idxruid (ruid),
 KEY Idxquotation (quotation),
 KEY Idxproductid (productid),
 KEY Idxcity (city),
 KEY Idxweathertype (weathertype))
ENGINE = MyISAM;


LOAD DATA INFILE 'E:/data/zsk_yob_visites_finales.csv' 
INTO TABLE visit 
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(@ruid, @visitdate, @url, @quotation, @quotationid, @amount, @quotationae, @eulerianid, @productid, @city, @weathertype, @temp, @pressure, @humidity)
SET ruid = if(@ruid='',NULL,@ruid),
	visitdate = if(@visitedate='',NULL,@visitdate),
 url = if(@url='',NULL,@url),
 quotation = if(@quotation='',NULL,@quotation),
 quotationid = if(@quotationid = '',NULL,@quotationid),
 amount = if(@amount='',NULL,@amount),
 quotationae = if(@quotationae='',NULL,@quotationae),
 eulerianid = if(@eulerianid='',NULL,@eulerianid),
 productid = if(@productid='',NULL,@productid),
 city = if(@city='',NULL,@city),
 weathertype = if(@weathertype='',NULL,@weathertype),
 temp = if(@temp='',NULL,@temp),
 pressure = if(@pressure='',NULL,@pressure),
 humidity = if(@humidity='',NULL,@humidity);
 

#Create a useful subtable with the data structured as needed
CREATE TABLE visitsub
	SELECT 	IF(productid LIKE '%PBAUTO%', 'PBAUTO', IF(productid LIKE '%PBMOTO%', 'PBMOTO', IF(productid LIKE '%PBPERSO%', 'PBPERSO', IF(productid LIKE '%PBTRAV%', 'PBTRAV', IF(productid LIKE '%PBRACHAT%', 'PBRACHAT', IF(productid LIKE '%REV%', 'REV', IF(ISNULL(productid), NULL, 'OTHER' ))))))) as productid,
			visitdate,
			minute(visitdate) as minute,
			hour(visitdate) as hour,
			weekday(visitdate) as weekday,
			quotation,
			amount,
			city,
			weathertype,
			temp,
			pressure,
			humidity
		FROM visit
		WHERE NOT(ISNULL(city)) AND NOT(ISNULL(weathertype));

#Adding dummy variables for all product types        
ALTER TABLE visitsub 	ADD COLUMN	pbauto 		tinyint,
						ADD COLUMN	pbmoto 		tinyint,
						ADD COLUMN  pbperso 	tinyint,
						ADD COLUMN	pbtrav 		tinyint,
						ADD COLUMN  rev 		tinyint,
						ADD COLUMN  pbrachat 	tinyint,
						ADD COLUMN	other 		tinyint;

UPDATE visitsub SET pbauto=IF(productid='PBAUTO','1','0'), 
					pbmoto=IF(productid='PBMOTO','1','0'), 
					pbperso=IF(productid='PBPERSO','1','0'), 
                    pbtrav=IF(productid='PBTRAV','1','0'), 
                    pbrachat=IF(productid='PBRACHAT','1','0'), 
                    rev=IF(productid='REV','1','0'), 
                    other=IF(productid='OTHER','1','0');
