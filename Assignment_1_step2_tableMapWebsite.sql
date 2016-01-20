use charity

#table DO
SELECT LEFT(contacts.ZipCode,2),
		AVG(acts.Amount),
        COUNT(acts.Amount)
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'DO'
GROUP by 1


#table PA
SELECT LEFT(contacts.ZipCode,2),
		AVG(acts.Amount),
        COUNT(acts.Amount)
FROM acts
JOIN contacts
ON contacts.ContactId = acts.ContactId AND acts.ActType = 'PA'
GROUP by 1

#But… some donations do not have a ZipCode
#Note that French Zipcodes are between 1 and 98 (metropole= [01;95], DOM-TOM etc = [97;98], and a 99 in for foreign countries; 96 does not exist anymore, it was for "protectorat de Tunisie"

