USE DM;

SELECT 
    Sex, SUM(Earnings)
FROM
    extract_medium
GROUP BY Sex;

SELECT 
    Age, SUM(Earnings)
FROM
    extract_medium
GROUP BY Age;

SELECT 
    Marriage, SUM(Earnings)
FROM
    extract_medium
GROUP BY Marriage;

SELECT 
    Sex, Education, AVG(Earnings)
FROM
    extract_medium
GROUP BY Education
ORDER BY Earnings DESC;

SELECT 
    State, Education, Sex, Age, Earnings
FROM
    extract_medium
WHERE
    Earnings < ANY (SELECT 
            Earnings
        FROM
            extract_medium)
        AND Sex = 1;


SELECT 
    State, Education, Sex, Age, Earnings
FROM
    extract_medium
WHERE
    Earnings < ANY (SELECT 
            Earnings
        FROM
            extract_medium
        WHERE
            Education < 14)
        AND Sex = 1
ORDER BY Earnings DESC;



SELECT 
    AVG(Earnings)
FROM
    extract_medium
WHERE
    Sex = 1 AND Marriage = 1
LIMIT 0 , 25000;


SELECT 
    AVG(Earnings)
FROM
    extract_medium
WHERE
    Sex = 1 AND Marriage = 2
LIMIT 0 , 25000;

SELECT 
    AVG(Earnings)
FROM
    extract_medium
WHERE
    Sex = 1 AND Marriage = 3
LIMIT 0 , 25000;

SELECT 
    AVG(Earnings)
FROM
    extract_medium
WHERE
    Sex = 1 AND Marriage = 4
LIMIT 0 , 25000;

SELECT 
    AVG(Earnings)
FROM
    extract_medium
WHERE
    Sex = 1 AND Marriage = 5
LIMIT 0 , 25000;




