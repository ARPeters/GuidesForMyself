SQL Final - Andrew P

Data Scientist Role Play: Profiling and Analyzing the Yelp Dataset Coursera Worksheet

This is a 2-part assignment. In the first part, you are asked a series of questions that will help you profile and understand the data just like a data scientist would. For this first part of the assignment, you will be assessed both on the correctness of your findings, as well as the code you used to arrive at your answer. You will be graded on how easy your code is to read, so remember to use proper formatting and comments where necessary.

In the second part of the assignment, you are asked to come up with your own inferences and analysis of the data for a particular research question you want to answer. You will be required to prepare the dataset for the analysis you choose to do. As with the first part, you will be graded, in part, on how easy your code is to read, so use proper formatting and comments to illustrate and communicate your intent as required.

For both parts of this assignment, use this "worksheet." It provides all the questions you are being asked, and your job will be to transfer your answers and SQL coding where indicated into this worksheet so that your peers can review your work. You should be able to use any Text Editor (Windows Notepad, Apple TextEdit, Notepad ++, Sublime Text, etc.) to copy and paste your answers. If you are going to use Word or some other page layout application, just be careful to make sure your answers and code are lined appropriately.
In this case, you may want to save as a PDF to ensure your formatting remains intact for you reviewer.



Part 1: Yelp Dataset Profiling and Understanding

1. Profile the data by finding the total number of records for each of the tables below:
	
i. Attribute table = 10000
ii. Business table = 10000
iii. Category table =10000
iv. Checkin table = 10000
v. elite_years table = 10000
vi. friend table = 10000
vii. hours table = 10000
viii. photo table = 10000
ix. review table = 10000
x. tip table = 10000
xi. user table = 10000
	


2. Find the total distinct records by either the foreign key or primary key for each table. If two foreign keys are listed in the table, please specify which foreign key.

i. Business = 10000 distinct ids
ii. Hours = 1562 distinct business_ids 
iii. Category = 2643 distinct business_ids
iv. Attribute = 1115 distinct business_ids
v. Review = 10000 distinct ids
vi. Checkin = 493 distinct business_ids
vii. Photo = 10000 distinct ids 
viii. Tip = 3979 distinct business_ids
ix. User = 10000 distinct ids
x. Friend = 11 distinct user_ids
xi. Elite_years = 2780 distinct user_ids

Note: Primary Keys are denoted in the ER-Diagram with a yellow key icon.	



3. Are there any columns with null values in the Users table? Indicate "yes," or "no."

	Answer:
	
	
	SQL code used to arrive at answer:

SELECT 
  SUM(CASE WHEN id                   IS NULL THEN 1 ELSE 0 END) id_nulls
  , SUM(CASE WHEN name               IS NULL THEN 1 ELSE 0 END) name_nulls
  , SUM(CASE WHEN review_count       IS NULL THEN 1 ELSE 0 END) review_count_nulls
  , SUM(CASE WHEN yelping_since      IS NULL THEN 1 ELSE 0 END) yelping_since_nulls
  , SUM(CASE WHEN useful             IS NULL THEN 1 ELSE 0 END) useful_nulls
  , SUM(CASE WHEN funny              IS NULL THEN 1 ELSE 0 END) funny_nulls
  , SUM(CASE WHEN cool               IS NULL THEN 1 ELSE 0 END) cool_nulls
  , SUM(CASE WHEN fans               IS NULL THEN 1 ELSE 0 END) fans_nulls
  , SUM(CASE WHEN average_stars      IS NULL THEN 1 ELSE 0 END) average_stars_nulls
  , SUM(CASE WHEN compliment_hot     IS NULL THEN 1 ELSE 0 END) hot_nulls
  , SUM(CASE WHEN compliment_more    IS NULL THEN 1 ELSE 0 END) more_nulls
  , SUM(CASE WHEN compliment_profile IS NULL THEN 1 ELSE 0 END) profile_nulls
  , SUM(CASE WHEN compliment_cute    IS NULL THEN 1 ELSE 0 END) cute_nulls
  , SUM(CASE WHEN compliment_list    IS NULL THEN 1 ELSE 0 END) list_nulls
  , SUM(CASE WHEN compliment_note    IS NULL THEN 1 ELSE 0 END) note_nulls
  , SUM(CASE WHEN compliment_plain   IS NULL THEN 1 ELSE 0 END) plain_nulls
  , SUM(CASE WHEN compliment_cool    IS NULL THEN 1 ELSE 0 END) cool_nulls
  , SUM(CASE WHEN compliment_funny   IS NULL THEN 1 ELSE 0 END) funny_nulls
  , SUM(CASE WHEN compliment_writer  IS NULL THEN 1 ELSE 0 END) writer_nulls
  , SUM(CASE WHEN compliment_photos  IS NULL THEN 1 ELSE 0 END) photos_nulls
FROM
  user
;

	
4. For each table and column listed below, display the smallest (minimum), largest (maximum), and average (mean) value for the following fields:

	i. Table: Review, Column: Stars
	
		min: 1		max: 5		avg: 3.7082
		
	
	ii. Table: Business, Column: Stars
	
		min: 1		max: 5		avg: 3.6549
		
	
	iii. Table: Tip, Column: Likes
	
		min: 0		max: 2		avg: 0.0144
		
	
	iv. Table: Checkin, Column: Count
	
		min: 1		max: 53		avg: 1.9414
		
	
	v. Table: User, Column: Review_count
	
		min: 0		max: 2000		avg: 24.2995
		


5. List the cities with the most reviews in descending order:

	SQL code used to arrive at answer:
	
	SELECT
	b.city
	, COUNT(r.id) review_count

FROM
	business b JOIN review r ON b.id = r.business_id

GROUP BY
	b.city

ORDER BY
	review_count DESC
;
	
	Copy and Paste the Result Below:

+-----------------+--------------+
| city            | review_count |
+-----------------+--------------+
| Las Vegas       |          193 |
| Phoenix         |           65 |
| Toronto         |           51 |
| Scottsdale      |           37 |
| Henderson       |           30 |
| Tempe           |           28 |
| Pittsburgh      |           23 |
| Chandler        |           22 |
| Charlotte       |           21 |
| Montréal        |           18 |
| Madison         |           16 |
| Gilbert         |           13 |
| Mesa            |           13 |
| Cleveland       |           12 |
| North Las Vegas |            6 |
| Edinburgh       |            5 |
| Glendale        |            5 |
| Lakewood        |            5 |
| Cave Creek      |            4 |
| Champaign       |            4 |
| Markham         |            4 |
| North York      |            4 |
| Mississauga     |            3 |
| Surprise        |            3 |
| Avondale        |            2 |
+-----------------+--------------+
(Output limit exceeded, 25 of 67 total rows shown)

	
6. Find the distribution of star ratings to the business in the following cities:

i. Avon

SQL code used to arrive at answer:

SELECT
	DISTINCT(r.stars) star_rating
    , COUNT(b.name) count
FROM
	business b LEFT JOIN review r ON b.id = r.business_id

WHERE
	b.city = 'Avon' 
GROUP BY
	star_rating

ORDER BY
	star_rating
;


Copy and Paste the Resulting Table Below (2 columns – star rating and count):

+-------------+-------+
| star_rating | count |
+-------------+-------+
|        None |    10 |
+-------------+-------+

ii. Beachwood

SQL code used to arrive at answer:

SELECT
	DISTINCT(r.stars) star_rating
    , COUNT(b.name) count
FROM
	business b LEFT JOIN review r ON b.id = r.business_id

WHERE
	b.city = 'Beachwood' 
GROUP BY
	star_rating

ORDER BY
	star_rating
;

Copy and Paste the Resulting Table Below (2 columns – star rating and count):

+-------------+-------+
| star_rating | count |
+-------------+-------+
|        None |    13 |
|           3 |     1 |
+-------------+-------+		


7. Find the top 3 users based on their total number of reviews:
		
	SQL code used to arrive at answer:
	
SELECT
	u.name
	, COUNT(r.id) review_count

FROM
	user u JOIN review r ON u.id = r.user_id

GROUP BY
	u.id

ORDER BY
	review_count DESC

LIMIT 3
;		
	Copy and Paste the Result Below:
		
+-----------+--------------+
| name      | review_count |
+-----------+--------------+
| Kaitlan   |            2 |
| Christina |            2 |
| Craig     |            2 |
+-----------+--------------+

8. Does posing more reviews correlate with more fans?

	Please explain your findings and interpretation of the results:

	Based on the current data set, posting more reviews does NOT positively correlate with having more fans. First, , in the given data set most reviewers
have only written one review. There were only three exceptions, apparently. Moreover, when put into descending order of  number of fans, you have
to go down the list to 7th place to find the first individual who has written more than one review. The next invididual with more than one review
is not until you get to 24th place, and they clock in with 3 fans. 

	Moreover, if you run the same code but change JOIN to a LEFT JOIN, you get all the users who appear in the User table but not in the Review table,
presumably because they have written no reviews. If you include them in the table and order by the number of fans, then the top 21 users by number of
fans have written NO reviews. Or at least are not matched to any records in this subset of the Reviews table.


At best, you could make the argument that there arent enough invidiauls who have written more than one review to have a significant finding one way or the other.
What we have here may not be evidence of the absence of a correlation, but it definitely qualifies as an absence of evidence of a correlation. 

Code used: 
SELECT
	u.name
	, COUNT(r.id) review_count
	, u.fans

FROM
	user u JOIN review r ON u.id = r.user_id
    /*OR swap out the above code with 
	user u LEFT JOIN review r ON u.id = r.user_id
    */

GROUP BY
	u.id

ORDER BY
	u.fans DESC
;

Data Produced:

+------------+--------------+------+
| name       | review_count | fans |
+------------+--------------+------+
| Nieves     |            1 |   80 |
| Dottsy     |            1 |   49 |
| Dixie      |            1 |   41 |
| Ed         |            1 |   38 |
| Pinky      |            1 |   32 |
| Annie      |            1 |   29 |
| Christina  |            2 |   27 |
| Crissy     |            1 |   25 |
| Kyle       |            1 |   18 |
| AJA        |            1 |   16 |
| Jean       |            1 |   15 |
| Dave       |            1 |   14 |
| Mark       |            1 |   12 |
| Danny      |            1 |   11 |
| Andrea     |            1 |   10 |
| Christophe |            1 |   10 |
| Jade       |            1 |    6 |
| Char       |            1 |    6 |
| Alaina     |            1 |    6 |
| LA         |            1 |    5 |
| James      |            1 |    5 |
| W          |            1 |    4 |
| Isabelle   |            1 |    4 |
| Kaitlan    |            2 |    3 |
| Patricia   |            1 |    3 |
+------------+--------------+------+

	
9. Are there more reviews with the word "love" or with the word "hate" in them?

	Answer:

	
	SQL code used to arrive at answer:

	
	
10. Find the top 10 users with the most fans:

	SQL code used to arrive at answer:

SELECT
	name
	, fans

FROM
	user

ORDER BY
	fans DESC

LIMIT 10
;
	
	Copy and Paste the Result Below:
+-----------+------+
| name      | fans |
+-----------+------+
| Amy       |  503 |
| Mimi      |  497 |
| Harald    |  311 |
| Gerald    |  253 |
| Christine |  173 |
| Lisa      |  159 |
| Cat       |  133 |
| William   |  126 |
| Fran      |  124 |
| Lissa     |  120 |
+-----------+------+
	
	
11. Is there a strong relationship (or correlation) between having a high number of fans and being listed as "useful" or "funny?" Out of the top 10 users with the highest number of fans, what percent are also listed as “useful” or “funny”?

Key:
0% - 25% - Low relationship
26% - 75% - Medium relationship
76% - 100% - Strong relationship
	
	SQL code used to arrive at answer:

SELECT
	name
	, fans
	, useful
	, funny

FROM
	user

ORDER BY
	fans ASC

LIMIT 10
;
	
	Copy and Paste the Result Below:

+-----------+------+--------+--------+
| name      | fans | useful |  funny |
+-----------+------+--------+--------+
| Amy       |  503 |   3226 |   2554 |
| Mimi      |  497 |    257 |    138 |
| Harald    |  311 | 122921 | 122419 |
| Gerald    |  253 |  17524 |   2324 |
| Christine |  173 |   4834 |   6646 |
| Lisa      |  159 |     48 |     13 |
| Cat       |  133 |   1062 |    672 |
| William   |  126 |   9363 |   9361 |
| Fran      |  124 |   9851 |   7606 |
| Lissa     |  120 |    455 |    150 |
+-----------+------+--------+--------+
	
	Please explain your findings and interpretation of the results:
	
	By the key established in the question, this fits the definition of a 'strong' relationship, in that 100% percent of the top ten have many listings as
"useful" or "funny". However, it must be pointed out that having many listings of 'useful' or 'funny' has no obvious relationship to a users
rank within the top 10, or even within the top 25. It is likely that there is a nonlinear relationship between a users fan count and the number of times they
are listed as 'useful' or 'funny'. It is entirely possible that being useful and funny are necessities to get fans up to a certain point, but show diminishing
returns eventually. Pursuing this thought would require more analyses that can be done at present.
	

Part 2: Inferences and Analysis

1. Pick one city and category of your choice and group the businesses in that city or category by their overall star rating. Compare the businesses with 2-3 stars to the businesses with 4-5 stars and answer the following questions. Include your code.
	
i. Do the two groups you chose to analyze have a different distribution of hours?

	I have selected to focus on Restaurants in Las Vegas. The restaurant that is 
rated 3 stars or fewer is open from 11:00 to Midnight.  Oddly, it seems that the 
restaurants in Vegas that have the higher star ratings close earlier. The closest
a High Star restaurant comes to that time frame is closing at 11:00 PM. The other 
Highly rated restaurant closes at 8. The remaining restaurant does not have hours
listed. This is really much too small a sample size to mean much but for purposes
of this quiz, we do see a difference.

SELECT
	b.city
	, CASE WHEN b.stars <=3 THEN 1
		   WHEN b.stars >3 THEN 2 ELSE 0 END Star_Category
	, c.category
	, h.hours

FROM
	business b LEFT JOIN category c ON b.id = c.business_id
			   LEFT JOIN review r ON b.id = r.business_id
			   LEFT JOIN hours h ON b.id = h.business_id

WHERE
	b.city LIKE 'Las Vegas'
	AND c.category = 'Restaurants'
	
GROUP BY
  b.id

ORDER BY
  Star_Category
;+-----------+---------------+-------------+----------------------+
| city      | Star_Category | category    | hours                |
+-----------+---------------+-------------+----------------------+
| Las Vegas |             1 | Restaurants | Saturday|11:00-0:00  |
| Las Vegas |             2 | Restaurants | Saturday|10:00-23:00 |
| Las Vegas |             2 | Restaurants | Saturday|11:00-20:00 |
| Las Vegas |             2 | Restaurants | None                 |
+-----------+---------------+-------------+----------------------+
ii. Do the two groups you chose to analyze have a different number of reviews?

    The High-Star Vegas Restaurants do seem to have more reviews than those in the
Low-Star category. The restaurant with 3 or fewer stars all seem to have 123 reviews.
Two of those with 4 or more stars have  more reviews 168 to 768. The remaining 
restaurant only has 3; with a larger sample size, this would likely be an anomoly.

SELECT
	b.city
	, CASE WHEN b.stars <=3 THEN 1
		   WHEN b.stars >3 THEN 2 ELSE 0 END Star_Category
	, c.category
	, b.review_count

FROM
	business b LEFT JOIN category c ON b.id = c.business_id
			   LEFT JOIN review r ON b.id = r.business_id
			   LEFT JOIN hours h ON b.id = h.business_id

WHERE
	b.city LIKE 'Las Vegas'
	AND c.category = 'Restaurants'

GROUP BY
  b.id

ORDER BY
  Star_Category
;

+-----------+---------------+-------------+--------------+
| city      | Star_Category | category    | review_count |
+-----------+---------------+-------------+--------------+
| Las Vegas |             1 | Restaurants |          123 |
| Las Vegas |             2 | Restaurants |          768 |
| Las Vegas |             2 | Restaurants |          168 |
| Las Vegas |             2 | Restaurants |            3 |
+-----------+---------------+-------------+--------------+


iii. Are you able to infer anything from the location data provided between these two groups? Explain.

SQL code used for analysis:
SELECT
	b.city
	, CASE WHEN b.stars <=3 THEN 1
		   WHEN b.stars >3 THEN 2 ELSE 0 END Star_Category
	, c.category
	, b.postal_code
	, b.latitude
	, b.longitude

FROM
	business b LEFT JOIN category c ON b.id = c.business_id
			   LEFT JOIN review r ON b.id = r.business_id
			   LEFT JOIN hours h ON b.id = h.business_id

WHERE
	b.city LIKE 'Las Vegas'
	AND c.category = 'Restaurants'
	
GROUP BY
  b.id

ORDER BY
  Star_Category
;		
+-----------+---------------+-------------+-------------+----------+-----------+
| city      | Star_Category | category    | postal_code | latitude | longitude |
+-----------+---------------+-------------+-------------+----------+-----------+
| Las Vegas |             1 | Restaurants | 89103       |  36.1003 |   -115.21 |
| Las Vegas |             2 | Restaurants | 89146       |  36.1267 |   -115.21 |
| Las Vegas |             2 | Restaurants | 89134       |  36.1933 |  -115.304 |
| Las Vegas |             2 | Restaurants | 89169       |  36.1259 |  -115.135 |
+-----------+---------------+-------------+-------------+----------+-----------+

	To be honest, nothing about the location data leaps out at me. I have data on
four Vegas restaurants, all with different lattitudes and longitudes, and from
3 different postal codes. This is hardly unexpected since they are all different 
restaurants, and Las Vegas is a large city. 

2. Group business based on the ones that are open and the ones that are closed. What differences can you find between the ones that are still open and the ones that are closed? List at least two differences and the SQL code you used to arrive at your answer.
		
i. Difference 1:
	It seems that businesses that are currently open are more likely to have photos
associated with them and their reviews (10%) than business that are closed (5%).
         
         
ii. Difference 2:
	It seems that businesses that are currently open are more likely to have associated
records in the Review table (10%) than those that are closed (5%).
         
         
SQL code used for analysis:
SELECT
	b.is_open
	, COUNT(b.id)
	, COUNT(r.id) count_reviews
	, COUNT(p.id)

FROM
	business b LEFT JOIN review r ON b.id = r.business_id
			   LEFT JOIN category c ON b.id = c.business_id
			   LEFT JOIN photo p ON b.id = p.business_id

GROUP BY
	b.is_open
;

+---------+-------------+---------------+-------------+
| is_open | COUNT(b.id) | count_reviews | COUNT(p.id) |
+---------+-------------+---------------+-------------+
|       0 |        1635 |            73 |          82 |
|       1 |        9537 |          1013 |         935 |
+---------+-------------+---------------+-------------+
	
3. For this last part of your analysis, you are going to choose the type of analysis you want to conduct on the Yelp dataset and are going to prepare the data for analysis.

Ideas for analysis include: Parsing out keywords and business attributes for sentiment analysis, clustering businesses to find commonalities or anomalies between them, predicting the overall star rating for a business, predicting the number of fans a user will have, and so on. These are just a few examples to get you started, so feel free to be creative and come up with your own problem you want to solve. Provide answers, in-line, to all of the following:
	
i. Indicate the type of analysis you chose to do:

	I intend to look at the Star Ratings by day of the week, to look for an effect of 
weekday on the Star Category of reviews.
         
         
ii. Write 1-2 brief paragraphs on the type of data you will need for your analysis and why you chose that data:

	Im a biostatistician at a hospital, and a common issue with comparing the efficacy
of ER surgeons is comparing them when treating patiens with similar severities of
injuries. One of the counfounding issues with this is the night of the week surgeons
work on. Saturdays are more lethal than, say, Tuesdays. It is on Saturday nights that 
people are more likely to be out and about, to be drinking, to be engaging in riskier
behavior in general. So a surgeon who regular works a Saturday shift will have more
sever injuries to deal with and may appear less effective for an extraneous reason.

    This got me thinking about these restaurants and reviewers. Do reviewers write
more 'sever' reviews depending on the day of the week?  

	The trickiest part of this analysis will be converting the date column in the 
reviews table into a measure of the days of the week. This will involve converting 
the date of the review and todays date into a julian day, getting the difference,
casting that difference as a floating value, performing a modulo 7 on the difference,
and then grouping by the modulo value. An odd caveate of doing this this way is that,
 since it starts with a reference to todays date, the values taken by this grouping
 variable will change depending on the day this value is run. For example, as I 
 write this on a Saturday, any julian day value that is an even multiple of 7 will 
 have a modulo 7 value of 0 and will also be a Saturday. If I ran this again on
 Tuesday, than all modulo 7 values of 0 would correspond to Tuesdays. It is clunky
but as long as it is consistent it should work.

	After that, I intend to create dummy variables for each Star Rating. This way,
I can see how the frequency of each star rating changes across each day of the
week. And for the sake of completeness, I am also going to add a column showing
the total number of reviews from that day of the week.
                  
iii. Output of your finished dataset:

+-------------+----------+----------+------------+-----------+-----------+--------------+
| day_of_week | One_Star | Two_Star | Three_Star | Four_Star | Five_Star | review_count |
+-------------+----------+----------+------------+-----------+-----------+--------------+
|         0.0 |      210 |      138 |        129 |       315 |       594 |         1386 |
|         1.0 |      183 |       89 |        178 |       328 |       550 |         1328 |
|         2.0 |      174 |      106 |        178 |       340 |       556 |         1354 |
|         3.0 |      191 |      116 |        204 |       346 |       591 |         1448 |
|         4.0 |      196 |      133 |        168 |       333 |       603 |         1433 |
|         5.0 |      235 |      123 |        200 |       365 |       664 |         1587 |
|         6.0 |      203 |      132 |        162 |       374 |       593 |         1464 |
+-------------+----------+----------+------------+-----------+-----------+--------------+         
         
iv. Provide the SQL code you used to create your final dataset:

SELECT
 	(CAST (JULIANDAY('now') -  (JULIANDAY(date)) AS FLOAT) % 7) day_of_week
 	, SUM(CASE WHEN stars = 1 THEN 1 ELSE 0 END) One_Star
	, SUM(CASE WHEN stars = 2 THEN 1 ELSE 0 END) Two_Star
	, SUM(CASE WHEN stars = 3 THEN 1 ELSE 0 END) Three_Star
	, SUM(CASE WHEN stars = 4 THEN 1 ELSE 0 END) Four_Star
	, SUM(CASE WHEN stars = 5 THEN 1 ELSE 0 END) Five_Star
	, COUNT(id) review_count

FROM
 	review

GROUP BY 
 	day_of_week
;