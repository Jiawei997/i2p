# Data Biography

## Student Number: 20022771

---

### 1. Who collected the data?

Murray Cox and some of other co-founders of the website collect the data from Airbnb wed page. In fact, people with technical skills can scrape the data from Airbnb.

---

### 2. Why did they collect it?

They collect and provide data to allow people to explore the renting issue based on Airbnb platform. With data from Airbnb we can dive into the problem or the status quo of current rental market under the prevailing sharing economy, and to understand how Airbnb reshapes our society and economy.

---

### 3. How was it collected?

The data is collected in the form of scraping, since obviously the csv file shows the scrape id for each row. This method truly fit the big data collection, with massive amount and varieties of data [1]that need to be scraped in one go. However, the shortage of this method is that, it calls for accuracy and completeness, because during the scraping process, some data can be missing.

---

### 4. What useful information does it contain?


Latitude, longitude are vital spatial data, which allow us study on a spatial level. Meanwhile, they can be used to calculate the distance to city center or nearest public transport.

Type of property and other room information including amenities, accommodates, price, etc. are highly correlated to guests’ decision making.

Host details like photos, host about, response rate and time, verification and super-host, etc., which to some degree have effects on customer experience and trustworthiness[2].

Review frequency, number of reviews and review scores reflect on customer experience.


---

### 5. What useful information is it missing?

The dataset should include customer guests’ comments and the score for each listing, these will contribute to customer experience study.

Occupancy levels is missing from the dataset, since this is the way to determine how busy the site is through the volume of user reviews that guests leave behind.

---

### 6. To what extent is the data 'complete'?

The data is incomplete in the sense that the attributes are in single dimension, where we have various continuous and categorical variables, missing data that can be measured on a time line, so we cannot carry out analysis based on the sequence of time. Spatial data add another dimension for data analysis, which allow us explore the data in the spatial level. 

Besides, large proportion of null values in the variables make the data incomplete, therefore, completeness also lies in the validity of data.

The vacancy rate, the important data with respect to renting data supports findings on if the hosts are operating legally.The incompleteness of essential data like this is in effect challenging the representativeness of data and at the same time questioning the truth data are telling. 

Therefore, as for the completeness of data, it is crucial to have data with multi-dimensions, sufficient vaild values as well as meaningful data.


---

### 7. What kinds of analysis would this support?

Spatial analysis. Based on latitude, longitude data we can explore varieties of factors on spatial level.

Host analysis. Host profiles and response rate as well as response frequency can support the host analysis, where we can explore which variable is the most influential factor affecting users’ choice and what contributes to the trustworthiness in the peer-to-peer rental market. 

Guest experience. The review scores can let us quantitatively measure guest experience related to data accuracy, property cleanliness, location, service provision, etc. 

Property analysis. Data including coordinates, room type, price, amenities, beds, instant bookable, etc. allow us to evaluate the property with respect to location, cost effectiveness, quality, etc.

Popularity analysis. Reviews per month, number of reviews as well as last review show the volume and frequency of guests’ comment shared, it somehow reflects the popularity of individual listing.



---

###### 8. What kinds of analysis would it _not_ support?

Missing guest comments data. We cannot, somehow calculate the most frequent works (word cloud) to detect reasons for good or bad guest experience apart from the review system, since the comments contain more valuable information that the review system did not show.

Missing property quality data. The quality with reference to various aspects should appear in the form of scores, like how guests rate their experience in multiple aspects, guests, on the other hand should mark their properties too, this will lead to property consistency analysis.

Missing vacancy rate. Vacancy data allow us to look into how the popular the listing is, and to see if the host rent the property whole year round. 



---

### 9. Which of the uses presented in Q.7 and Q.8 are _ethical_?

Guest experience, popularity analysis and property analysis are ethical, in the sense that they don’t include personal information and all of the contents are public that are free for people to view. Spatial analysis on the other hand is ethical when don’t combine with hosts detail and other privacy messages, because privacy is defined when spatial data show up, the users’ location data are highly sensitive with reference to personal safety. Therefore, spatial data are ethical when combined with prices, property types, etc.

Analysis like host analysis, etc. are not ethical. Host profiles aiming to build trust with customers weight so much in the listing dataset. Studies show that the length of host profiles has positive correlation with trustworthiness[2]. However, it brings about problems like racial discrimination and privacy concern. Research shows black hosts occupy much less revenue from Airbnb compared with their white counterparts, suggesting a racial problem with respect to hosts’ photos[3]. In terms of privacy issue, though it is a necessity to post personal details on the platform, it calls for appropriate informed consent and tough measures against illegal usage of host profiles. However, in most cases, users are forced to consent the privacy conditions in order to get access the platform. I think this is neither a proper way to inform users about how their data will be manipulate nor an effective way of protecting their privacy against invasion. 

Bias exists with respect to what guests care. People in different culture think about different things, for instance people from China care much about the size and orientation of the property. However, Airbnb seems to be in a different ‘culture’, where hosts and guests care more about the type of the property rather than the space and orientation. So, I think in this case Airbnb is not as friendly as it claims to be. Culture differential also hosts in comment interpretation, with people from various social back ground trying to interpret others’ comments, which can cause misunderstand and interpretation biases.


 
Reference

[1]M. Cheng and X. Jin, “What do Airbnb users care about? An analysis of online review comments,” Int. J. Hosp. Manag., vol. 76, no. September 2017, pp. 58–70, 2019, doi: 10.1016/j.ijhm.2018.04.004.
[2]X. Ma, J. T. Hancock, K. L. Mingjie, and M. Naaman, “Self-disclosure and perceived trustworthiness of airbnb host profiles,” Proc. ACM Conf. Comput. Support. Coop. Work. CSCW, pp. 2397–2409, 2017, doi: 10.1145/2998181.2998269.
[3]E. Ert, A. Fleischer, and N. Magen, “Trust and reputation in the sharing economy: The role of personal photos in Airbnb,” Tour. Manag., vol. 55, pp. 62–73, 2016, doi: 10.1016/j.tourman.2016.01.013.

word count:1090(excluding reference)