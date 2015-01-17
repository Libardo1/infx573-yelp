###############################################################################
# Final Project 
# John Fuller, Lauren Hubener, Jonathan Lin, Amy Trost
# INFX537 Winter 2014
# Submitted December 12, 2014
###############################################################################

###############################################################################
#### Research Question: ####
# In this report, we consider the possible factors that account
# for the high number of reviews for a relatively small number 
# of businesses on Yelp, and why these businesses seem to generate 
# many more reviews in comparison with the majority of the 
# included businesses. Does the discrepancy in the high number 
# of reviews for a small fraction of businesses indicate that 
# these select businesses are of higher overall quality than their 
# Yelp competitors, and can high review counts be used as a 
# proxy for success? Furthermore, could a business’s success 
# be predicted based on certain attributes? These questions led 
# to the formation of our research question: 
# What are the attributes of frequently reviewed businesses on Yelp? 
# Can we predict if a business will be highly reviewed based 
# on these attributes?
###############################################################################

###############################################################################
#### Libraries ####
# Load these libraries
# Install these packages with install.packages()
###############################################################################

library(jsonlite)  # JSON to R conversions
library(plyr)
library(dplyr)  # Allows for data manipulation
library(ggvis)
library(RTextTools)  # we need these to run our API
library(httr)  # import urls with jsonlite
library(ggplot2)  # plotting and graphs
library(gdata)  # helps with data manipulation
library(moments)  # package for adding skewness
library(pROC)  # creating ROC curves
library(arm)   # data analysis using regression & multilevel models
library(randomForest)  # randomforest algorithm
library(rpart)  # For creating Classification and Regression Trees
library(rattle)  # Fancy tree plot
library(rpart.plot)  # Enhanced tree plots
library(RColorBrewer)  # Color selection for fancy tree plot
library(ROCR)  # Use to produce a ROC Curve

# Enforce dplyr select function
select <- dplyr::select

###############################################################################
#### 1.1. Data Importation ####
###############################################################################

#### Start by installing libraries for EDA ####
library(jsonlite)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggvis)

####  Import the Yelp Academic Dataset ####
# This script assumes that the Yelp Academic Dataset has already been
# downloaded and the zipped JSON files have been extracted to a 
# directory yelp_dataset_challenge_academic_dataset/
# the files we will use are the business and review datasets

# We need to add the flatten parameter to account for the nested fields
business_data <- fromJSON(paste("[",paste(readLines(
  "files/yelp_academic_dataset_business.json"),
  collapse=","),"]"), flatten = TRUE)

review_data <- fromJSON(paste("[",paste(readLines(
  "files/yelp_academic_dataset_review.json"),
  collapse=","),"]"), flatten = TRUE)

dim(business_data)  # dimensions of business table, 42153 businesses total
dim(review_data)    # 112.5 k reviews, but only 10 fields in each review.

#### initial inspection of review data ####
summary(review_data)      
View(head(review_data))   # 6 observations of 10 variables
max(review_data$date)     # most recent review July 16 2014
min(review_data$date)     # earliest review October 18 2004

#### initial inspection of business table ####
View(head(business_data))   # 6 observations of 105 variables

#### Data manipulation of business_data datasets ####
# Create a smaller data-frame of business info (easier to manipulate)
# contains a subset of the fields in the original business file
business_filtered <- business_data[,c('business_id', 'city',
                                'review_count','name',
                                'neighborhoods','longitude',
                                'state', 'stars', 'latitude',
                                'type')]
dim(business_filtered)   # still 42153 biz, only 10 fields now


###############################################################################
#### 1.2. Exploratory Data Analysis of review counts in business table ####
###############################################################################

# exploring how the number of reviews varies by business
# create a table/histogram of the number of businesses sorted by review number
bus_rev_num <- ddply(business_filtered, c("review_count"), 
                     summarise, number = length(review_count))
View(bus_rev_num)   # definitely a sharp drop as review count increases
# look at deciles of review counts
review_count_decile <- quantile(business_filtered$review_count,  
                                prob = seq(0, 1, length = 11), type = 5)
review_count_decile
# shows the following:
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  3    3    4    5    6    8   12   17   28   60 4084 

# view percentiles of review counts
review_count_percentile <- quantile(business_filtered$review_count,  
                                    prob = seq(0, 1, length = 101), type = 5)
review_count_percentile
# so if we just look at businesses with more than 100 reviews, that's a bit 
# more than 2000 businesses (a couple hundred thousand reviews, however)...

# create a histogram of review counts
business_filtered %>% ggvis(~review_count, fill := "#fff8dc") %>%
  layer_histograms(width = 20) %>%
  add_axis("x", title = "Number of reviews") %>%
  add_axis("y", title = "Number of businesses")


###############################################################################
#### 1.3. EDA of geography, calculating lat/long ranges for metro areas ####
###############################################################################

# Can we cluster businesses based on latitude and longitude?
# IE do a scatter-plot of lat/long data?
plot(business_filtered$longitude, business_filtered$latitude)
# OK so it looks like there's one place in Scotland (Edinburgh)
# and the Waterloo data are from Ontario!

# City distribution businesses using ddply
bus_city_distribution <- ddply(business_filtered, c("city"), 
                               summarise, number = length(city))

city_dist_sorted <- arrange(bus_city_distribution, desc(number))
# So Vegas has the most businesses, there are a lot of smaller neighborhoods
# as well, though, that are part of different metro areas. It may
# be good to aggregate businesses by metro area using lat/long specifications
View(city_dist_sorted)  

# figuring out range of latitudes and longitudes for 
# Madison, Phoenix, and Las Vegas
# this will provide guidance on the census blocks to retrieve
cities <- c("Phoenix","Las Vegas","Madison")  # narrowing scope to US business
min_lat_est <- c(32,35,42)  # starting with estimated lat/long for each area
max_lat_est <-c(35,37,43)  # max latitude estimates for 3 cities
min_long_est <-c(-114,-117,-90) # min longitude estimates for 3 cities
max_long_est <-c(-109,-114,-88) # max longitude estimates

# pulling estimates into a data frame
city_latlong = data.frame(
  cities, min_lat_est, max_lat_est, min_long_est, max_long_est
  )
View(city_latlong)

# in this loop we run through each city: Phoenix, Vegas, Madison
for(i in 1:3){
  # applying a rough filter to isolate businesses
  # from this city
  biz_filtered <- filter(business_filtered, 
                       latitude > city_latlong[i,2] 
                       & latitude < city_latlong[i,3]
                       & longitude> city_latlong[i,4]
                       & longitude< city_latlong[i,5])
  # getting maximum & minimum lat long for this subset of businesses
  city_latlong$maxlat[i] <- max(biz_filtered$latitude)
  city_latlong$maxlong[i] <- max(biz_filtered$longitude)
  city_latlong$minlat[i] <- min(biz_filtered$latitude)
  city_latlong$minlong[i] <- min(biz_filtered$longitude)}

###############################################################################
#### 1.4. Category parsing for regular businesses & 100+ businesses ####
###############################################################################

# Exploring how categories work with a test vector
test_vector <- c(
    "Doctors, Health & Medical",
    "Restaurants",
    "American (Traditional), Restaurants",
    "Home Services, Heating & Air Conditioning/HVAC",
    "Hotels & Travel, Bed & Breakfast, Event Planning & Services, Hotels"
    )
# we can use strsplit on my test case
split_categories <- strsplit(test_vector[1], ", ")  
View(split_categories)
split_categories[[1]][2]  # figuring out syntax for viewing items in array
length(split_categories[[1]])
# in the end, this exploration doesn't help, because our business categories
# are nested data frames, not sub arrays

# trim business table to include only US businesses
# and fewer fields for category parsing
colnames(business_data)  # viewing colnames to choose correct ones
# filtering by longitude to include US businesses
business_trim1 <- filter(business_data,longitude<(-89))
business_trim2 <- business_trim1[,c(
    'business_id',
    'open',
    'categories',
    'review_count',
    'longitude',
    'latitude',
    'stars',
    'attributes.Price Range',
    'attributes.Ambience.romantic',
    'attributes.Ambience.intimate',
    'attributes.Ambience.touristy',
    'attributes.Ambience.hipster',
    'attributes.Ambience.divey',
    'attributes.Ambience.classy',
    'attributes.Ambience.trendy',
    'attributes.Ambience.upscale',
    'attributes.Ambience.casual'
    )]
View(business_trim2)
dim(business_trim2) #38889 businesses

# get the maximum number of business categories
max_num_categories <-0  # set initial value to 0
max_row_num <- 0  # trying to find the row where all of these categories occur

for(i in 1:38889)  # loop through all businesses
  { # counting categories in each business  
  numcat <- length(business_trim2$categories[[i]])
  # if category count bigger than what's been 
  # seen already, make it the max
  if(numcat > max_num_categories) {
    max_num_categories <- numcat 
    max_row_num <- i
  }
  }

max_num_categories # max number of categories is 10
# viewing 6th category in our business with 10 cats
business_trim2$categories[[max_row_num]][6] 

# loop through to get the categories
total_catlist = NULL

for(i in 1:38889) #loop through each business
{
  # figure out how many categories each biz has
  numcat <- length(business_trim2$categories[[i]]) 
  # for each category, add to a big list that has
  # some categories lots of times, some just a few
  # (can parse this later to get frequency of categories)
  for(j in 1:numcat){
      catname <- business_trim2$categories[[i]][j]
      total_catlist <- c(total_catlist, catname)
      }
  }

View(total_catlist)  # 110,928 total category designations

# Creating a data frame of all of the categories listed in a line
total_catlist_df <- data.frame(total_catlist)

# now organize by the number of times each category
# occurs on the business table
cat_count <- ddply(total_catlist_df, c("total_catlist"), 
                     summarise, number = length(total_catlist))
cat_count_sorted <- arrange(cat_count, desc(number))
View(cat_count_sorted)  
# 703 total categories, you can see that some are used often:
# total_catlist number
# 1      Restaurants  13011
# 2         Shopping   5734
# 3             Food   4606
# 4    Beauty & Spas   3335
# 5        Nightlife   2337
# 6 Health & Medical   2324

# Now examine category distribution for businesses that get > 100 reviews
# filter business table to include only >100 reviews businesses
business_trim3 <- filter(business_trim2, review_count>99) 
dim(business_trim3)  # 2417 businesses here

# loop through to get 100+ category designations
total_catlist_100plus = NULL

for(i in 1:2417)  #loop through 100 plus businesses
{
  # find number of categories for each business
  numcat <- length(business_trim3$categories[[i]])
  # loop through each category to put it in a long list
  for(j in 1:numcat){
    catname <- business_trim3$categories[[i]][j]
    total_catlist_100plus <- c(total_catlist_100plus, catname)
  }
}

View(total_catlist_100plus) # 7749 total cat listings
# turn into a data frame
total_catlist_100plus_df <- data.frame(total_catlist_100plus)

# now organize by the number of times each category 
# occurs on the business table
cat_count_100plus <- ddply(
    total_catlist_100plus_df, c("total_catlist_100plus"), 
    summarise, number_100plus = length(total_catlist_100plus))
# sort in descending order
cat_count_sorted_100plus <- arrange(cat_count_100plus, desc(number_100plus))
# make it a data frame
cat_count_sorted_100plus <- data.frame(cat_count_sorted_100plus)
# create different column headings
names(cat_count_sorted_100plus) <- sub("total_catlist_100plus",
                                       "total_catlist",
                                       names(cat_count_sorted_100plus))
View(cat_count_sorted_100plus) # 269 categories in the 100+ businesses 

# how many category designations for all businesses?
total_cats_all <- sum(cat_count_sorted$number)
total_cats_all   # 110928 total category listings

# how many category designations for all businesses with more than 100 reviews
total_cats_100plus <- sum(cat_count_sorted_100plus$number_100plus)
total_cats_100plus  # 7749 total for 100+

# Merge category counts
cat_counts_merged <- merge(cat_count_sorted, cat_count_sorted_100plus,
    by=c("total_catlist"))
cat_counts_merged_sorted <- arrange(cat_counts_merged,desc(number_100plus))
View(cat_counts_merged_sorted)


###############################################################################
#### 1.5. Export CSV from EDA ####
###############################################################################
#### creating a file with counts merged
write.csv(cat_counts_merged_sorted, file = "files/Yelp_Cat_Counts.csv")

#### create a file with basic business data for census script ####
business_trim3 <- business_trim1[,c('business_id', 
                                    'longitude','latitude','stars')]
write.csv(business_trim3,"files/business_trim3.csv")


###############################################################################
#### 2. Importing and cleaning census data ####
# a. Run an API to retrieve census block data
# b. Merge these results with population and land
#    Area data retrieved from the census.gov website.
###############################################################################


# Adding libraries
library(dplyr)   # for data manipulation
library(RTextTools) # we need these to run our API


# read in business_trim3 file for business ID's, lat/long
# file was created in the "Nov 19 yelp revision" script
business_trim3 <- read.csv("files/business_trim3.csv")
dim(business_trim3) # 38889 rows, 5 columns

###############################################################################
#### 2.1. API to retrieve census block data ####
###############################################################################

# first running a sample API to see if it works
# pulling text of API call into a string
request <- 
  "http://data.fcc.gov/api/block/find?format=json&latitude=36.1215&longitude=-115.1739"
# calling API using "fromJSON"
res1 <- fromJSON(request)
# Viewing results; FIPS is the field that we need for our bulk set of queries
View(res1)

# run bulk API
blocknums_by_business = NULL # initialize variable to store blocknumbers
# call API in bulk for 38889 businesses
for(i in 1:38889){
  # pulling text of call into a string
  request <- paste(
    "http://data.fcc.gov/api/block/find?format=json&latitude=",
    business_trim2$latitude[i],"&longitude=",business_trim2$longitude[i]
    )
  # removing spaces from request
  request <- gsub(" ", "", request) 
  # running request
  res1 <- fromJSON(request)
  # adding results into an array
  blocknums_by_business[i] <- res1$Block$FIPS
}

View(blocknums_by_business) # list of census block numbers
# write these into a csv so we don't lose them
write.csv(blocknums_by_business, file = "files/blocknums_by_business.csv")

# adding block numbers to business_trim3 table
biz_blocks <- transform(business_trim3, 
                       block_num = blocknums_by_business)

# writing this to a csv file
write.csv(biz_blocks,"files/business_by_blocknums.csv")

###############################################################################
#### 2.2. Merging business ids with census data ####
###############################################################################

# read in pertinent files download via queries at census.gov
# land area in square meters for all 3 metro areas
geo_area <- read.csv("files/land_area_block_group.csv")
# population for all 3 metro areas
pop_data <- read.csv("files/PHOE_MAD_VEGAS_total_pop_by_blocks.csv")

# cleaning census block data so files will merge properly
# 1. clean the business table
# 1.a. Disabling scientific notation
biz_blocks2 <- transform(biz_blocks, 
               bn_nosci=format(biz_blocks$block_num, scientific=FALSE))

# 1.b. turn block numbers into a string, reduce to 12 digits
bn_string = NULL
for(i in 1:38889)   # looping through block numbers
  { each_string <- toString(biz_blocks2$bn_nosci[i]) # creating string
    each_string <- gsub(" ","0", each_string) # substituting 0s
    each_string <- substr(each_string, 1, 12) # making 12 digit
    # appending array
    bn_string[i] <- each_string 
  }

# make new data frame with revised block numbers
biz_blocks3 <- transform(biz_blocks2, bn_string=bn_string)

# 2. Format land area file
dim(geo_area)  # 4086  rows here
names(geo_area) # Viewing column headers 

# initializing variable to put in revised block numbers
geoblock_string = NULL
for(i in 1:4372)   # looping through block numbers
{ 
  # 2a. disable scientific notation
  each_string <- format(geo_area$FIPS[i], scientific=FALSE)
  each_string <- toString(each_string) # 2b. creating string from blocknum
  # adding 0 to some strings (4 should have been "04" originally)
  if(substring(each_string, 1, 1) == 4) 
    {each_string <- paste("0",each_string)
     # removing spaces
     each_string <- gsub(" ","", each_string)}
  # appending array
  geoblock_string[i] <- each_string 
}
# add newly revised FIPS code to geo_area df
geo_area <- transform(geo_area, FIPS2=geoblock_string)

# 3. Population file, formatting data 
dim(pop_data) # 4086 rows here
names(pop_data) # viewing names
popblock_string = NULL # initializing variable to put in revised blocknums

for(i in 1:4086)   # looping through block numbers
{ 
  # disable scientific notation
  each_string <- format(pop_data$Block_Group[i], scientific=FALSE)
  each_string <- toString(each_string) # creating string
  # adding 0 to some strings (4 should have been "04" originally)
  if(substring(each_string, 1, 1) == 4) 
  {each_string <- paste("0",each_string)
   # removing spaces
   each_string <- gsub(" ","", each_string)}
  # appending array
  popblock_string[i] <- each_string 
}
# adding a new column with revised FIPS code to pop file
pop_data <- transform(pop_data, FIPS2=popblock_string)

# cleaning up columns in all tables before join

names(pop_data) # looking at column names again
# only 2 columns for pop data table
pop_data <- subset(pop_data, select = c("FIPS2","Population"))

names(geo_area) # look at column names
# only 2 columns for geo area table
geo_area <- subset(geo_area, select = c("FIPS2","land_area"))

# clean up business table as well
names(biz_blocks3)
# only 3 columns here
business_census <- subset(biz_blocks3, 
      select = c("business_id","stars", "bn_string"))
# changing name of business_census heading so it matches the others
names(business_census)[3] <- "FIPS2"

# merge business data with geo area data
business_census2 <- merge(x=business_census,y=geo_area,
                          by="FIPS2", all.x=TRUE)
# merge business/geo data with population data
business_census3 <- merge(x=business_census2,y=pop_data,
                          by="FIPS2", all.x=TRUE)
# yay! it's all combined
View(business_census3)
summary(business_census3$Population)
# write it to a csv
write.csv(business_census3, file="files/census_business_data.csv")

# finding blocks of census data that are not 
# represented yet in population
na_values <- read.csv("NAs.csv")
na_values <- unique(na_values)
View(na_values)
# (eventually the data above were used to revise census csv files and 
# run this merge program again, we had to add more counties to our
# query on americanfactfinder.gov)

###############################################################################
#### 2.3. Make a small us_businesses.csv
# The following code was used to create a dataframe that contained
# only U.S. businesses with the following variables we wanted to 
# intially explore:
# From the business_data dataset:
# business_id, review_count, longitude, state, latitude
#
# From the review_data dataset:
# business_id, stars, date
#
# As our project progressed, we created new variables to use as 
# predictors, but this was an initial or early dataframe used.
###############################################################################


# Taking selected variables from the business_data dataset to be 
# joined in a new table with selected review data.
b1 <- business_data %>%
  select(business_id, review_count, longitude, state, latitude)


# Taking selected variables from the review_data dataset to be 
# joined in a new table with selected business data.
b2 <- review_data %>%
  select(business_id, stars, date)

# Using join function from R plyr package to merge variables from
# business and review datasets into a new dataframe.
joinb1b2 <- join(b1, b2, by="business_id")

# Saving the new dataframe as 'usBiz'
usBiz <- joinb1b2 %>% filter(longitude < -89) # Filtering out businesses
# less than the longitude of -89 returned only the U.S. businesses included
# in the dataset. In our project, we were only focusing on U.S. businesses.

# Viewing the new dataframe.
View(usBiz)

# Arranging the new dataframe by business id.
a <- usBiz %>%
  arrange(business_id)

# export to csv
write.csv(usBiz, file = "files/usBusinesses.csv")


###############################################################################
#### 3. Playing with dates ####
# cleaning some date data that was previously merged with businesses
# to calculate earliest date reviewed plus some skewness
###############################################################################

# adding libraries
library(jsonlite)  # JSON to R conversions
library(dplyr)  # data manipulation
library(ggplot2)  # plotting and graphs
library(gdata)  # helps with data manipulation
library(moments)  # package for adding skewness

# Read in John F's merged files
business_review_combo <- read.csv("files/usBusinesses.csv",stringsAsFactors=FALSE)
dim(biz_review_combo) # huge file, 1101793  X 8
num_rows <- dim(biz_review_combo)[1]

# adding a date formatted object
names(biz_review_combo) # retrieving names from df
datez <- as.Date(biz_review_combo$date) # formatting as date
biz_review_combo[,9] <- datez # adding a new column
names(biz_review_combo)[9] <- "new_date" # titling the row
names(biz_review_combo) # looks good!
summary(biz_review_combo$new_date) # from 2004 to 2014


# Interesting to see distribution of reviews based on review count
# so when *businesses* are unit of measure, most get few reviews
# when *reviews* are considered, however, most come from businesses
# that get a lot of reviews
names(business_review_combo)
summary(business_review_combo$review_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0    34.0   111.0   325.5   321.0  4084.0 


# Can we sort businesses by earliest review?
bus_earliest_review <- ddply(biz_review_combo, c("business_id"), 
                               summarise, earliest_review = min(new_date))
write.csv(bus_earliest_review,"files/earliest_review_date.csv") # write to csv

# examine skewness as a measure of review distribution
# alternately, write a function that breaks down reviews over time
review_time_skew <- ddply(biz_review_combo, c("business_id"), 
                             summarise, review_timing = skewness(new_date))
# writing this to a separate csv with skewness
write.csv(review_time_skew,"files/review_time_skew.csv") # write to csv
# examining skewness distribution
summary(review_time_skew$review_timing) 

###############################################################################
#### 3.1. Generating rate of reviews ####
# cleaning some date data that was previously merged with businesses
# to calculate earliest date reviewed plus some skewness
# Using the original business_data table and the usBusinesses table,
# calculate the duration of time each business has been open, and 
# create a new column totaling the rate of reviews (per day). 
# Arrange using the rate variable in order to identify the most
# reviewed businesses. Depending on how the date is limited, 
# this allows us to identify up and coming businesses (such as
# Guy Fieri and Giada in Las Vegas, Taco y Taco in Henderson, etc.) as well.
###############################################################################

# reinport data
usBusinesses <- read.csv("files/usBusinesses.csv")

# convert date to date formate
usBusinesses$date <- as.Date(usBusinesses$date) 

# calculate the earliest few for each bisniess
bus_earliest_review <- ddply(usBusinesses, c("business_id"), 
                             summarise, earliest_review = min(date))

# filter businesses that are still open in order to calculate accurate rate
b1 <- business_data %>%
  select(business_id, open, name, review_count, city, state) %>%
  filter(open=="TRUE")

# filter businesses that did not have reviews before 2014
bus_earl_review <- bus_earliest_review %>%
  filter(earliest_review < "2014-01-01")


# include date of earliest review
b2 <- new_busi %>%
  select(business_id, earliest_review)

joinb1b2 <- join(b1, b2, by="business_id")


# convert to date format
date <- as.Date(joinb1b2$earliest_review)
joinb1b2$first_date <- date

# set current date as July 16, 2014 (max review date in dataset)
joinb1b2$curr_date <- as.Date("2014/07/16")

# calculate number of days open
days_open <- joinb1b2 %>%
  select(business_id:curr_date) %>%
  mutate(open = joinb1b2$curr_date - joinb1b2$first_date)

# convert to numeric
open2 <- as.numeric(days_open$open)
days_open$num_day <- open2

# calculate review count per day
rate <- days_open$review_count / days_open$num_day
days_open$rate <- rate

# top reviewed businesses controlling for time
new_top_reviewed <- days_open %>%
  select(business_id:rate) %>%
  arrange(desc(rate))

# export to csv
write.csv(top_reviewed, file = "files/rate_of_reviews.csv")

# filter businesses that did not have reviews before 2014
new_busi <- bus_earliest_review %>%
  filter(earliest_review > "2014-01-01")


###############################################################################
#### 4. Visualizing Category Distributions ####
# In this script we explore how category distributions vary between 
# regular businesses and businesses receiving > 100 reviews
###############################################################################

# read in file with category counts
# these files were generated in the "November 19 yelp revision"
cat_counts_merged_sorted <- read.csv("files/Yelp_Cat_Counts.csv")
View(cat_counts_merged_sorted, "category counts")

# Sum of category counts across all categories,
# for all restaurants, and for our study group
total_cats_all <- sum(cat_counts_merged_sorted$number)
total_cats_100plus <- sum(cat_counts_merged_sorted$number_100plus)
# now there are only 90,967 total categories b/c we lost
# the categories that weren't represented in 100+ restaurants in our merge
total_cats_all    
total_cats_100plus  # still 7749 total here

# adding columns to our category count data frame,
# showing category designation as a proportion of total
cat_counts_all <- transform(cat_counts_merged_sorted, 
                            prop = number/total_cats_all)
cat_counts_all <- transform(cat_counts_all, prop_100plus = 
                              number_100plus/total_cats_100plus)

# now we can look at how each category is representative of the total
View(cat_counts_all, "cat counts with prop")

# getting rid of "restaurants" category to make visualizations easier to read
cat_counts_norestaurants <- filter(cat_counts_all, number<10000)

# plotting data with labels to compare proportions
cat_counts_norestaurants %>% 
  ggvis(~number, ~number_100plus, fill := "blue", opacity := 0.6) %>% 
  layer_text(text:=~total_catlist) %>%
  layer_points() %>%
  add_axis("x", title="Number of all businesses in Category") %>%
  add_axis("y",  title="Number of highly reviewed businesses in Category") %>%
  layer_model_predictions(model = "lm")

#### correlations between businesses and number of categories in each type ####
# figuring out relationship between number of reviews and number 
# of 100 plus reviews by category
cat_counts.corr <- cat_counts_norestaurants %>% 
  lm(formula=number_100plus~number)
summary(cat_counts.corr)

# adding residuals to data frame so that we can plot them
cat_counts_norestaurants <- 
  transform(cat_counts_norestaurants, residual=(resid(cat_counts.corr)))
View(cat_counts_norestaurants)

# Which categories are more likely to match highly reviewed restaurants?
# look at residual plots to see.
cat_counts_norestaurants %>%
  ggvis(~number, ~residual, fill := "blue", opacity := 0.4) %>% 
  layer_text(text:=~total_catlist) %>%  # add labels
  layer_points() %>%
  add_axis("x", title="Number of all businesses with Category") %>%
  add_axis("y",  title="residual")

# Now lets try to view residuals based on less common categories
cat_counts_lowerbound <- filter(cat_counts_norestaurants, number<2000)

# plot a subset of data
cat_counts_lowerbound %>%
  ggvis(~number, ~residual, fill := "blue", opacity := 0.4) %>% 
  layer_text(text:=~total_catlist) %>%
  layer_points() %>%
  add_axis("x", title="Number of all businesses with Category") %>%
  add_axis("y",  title="residual percent")

###############################################################################
#### 5. Creating Category Matrices ####
# In this script we create a large file that shows, 
# for each Yelp category and each business,
# a value of "0" or "1" to show which categories
# are included in each business
###############################################################################

# Installing libraries
library(jsonlite)  # allows for json file conversion
library(dplyr) # allows for data manipulation
library(ggplot2) # graphing library

# 42153 businesses listed
dim(business_data)

business_trim1 <- filter(
    business_data,longitude<(-89)) # include only US businesses
# filter so that categories and review count are included, not much else
business_trim2 <- business_trim1[,c(
    'business_id', 'open','categories','review_count','stars'
    )]
View(business_trim2)

# get the maximum number of business categories
max_num_categories <-0   # set initial value to 0
max_row_num <- 0 # trying to find the row where all of these categories occur

for(i in 1:38889) # loop through all businesses
{ # counting categories in each business  
  numcat <- length(business_trim2$categories[[i]])
  # if category count bigger than what's been 
  # seen already, make it the max
  if(numcat > max_num_categories) {
    max_num_categories <- numcat 
    max_row_num <- i
  }
}

max_num_categories # max number of categories is 10
# viewing 6th category in our business with 10 cats
business_trim2$categories[[max_row_num]][6] 

# read in cat count csv to get category names
# (this file was created in the Nov 19 Yelp revision.r)
cat_csv <- read.csv("files/Yelp_Cat_Counts.csv")
catlist <- cat_csv$total_catlist
length(catlist) # 269 total categories here

# create an array to hold business categories
biz_catYN <- data.frame(matrix(ncol = 269, nrow = 38889))
dim(biz_catYN) # checking array

# loop through each business
for(i in 1:38889) {
  # figure out how many categories are in each business
  numcat = length(business_trim2$categories[[i]])
  for(j in 1:268){  #loop through each category in big list
    if (numcat > 0){ # if there is more than one category listed for the biz
    for(k in 1:numcat) { # loop through each category in each biz
      # if there's a match, make it a yes!
      if(business_trim2$categories[[i]][k]==catlist[j]) biz_catYN[i,j] <- 1
    }
    }
  }
}

# adding category names, business numbers to catlist
bizcats <- cbind(business_trim2$business_id,business_trim2$review_count, 
                 biz_catYN)
colnames(biz_cats) <- catlist

# writing bizcats to csv file
write.csv(bizcats,"files/bizcats.csv")

###############################################################################
#### 6. Creating Predictor Files ####
# In this script we create a file that includes most of the predictors 
# we will need to run statistical analysis on our yelp data set
# using stuff from original yelp business data set, csv's from 
# "playing with dates" script and "import clean census data" script
# the category predictors created here as well as
# review frequency gets added later
###############################################################################

# adding libraries
library(jsonlite) # JSON to r conversions
library(dplyr) # data manipulation
library(ggplot2) # plotting and graphics
library(gdata)  # helps with data manipulation
library(moments) # package for adding skewness

# filter business file
# Only include fields that we care about
business_trimmed <- business_data[,c(
    'business_id',
    'open',
    'review_count',
    'longitude',
    'latitude',
    'state',
    'stars',
    'attributes.Price Range',
    'attributes.Ambience.romantic',
    'attributes.Ambience.intimate',
    'attributes.Ambience.touristy',
    'attributes.Ambience.hipster',
    'attributes.Ambience.divey',
    'attributes.Ambience.classy',
    'attributes.Ambience.trendy',
    'attributes.Ambience.upscale',
    'attributes.Ambience.casual'
    )]

# Fixing space on price range
names(business_trimmed)[7] <- "attributes.PriceRange"

# Fixing space on price range
names(business_trimmed)[7] <- "attributes.PriceRange"

#### filtering to US, add metro area designation ####
# Next, only include US businesses, and add a column that places 
# business in one of the 3 metro areas
# city      maxlat       maxlong      minlat           minlong
# Phoenix   34.048668    -111.25919   32.87663833      -112.94023
# Las Vegas 36.33588639  -114.85108   35.950985        -115.369725
# Madison   42.9985118   -89.199643   42.953165        -89.545987

# here's where we limit to US businesses
business_trimmed <- filter(business_trimmed, longitude< -89)
dim(business_trimmed)   # now we have 38889 businesses

# here's where we limit to US businesses
business_trimmed <- filter(business_trimmed, longitude< -89)
dim(business_trimmed)   # now we have 38889 businesses

# here's a function that reads in latitude and assigns metro area
# filter using lat/long designations from above (created in yelp
# Nov 19 revision)
metro.area <- function(latitude) { 
    area_name <- NA
    # Function accepts dates, returns distribution
    if (latitude>35 & latitude<36.5)   area_name ="VEGAS"
    if (latitude>32.5 & latitude<34.1) area_name ="PHOENIX"
    if (latitude>42)                   area_name ="MADISON"
    return(area_name) # Output of function
}

# create a column for metro areas
rownums <- dim(business_trimmed[1])
towns = NULL
# reading in an array of towns b/c for some
# reason function won't accept an array of 
# latitudes
# calling function for each individual business based on latitude
for (i in 1:rownums) {
    towns[i] <- metro.area(business_trimmed$latitude[i])}
# adding town data to business file
business_trimmed[,17] <- towns
# renaming as metroArea
names(business_trimmed)[17] <- "metroArea"

#### Add census data/population density to the filtered file ####
census_data <- read.csv("files/census_merged_data.csv", stringsAsFactors=FALSE)
View(census_data)
names(census_data)

# Adding a column to represent population per square mile
# convert land area from square meters
census_data[,7] <- census_data$Population/(census_data$land_area/(2.59e6))
names(census_data)[7] <- "popPerSqMi"
# view summary, make sure numbers are reasonable
summary(census_data$popPerSqMi)

# dropping irrelevant columns, only include pop density
census_trim <- census_data[,c('business_id', "popPerSqMi")]
# Need to remove duplicate business ids from census column
dim(census_trim)
census_trim2  <- ddply(census_trim, c("business_id","popPerSqMi"), 
                   summarise, ID = first(business_id))
View(census_trim2)
census_trim <- census_trim2[,c('business_id', "popPerSqMi")]

# merging census data to existing table
biz_merge <- merge(x=business_trimmed, y=census_trim, 
                   by="business_id", all.x=TRUE, all.y=FALSE)
dim(biz_merge)
View(census_trim)
summary(biz_merge$popPerSqMi) # missing about 1% of population densities

#### Filter business data by review date ####
# only include businesses that had the chance
# to collect 6 months of data
# the most recent review in the database was July 16, 2014
max(review_data$date)

# reading in earliest review date from 
review_first <- read.csv("files/earliest_review_date.csv",
    stringsAsFactors=FALSE)
View(review_first)
dim(review_first)

# formatting review dates to be dates
datez <- as.Date(review_first$earliest_review)
review_first[,4] <- datez
names(review_first)[4] <- "MostRecentReview"
summary(review_first$date)

# cleaning up table
review_first <- subset(review_first, select=-X)
review_first <- subset(review_first, select=-earliest_review)
review_first

# add this to business table
biz_merge <- merge(x=business_trimmed, y=review_first, 
                   by="business_id", all.x=TRUE)
summary(biz_merge$MostRecentReview)

# filter any businesses that didn't start
# reviews until 2014
biz_merge <- filter(biz_merge, MostRecentReview < "2014-01-01")
dim(biz_merge)    # now we have 37334 businesses

#### Add skewness column####
skewness_data <- read.csv("files/review_time_skew.csv", stringsAsFactors=FALSE)
dim(skewness_data)
names(skewness_data)

# dropping irrelevant columns
skewness_data <- skewness_data[,c('business_id', "review_timing")]

# merging data into a new variable
biz_merge2 <- merge(x=biz_merge, y=skewness_data, 
                    by="business_id", all.x=TRUE, all.y=FALSE)

# Output a new csv with predictors and rate of review
write.csv(biz_merge2,"files/predictors_with_rate_of_reviews.csv")

####creating category predictor file####
#### First set of predictors: Categories ####
bizcats <- read.csv("files/bizcats.csv", stringsAsFactors=FALSE)
# making first column the row names
rownames(bizcats) <- bizcats$X
# removing the first column
bizcats <- subset(bizcats, select=-X)
View(bizcats)
dim(bizcats)  # 268 columns, 38889 rows

# let's only include categories that occur in more
# than 100 businesses 

# exploring the sums of the categories to make sure
# the code below this works
biz_sums <- colSums(bizcats[2:270])
summary(biz_sums)

# filtering bizcats
keeps <- which(colSums(bizcats)>100)
bizcats2 <- subset(bizcats[,keeps]) 
rownames(bizcats2)

# making the business_id a row again
bizcats3 <- cbind(business_id = rownames(bizcats2),bizcats2)
View(bizcats3)

# create a table with business_id and number of reviews
# to merge with this one
business_id_nums <- biz_merge4[,c('business_id', 'review_count')]
dim(business_id_nums)

bizcats_IDs <- merge(x=business_id_nums, y=bizcats3, 
                     by="business_id", all.x=TRUE)
View(bizcats_IDs)


# Writing category file to csv
write.csv(bizcats_IDs,"files/categoryPredictors.csv")


###############################################################################
#### 7. Yelp data simple correlations and visualizations ####
# Once our cleaned up files with predictors was created we 
# did some simple visualizations to see how some predictors
# correlated to our response variable.
###############################################################################

# adding libraries
library(dplyr)  # data manipulation
library(ggvis)  # graphing
library(ggplot2) # graphing 

# reading in csv files
business_nocat <- 
  read.csv("files/predictors_with_rate_of_reviews.csv", 
           stringsAsFactors=FALSE)
business_cat <-
  read.csv("files/categoryPredictors.csv", 
    stringsAsFactors=FALSE)
dim(business_nocat)  # 37334 businesses  X 24 columns
dim(business_cat)  # 37334 businesses X 126 columns

#### filtering data before running tests ####
# Creating a binary variable that sets to "1" if review_count
# is above a certain threshold, 0 otherwise
# filtering bizcats so that only certain ones are included
revct_binary=rep(0,dim(business_nocat)[1]) # initializing variable as 0

# making it 1 if review count is > 100
revct_binary[business_nocat$review_count>100]=1  

# adding this variable as a column to our predictor data frame (the no cat one)
business_nocat <- cbind(HighRevYN=revct_binary,business_nocat)
View(business_nocat)

# repeat for business_cat data set
revct_binary=rep(0,dim(business_cat)[1]) # initializing variable as 0

# making it 1 if review count is > 100
revct_binary[business_cat$review_count>100]=1

# adding this variable as a column to our predictor data frame (the no cat one)
business_cat <- cbind(HighRevYN=revct_binary,business_cat)
View(business_cat)

#### running a few lm models ####
simple_cor <- lm(review_count~stars+review_timing+popPerSqMi, 
    data = business_nocat)
summary(simple_cor) 

# stars, review timing, and population all seem to potentially correlate, 
# although our residual standard error is *way* too small for the df we have
# see below: 
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    2.174e+01  2.223e+00   9.780  < 2e-16 ***
#  stars          3.071e+00  5.741e-01   5.349 8.92e-08 ***
#  review_timing -6.236e+00  8.151e-01  -7.651 2.04e-14 ***
#  popPerSqMi    -5.931e-04  9.974e-05  -5.946 2.77e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 98.28 on 36704 degrees of freedom
# (626 observations deleted due to missingness)
# Multiple R-squared:  0.003309,  Adjusted R-squared:  0.003227 
# F-statistic: 40.61 on 3 and 36704 DF,  p-value: < 2.2e-16

#### a few visualizations ####
# look at skew, this is a graph with all of the data on it, tough to interpret
plot(business_nocat$review_count,business_nocat$review_timing)

# try sampling data at 1% of total set
# create a list of sample numbers
data_sample_nums <- sample(1:dim(business_nocat)[1], 
                           0.01*dim(business_nocat)[1], replace=FALSE)
# create a collection of data that only represents 1% of original
data_sample <- subset(business_nocat[data_sample_nums, ])
dim(data_sample) # 373 rows, easier to visualize

# view timing of reviews now, still hard to make sense of this b/c
# only outliers and very infrequently reviewed stuff is visible
data_sample %>% 
  ggvis(x = ~review_count, y = ~review_timing) %>%  # scatterplot
  layer_points(fill=~factor(metroArea)) %>% # make each metro area a diff color
  add_axis("x", title="number of reviews") %>%
  add_axis("y", title="timing of reviews")

# try plotting information just for businesses with review
# counts between 10 and 300
biz_nocat_filtered <- filter(business_nocat, review_count < 300)
biz_nocat_filtered <- filter(biz_nocat_filtered, review_count > 10)
dim(biz_nocat_filtered) # 16161 businesses now

# sample 2% of these businesses to create an interpretable graph
# generate sampling numbers
data_sample_nums2 <- sample(1:dim(biz_nocat_filtered)[1],
    0.02*dim(biz_nocat_filtered)[1], replace=FALSE)
# create collection of data that represents 2% of original 
data_sample2 <- subset(biz_nocat_filtered[data_sample_nums2, ])
dim(data_sample2) # 323 rows

# Plot star count and number of reviews
data_sample2 %>% 
  ggvis(x = ~stars, y = ~review_count) %>%  #scatterplot
  # each metro area is a difft color
  layer_points(fill=~factor(metroArea)) %>%
  # add a trendline for each metro area
  group_by(metroArea) %>% 
  layer_model_predictions(model = "lm") %>%
  # axes labels
  add_axis("x", title="stars") %>%
  add_axis("y", title="number of reviews")


data_sample2 <- filter(data_sample2, popPerSqMi <20000)
data_sample2 <- filter(data_sample2, popPerSqMi >0)
dim(data_sample2) #315 numbers

# Plot population density and number of reviews
data_sample2 %>% 
  # scatterplot, adjusting opacity
  ggvis(x = ~popPerSqMi, y = ~review_count, opacity := 0.7) %>% 
  # each metro area is a difft color
  layer_points(fill=~factor(metroArea)) %>%
  group_by(metroArea) %>% 
  # format legend title
  add_legend("fill",title="Metropolitan Area", 
    properties = legend_props(title=list(fontSize=16))) %>%
  # axis formatting
  add_axis("x", title="population density (people per square mile)", 
      title_offset = 80, properties = axis_props(title=list(fontSize=18))) %>%
  add_axis("y", title="review count",
      title_offset = 50, properties = axis_props(title=list(fontSize=18)))

# Plot skewness and number of reviews
data_sample2 %>% 
  # scatterplot
  ggvis(x = ~review_timing, y = ~review_count) %>% 
  # each metro area is a difft color
  layer_points(fill=~factor(metroArea)) %>%
  group_by(metroArea) %>% 
  # format legend title
  add_legend("fill",title="Metropolitan Area",
             properties = legend_props(title=list(fontSize=16))) %>%
  # adding trendlines for each metro area
  layer_model_predictions(model = "lm") %>%
  # formatting axes
  add_axis("x", title="distribution over time (skewness)", title_offset = 50, 
           properties = axis_props(title=list(fontSize=18))) %>%
  add_axis("y", title="number of reviews", 
           title_offset = 80, properties = axis_props(title=list(fontSize=18)))
  
# plot rate of reviews and number of reviews
data_sample2 %>%  #scatterplot
  ggvis(x = ~review_count, y = ~rate) %>% 
  # each metro area is a different color
  layer_points(fill=~factor(metroArea)) %>%
  group_by(metroArea) %>% 
  # format legend title
  add_legend("fill",title="Metropolitan Area", 
             properties = legend_props(title=list(fontSize=16))) %>%
  # add trendline for each metro area
  layer_model_predictions(model = "lm") %>%
  # format axes
  add_axis("x", title="Number of Reviews", title_offset = 50, 
          properties = axis_props(title=list(fontSize=18))) %>%
  add_axis("y", title="Number of Reviews per Day", title_offset = 80, 
           properties = axis_props(title=list(fontSize=18)))

###############################################################################
#### 8. Running Statistical Tests ####
# This code reads in predictor files
# (that each have business ids and numbers of reviews)
# and runs statistical models
###############################################################################

# Installing libraries
library(dplyr) # data manipulation
library(pROC) # creating ROC curves
library(arm)  # data analysis using regression & multilevel models
library(randomForest) # randomforest algorithm

business_cat <- read.csv("files/categoryPredictors.csv", 
                         stringsAsFactors=FALSE)
business_nocat <- read.csv("files/predictors_with_rate_of_reviews.csv", 
                           stringsAsFactors=FALSE)

dim(business_nocat)  # 37334 businesses  X 26 columns
dim(business_cat)  # 37334 businesses X 126 columns

#### filtering data before running tests, creating train/test split ####

# data filtering
# Creating a binary variable that sets to "1" if review_count
# is above a certain threshold, 0 otherwise
# filtering bizcats so that only certain ones are included
revct_binary=rep(0,dim(business_nocat)[1]) # initializing variable as 0

# making it 1 if review count is > 100
revct_binary[business_nocat$review_count>100]=1  

# adding this variable as a column to our predictor data frame (the no cat one)
business_nocat <- cbind(HighRevYN=revct_binary,business_nocat)
View(business_nocat)

# repeat for business_cat data set
revct_binary=rep(0,dim(business_cat)[1]) # initializing variable as 0

# making it 1 if review count is > 100
revct_binary[business_cat$review_count>100]=1

# adding this variable as a column to our predictor dataframe (the no cat one)
business_cat <- cbind(HighRevYN=revct_binary,business_cat)
View(business_cat)

# Training/Testing Split for both data groups 
# no seed has been set yet, we can do this later
# Code adapted from Schutt & O'Neil (p.77)
# train and test are the data sets for all predictors except the categories
n.points <- dim(business_nocat)[1]  # number of rows in first file
sampling.rate <- 0.8    # 80% samples
num.test.set.labels <- n.points*(1-sampling.rate) # size of test set

# randomly sample which rows will go in the training set
# this produces a list of numbers (80% of the values between 1 and 1309)
training <- sample(1:n.points, sampling.rate * n.points, replace=FALSE)
train <- subset(business_nocat[training, ])   # 80% of actual data in train
testing <- setdiff(1:n.points, training) # the other rows go to test set
test <- subset(business_nocat[testing, ]) # test will contain 20% of the data

# train2 and test2 are the data sets for the category predictors
# both matrices have 37334 rows of data, so the some of the code can be reused
train2 <- subset(business_cat[training, ])   # 80% of actual data in train2
test2 <- subset(business_cat[testing, ]) # test will contain 20% of the data

#### Test #1: logistic regression (not for categories)  ####
# train a logistic model to predict the number
# of reviews expected of a business based
# on a fixed set of factors
names(train)
glm.fit=glm(HighRevYN~popPerSqMi+stars+attributes.PriceRange+review_timing,
            data=train,
            family=binomial)
summary(glm.fit)


# see results below:
# Call:
#  glm(formula = HighRevYN ~ popPerSqMi + stars + attributes.PriceRange + 
#        review_timing, family = binomial, data = train)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.9038  -0.4755  -0.4067  -0.3438   2.5597  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -4.334e+00  1.456e-01 -29.767   <2e-16 ***
#  popPerSqMi            -1.032e-05  5.074e-06  -2.033    0.042 *  
#  stars                  3.256e-01  3.347e-02   9.728   <2e-16 ***
#  attributes.PriceRange  4.536e-01  3.256e-02  13.929   <2e-16 ***
#  review_timing         -2.127e-01  4.092e-02  -5.199    2e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 11920  on 19293  degrees of freedom
# Residual deviance: 11595  on 19289  degrees of freedom
# (10573 observations deleted due to missingness)
# AIC: 11605
# look at the generated model, all factors appear significant. 
# this is promising, but see below for model weaknesses.

# now, predict the probability of a restaurant being highly 
# rated.
yhat1 <- predict(glm.fit,test,type="response") # using model created above
View(yhat1)  # looking at results
length(yhat1) # 7467 variables

# Create a matrix of 0/1 with these probabilities
glm.pred=rep(0,7467)  # fill glm.pred with 0s as default

# if model predicts odds of > 100 reviews
# give it a value of 1
glm.pred[yhat1>.5]=1  

# pulls out the actual survival rates from the original data
highlyReviewed.test_actual = business_nocat$HighRevYN[testing]

# compare actual rates with predicted rates
table(glm.pred,highlyReviewed.test_actual)

# OK, so if you look at the confusion matrix
# our model is pretty crappy right now:
# highlyReviewed.test_actual
# glm.pred    0    1
# 0 7002  465
# it never predicts that a restaurant will be highly reviewed

mean(glm.pred==highlyReviewed.test_actual) # predictions equaled actual 93%
mean(glm.pred!=highlyReviewed.test_actual) # predictions were incorrect 6%

#### Test #2: logistic regression (no categories) include review timing ####
names(train)

# let's try a model with the most likely variables to use as predictors
glm.fit=glm(HighRevYN~attributes.PriceRange+review_timing+rate,
            data=train,
            family=binomial)
summary(glm.fit)

# here's the result
# Call:
#  glm(formula = HighRevYN ~ attributes.PriceRange + review_timing + 
#        rate, family = binomial, data = train)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -5.4901  -0.2880  -0.2039  -0.1385   2.7676  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -5.47016    0.11918  -45.90   <2e-16 ***
#  attributes.PriceRange  0.49170    0.04812   10.22   <2e-16 ***
#  review_timing         -1.15020    0.06491  -17.72   <2e-16 ***
#  rate                  41.18260    0.87844   46.88   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 12065.9  on 19549  degrees of freedom
# Residual deviance:  6366.7  on 19546  degrees of freedom
# (10317 observations deleted due to missingness)
# AIC: 6374.7
# Number of Fisher Scoring iterations: 7

# now, predict the probability of a restaurant being highly 
# rated.
yhat2 <- predict(glm.fit,test,type="response") # using model created above
View(yhat2)  # looking at results
length(yhat2) # 7467 variables


# Create a matrix of 0/1 with these probabilities
glm.pred=rep(0,7467)  # fill glm.pred with 0s as default

# if model predicts odds of > 100 reviews
# give it a value of 1
glm.pred[yhat2>.5]=1  

# pulls out the actual survival rates from the original data
highlyReviewed.test_actual = business_nocat$HighRevYN[testing]

# compare actual rates with predicted rates
table(glm.pred,highlyReviewed.test_actual)

# things look pretty good when rate of review is incorporated
# highlyReviewed.test_actual
# glm.pred    0    1
# 0     6946  248
# 1       56  217
mean(glm.pred==highlyReviewed.test_actual) # predictions equaled actual 96%
mean(glm.pred!=highlyReviewed.test_actual) # predictions were incorrect 4%

#### Test #3 Random Forest ####
# fit a random forest model to data
# first thing to do, factor reviews count 
View(business_nocat)

# factors needed for RF classifier
business_nocat$HighRev = factor(business_nocat$HighRevYN)  

# Explore the random forest model for passenger survival
fit <- randomForest(HighRev~stars+review_timing+popPerSqMi, na.action = na.omit,
                            data=train)

fit   # shows an estimated error rate of 20.82%, similar to model in problem 4
plot(fit) # Viewing fit

importance(fit)   # showing how title helps prediction more than other factors
# results of importance()
# MeanDecreaseGini
# parch          12.56433
# pclass         38.98693
# title         104.20922

# Saving first set of predictions as yhat3
yhat3 <- predict(fit, newdata=test, type="prob")[,1]

#### Test #4 Logistic Regression for categories ####
# viewing names of business categories
names(business_cat)

# picking best categories (based on our EDA) to use in this model
vars <- c("Restaurants","Nightlife","Bars","American..New.","Mexican",
          "Beauty...Spas","Shopping","Active.Life","Fast.Food", 
          "American..Traditional.")

# creating the formula for business categories to add to lm model
glm_form <- as.formula(paste(names(business_cat)[1], "~", 
                      paste(vars, collapse="+"), collapse="")) 

# run binomial model
glm.fit=glm(glm_form, data=train2, family=binomial)
summary(glm.fit)

# most factors appear significant, see below
# Call:
#  glm(formula = glm_form, family = binomial, data = train2)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.1643  -0.4858  -0.2236  -0.1614   3.3692  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -3.67603    0.05751 -63.919  < 2e-16 ***
#  Restaurants             1.96031    0.06658  29.443  < 2e-16 ***
#  Nightlife               1.10873    0.18455   6.008 1.88e-09 ***
#  Bars                   -0.42932    0.19893  -2.158 0.030912 *  
#  American..New.          0.57600    0.08860   6.501 7.96e-11 ***
#  Mexican                -0.36177    0.09683  -3.736 0.000187 ***
#  Beauty...Spas          -0.65792    0.19188  -3.429 0.000606 ***
#  Shopping               -1.33851    0.19397  -6.900 5.18e-12 ***
#  Active.Life            -0.07528    0.18487  -0.407 0.683854    
# Fast.Food              -1.92921    0.20500  -9.411  < 2e-16 ***
#  American..Traditional. -0.32365    0.09772  -3.312 0.000926 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1) 
#
# Null deviance: 14043  on 29866  degrees of freedom
# Residual deviance: 11858  on 29856  degrees of freedom
# AIC: 11880


# now, predict the probability of a restaurant being highly 
# rated.
yhat4 <- predict(glm.fit,test2,type="response") # using model created above


# Create a matrix of 0/1 with these probabilities
glm.pred=rep(0,length(yhat4))  # fill glm.pred with 0s as default
# if model predicts odds of > 100 reviews
# give it a value of 1
glm.pred[yhat4>.5]=1  

# pulls out the actual survival rates from the original data
highlyReviewed.test_actual = business_nocat$HighRevYN[testing]

# compare actual rates with predicted rates
table(glm.pred,highlyReviewed.test_actual)
# again, we can't actually predict highly reviewed restaurants
#    highlyReviewed.test_actual
# glm.pred    0    1
#       0 7002  465
# OK, so if you look at the confusion matrix
# our model is pretty crappy right now:
mean(glm.pred==highlyReviewed.test_actual) # predictions equaled actual 93%
mean(glm.pred!=highlyReviewed.test_actual) # predictions were incorrect 7%

#### Re-do Training/Testing Split ####
# create a data set with just lots of reviews
high_rev_biz <- subset(business_nocat, HighRevYN==1)
dim(high_rev_biz) # 2363 rows

# and one without a lot of reviews
low_rev_biz <- subset(business_nocat, HighRevYN==0)
dim(low_rev_biz) # 34971 rows

# do every training/testing thing twice
n.points1 <- dim(high_rev_biz)[1]
n.points2 <- dim(low_rev_biz)[1]

sampling.rate1 <- 0.8    # 1890 highly reviewed restaurants
sampling.rate2 <- 0.15   # 3497 less reviewed restaurants

# highly reviewed restaurants; 472.6 in testing set
# size of test set for highly reviewed restaurants
num.test.set.labels1 <- n.points1*(1-sampling.rate1) 
# size of test set for less reviewed restaurants
num.test.set.labels2 <- n.points2*(1-sampling.rate1) 

# randomly sample which rows will go in the training set
# this produces a list of numbers (80% of the values)
# training1/testing1 are from the highly reviewed businesses
training1 <- sample(1:n.points1, sampling.rate1 * n.points1, replace=FALSE)
train1 <- subset(high_rev_biz[training1, ])   # 80% of actual data in train

testing1 <- setdiff(1:n.points1, training1) # the other rows go to test set
# test will contain 20% of the data, 473 businesses
test1 <- subset(high_rev_biz[testing1, ]) 

# training2/testing2 are from the highly reviewed businesses
training2 <- sample(1:n.points2, sampling.rate2 * n.points2, replace=FALSE)
train2 <- subset(low_rev_biz[training2, ])   # 15% of actual data in train

# the other rows go into a potential test set
testing_pool <- setdiff(1:n.points2, training2)
npoints3 <- length(testing_pool)

# We will sample 5% of these for our test data
testing2 <- sample(1:npoints3, 0.05 * npoints3, replace=FALSE)
test2 <- subset(low_rev_biz[testing2, ]) # test will contain 20% of the data

train_biased <- rbind(train1,train2)
test_biased <- rbind(test1,test2)
training_biased <- c(training1,training2)
testing_biased <- c(testing1,testing2)

#### Test #5: Try to do a logistic regression model #### 
####          with a biased training set ####
names(train_biased)
glm.fit=glm(HighRevYN~stars+attributes.PriceRange+review_timing,
            data=train_biased,
            family=binomial)
summary(glm.fit)
# look at the generated model, all factors appear significant. 
# this is promising, but see below for model weaknesses.
# Call:
#  glm(formula = HighRevYN ~ stars + attributes.PriceRange + review_timing, 
#      family = binomial, data = train_biased)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6295  -0.9397  -0.7523   1.3241   1.9901  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           -3.17042    0.18810 -16.855  < 2e-16 ***
#  stars                  0.42153    0.04438   9.497  < 2e-16 ***
#  attributes.PriceRange  0.50414    0.04181  12.058  < 2e-16 ***
#  review_timing         -0.23773    0.05176  -4.593 4.38e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 6646.1  on 5118  degrees of freedom
# Residual deviance: 6375.4  on 5115  degrees of freedom
# (2016 observations deleted due to missingness)
# AIC: 6383.4

# now, predict the probability of a restaurant being highly 
# rated.
yhat5 <- predict(glm.fit,test_biased,
    type="response") # using model created above
View(yhat5)  # looking at results

# Create a matrix of 0/1 with these probabilities
glm.pred=rep(0,length(yhat5))  # fill glm.pred with 0s as default
# if model predicts odds of > 100 reviews
# give it a value of 1
glm.pred[yhat5>.5]=1  

# pulls out the actual survival rates from the original data
highlyReviewed.test_actual = business_nocat$HighRevYN[testing_biased]

# compare actual rates with predicted rates
table(glm.pred,highlyReviewed.test_actual)
# highlyReviewed.test_actual
# glm.pred    0    1
# 0 1712  107
# 1  133    7
# even with the bias our precision is low

# OK, so if you look at the confusion matrix
# our model is pretty crappy right now:
mean(glm.pred==highlyReviewed.test_actual) # predictions equaled actual 88%
mean(glm.pred!=highlyReviewed.test_actual) # predictions were incorrect 12%
# but we're not catching a lot of the restaurants we should that
# actually get >100 reviews 

#### Test #6 Combine Categories and ####
# other data, bias training data
names(business_cat)[1:5]
business_cat2 <- subset(business_cat, select = c("business_id",
        "Restaurants","Nightlife","Bars","American..New.","Mexican",
        "Beauty...Spas","Shopping","Active.Life","Fast.Food", 
                   "American..Traditional."))
business_nocat2 <- subset(business_nocat, select = c("HighRevYN",
          "business_id","review_count","attributes.PriceRange",
          "review_timing","metroArea"))

merged_preds <- merge(x=business_nocat2,y=business_cat2,
                                by="business_id", all.x=TRUE)
dim(merged_preds)   # 16 columns x 37334 businesses

#### Re-do Training/Testing Split ####
# create a data set biased to categories
View(merged_preds)
many_cat_biz <- subset(merged_preds, Nightlife==1 | Bars==1 |
          Fast.Food==1 | American..New.==1 | Mexican==1 | Beauty...Spas==1 |
            Shopping==1| Active.Life==1 | American..Traditional.==1)
dim(many_cat_biz) # 16307 businesses have some kind of important
                  # category assigned to them

# and ones without a lot of categories
few_cat_biz <- subset(merged_preds, Nightlife==0 & Bars==0 &
            Fast.Food==0 & American..New.==0 & Mexican==0 & Beauty...Spas==0 
            & Shopping==0 & Active.Life==0 & American..Traditional.==0) 
dim(few_cat_biz) # 21027 rows

# do every training/testing thing twice
n.points1 <- dim(many_cat_biz)[1]
n.points2 <- dim(few_cat_biz)[1]

sampling.rate1 <- 0.8    # 13046 categorized restaurants in test
sampling.rate2 <- 0.1   # 2103 uncategorized restaurants in test

# highly reviewed  in testing set
# size of test set for highly categorized businesses
num.test.set.labels1 <- n.points1*(1-sampling.rate1) 

# size of test set for uncategorized businesses
# keep same sampling rate for test (make it unbiased)
num.test.set.labels2 <- n.points2*(1-sampling.rate1) 

# randomly sample which rows will go in the training set
# this produces a list of numbers (80% of the values)
# training1/testing1 are from the highly reviewed businesses
training1 <- sample(1:n.points1, sampling.rate1 * n.points1, replace=FALSE)
train1 <- subset(many_cat_biz[training1, ])   # 80% of actual data in train

testing1 <- setdiff(1:n.points1, training1) # the other rows go to test set

# test will contain 20% of the data
test1 <- subset(many_cat_biz[testing1, ]) 

# training2/testing2 are from the highly categorized businesses
training2 <- sample(1:n.points2, sampling.rate2 * n.points2, replace=FALSE)
train2 <- subset(few_cat_biz[training2, ])   # 15% of actual data in train

# the other rows go into a potential test set
testing_pool <- setdiff(1:n.points2, training2)
npoints3 <- length(testing_pool)

# We will sample 5% of these for our test data
testing2 <- sample(1:npoints3, 0.05 * npoints3, replace=FALSE)
test2 <- subset(few_cat_biz[testing2, ]) # test will contain 20% of the data

train_biased <- rbind(train1,train2)
test_biased <- rbind(test1,test2)
training_biased <- c(training1,training2)
testing_biased <- c(testing1,testing2)

length(training_biased) # 15147 rows
length(testing_biased)  # 4208 rows

# run actual model with newly biased, combined data set
names(train_biased)

# creating long string of predictors to feed in to model
vars <- c("attributes.PriceRange","review_timing","metroArea",
          "Restaurants","Nightlife","Bars","American..New.","Mexican",
          "Beauty...Spas","Shopping","Active.Life","Fast.Food", 
          "American..Traditional.")

# creating the formula for business categories to add to lm model
glm_form <- as.formula(paste(names(train_biased)[2], "~", 
                             paste(vars, collapse="+"), collapse="")) 

glm.fit=glm(glm_form, data=train_biased,family=binomial)
summary(glm.fit)
# here are the results
# Call:
#  glm(formula = glm_form, family = binomial, data = train_biased)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.6595  -0.4035  -0.1914  -0.1082   3.6778  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -5.48800    0.27426 -20.010  < 2e-16 ***
#  attributes.PriceRange   0.80844    0.05922  13.652  < 2e-16 ***
#  review_timing          -0.18540    0.06343  -2.923 0.003468 ** 
#  metroAreaPHOENIX        0.91125    0.21179   4.303 1.69e-05 ***
#  metroAreaVEGAS          1.41867    0.21330   6.651 2.91e-11 ***
#  Restaurants             1.35779    0.14286   9.504  < 2e-16 ***
#  Nightlife               0.26439    0.21390   1.236 0.216432    
# Bars                    0.05863    0.21329   0.275 0.783409    
# American..New.          0.43840    0.11284   3.885 0.000102 ***
#  Mexican                -0.10719    0.12495  -0.858 0.390999    
# Beauty...Spas          -1.54413    0.22065  -6.998 2.59e-12 ***
#  Shopping               -2.29477    0.21453 -10.697  < 2e-16 ***
#  Active.Life             0.28394    0.31935   0.889 0.373936    
# Fast.Food              -1.52777    0.21153  -7.222 5.11e-13 ***
#  American..Traditional. -0.28578    0.11591  -2.466 0.013680 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# (Dispersion parameter for binomial family taken to be 1)
# Null deviance: 6330.9  on 12020  degrees of freedom
# Residual deviance: 4867.5  on 12006  degrees of freedom
# (3126 observations deleted due to missingness)
# AIC: 4897.5

# now, predict the probability of a restaurant being highly 
# rated.
yhat6 <- predict(
  glm.fit,test_biased,type="response") # using model created above

# Create a matrix of 0/1 with these probabilities
glm.pred=rep(0,length(yhat6))  # fill glm.pred with 0s as default
# if model predicts odds of > 100 reviews
# give it a value of 1
glm.pred[yhat6>.5]=1  

# pulls out the actual survival rates from the original data
highlyReviewed.test_actual = merged_preds$HighRevYN[testing_biased]

# compare actual rates with predicted rates
table(glm.pred,highlyReviewed.test_actual)

# highlyReviewed.test_actual
# glm.pred    0    1
# 0 3927  260
# 1  20    1
# even with the bias our precision is low
# we correctly predicted only 1 business

# OK, so if you look at the confusion matrix
# our model is pretty crappy right now:
mean(glm.pred==highlyReviewed.test_actual) # predictions equaled actual 93%
mean(glm.pred!=highlyReviewed.test_actual) # predictions were incorrect 6%
# but we're not catching a lot of the restaurants we should that
# actually get > 100 reviews 

###############################################################################
#### 9. CART predictions ####
# In this section, we utilize the "rpart" package to 
# create a classification and regression tree (CART) model.
# We have a total of 16 predictors we use depending on the formula
###############################################################################

#### 9.1 Citations for this section ####
# http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
# rpart code adapted from http://rpubs.com/minma/cart_with_rpart
# ROC Curve code adapted from http://scg.sdsu.edu/ctrees_r/

#### import predictor data ####
predictors <- read.csv("files/predictors_with_rate_of_reviews.csv")

# create a variable to predict if a business will get more than 100 reviews
# where 1 is yes, the business is predicted to have more than 100 reviews
# and 0 is no, the business is not predicted to have more than 100 reviews
# and add the variable into our data frame
predictors$limiter <-
  as.factor(ifelse(predictors$review_count > 100, c(1), c(0)))

# tidy the dataset, get rid of 1:1 predictors like name and review_count
raw <- subset(predictors, select=c(open, stars:review_timing, rate, limiter))

# change price and stars into factors
raw$attributes.PriceRange <- 
  as.factor(raw$attributes.PriceRange)
raw$stars <- 
  as.factor(raw$stars)
raw <- na.omit(raw)  # remove rows with missing data

#### CART 1 Use all 16 predictors ####

frmla <- limiter ~ .  # create a model using all variables
set.seed(123)  # set a seed to enable replication

# set a small cp (complexity parameter)
mycontrol <- rpart.control(cp = 0.0001)

# create a classification and regression tree using the "rpart" package
fit <- rpart(frmla, method="class", data=raw, control = mycontrol)

printcp(fit)  # display the results
plotcp(fit)  # visualize cross-validation results
summary(fit)  # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]

# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# create a confusion matrix
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Plot the tree #
plot(fit.pruned, uniform = TRUE, main="CART for Yelp Reviews: 16 variables")
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

# Alternative plots using prp
# set faclen = 0 for full-length attribute names
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1, 
    main = "CART for Yelp Reviews: 16 variables") 

# run prp with defaults to try to get the plot to not overlap
prp(fit.pruned, 
    main = "CART for Yelp Reviews: 16 variables") 

# Use fancyRpartPlot to try and make a prettier tree graphic
fancyRpartPlot(fit.pruned, cex = 0.5,  # change the cex to alter text size
               main = "CART for Yelp Reviews: 16 variables")  

# Create ROC Curve using
# uses the "ROCR" package to fit our tree predictions to a ROC curve
fit.pr <- predict(fit.pruned, newdata = raw, type="prob")[,2]
fit.pred <- prediction(fit.pr, raw$limiter)
fit.perf <- performance(fit.pred, "tpr", "fpr")
yfit1 <- fit.perf  # name this our first ROC graph

#### CART 2 Try using less variables in our model ####

# eliminate attributes from our predictors
frmla <- limiter ~ open + stars + metroArea + popPerSqMi +
  review_timing + rate
fit <- rpart(frmla, method="class", data=raw, control = mycontrol)
printcp(fit)  # display the results
plotcp(fit)  # visualize cross-validation results
summary(fit)  # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]
# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# Confusion Matrix
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Plot the tree
plot(fit.pruned, uniform = TRUE, main= "CART for Yelp Reviews: 6 variables")
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

# Alternative plots using prp
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1, 
    main = "CART for Yelp Reviews: 6 variables") 
# set faclen = 0 for full-length attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap

# Use fancyRpartPlot to try and make a prettier tree graphic
# change the cex to alter text size
fancyRpartPlot(fit.pruned, cex = NULL,
               main = "CART for Yelp Reviews: 6 variables")  

# Create ROC Curve using

# uses the "ROCR" package to fit our tree predictions to a ROC curve
fit.pr <- predict(fit.pruned, newdata = raw, type="prob")[,2]
fit.pred <- prediction(fit.pr, raw$limiter)
fit.perf <- performance(fit.pred, "tpr", "fpr")
yfit2 <- fit.perf

#### CART 3 eliminate rate and attributes from our model ####

# eliminate attributes and rate from our predictors
frmla <- limiter ~ open + stars + metroArea + popPerSqMi +
  review_timing 
fit <- rpart(frmla, method="class", data=raw, control = mycontrol)
printcp(fit)  # display the results
plotcp(fit)  # visualize cross-validation results
summary(fit)  # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]
# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# Confusion Matrix
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Plot the tree
plot(fit.pruned, uniform = TRUE, main= "CART for Yelp Reviews: 5 variables")
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

# Alternative plots using prp
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1, 
    main = "CART for Yelp Reviews: 5 variables") 
# set faclen = 0 for full-length attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap

# Use fancyRpartPlot to try and make a prettier tree graphic
# change the cex to alter text size
fancyRpartPlot(fit.pruned, cex = NULL,
               main = "CART for Yelp Reviews: 5 variables")  

# Create ROC Curve using ROCR 
# uses the "ROCR" package to fit our tree predictions to a ROC curve
fit.pr <- predict(fit.pruned, newdata = raw, type="prob")[,2]
fit.pred <- prediction(fit.pr, raw$limiter)
fit.perf <- performance(fit.pred, "tpr", "fpr")
yfit3 <- fit.perf

#### CART 4 only eliminate rate from our model ####

# eliminate attributes and rate from our predictors
frmla <- limiter ~ . -rate 
fit <- rpart(frmla, method="class", data=raw, control = mycontrol)
printcp(fit)  # display the results
plotcp(fit)  # visualize cross-validation results
summary(fit)  # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]
# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# Confusion Matrix
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Plot the tree #
plot(fit.pruned, uniform = TRUE, main= "CART for Yelp Reviews: 15 variables")
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

# Alternative plots using prp
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1, 
    main = "CART for Yelp Reviews: 15 variables") 
# set faclen = 0 for full-length attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap

# Use fancyRpartPlot to try and make a prettier tree graphic
# change the cex to alter text size
fancyRpartPlot(fit.pruned, cex = 0.5,
               main = "CART for Yelp Reviews: 15 variables")  

# Create ROC Curve using ROCR 
# uses the "ROCR" package to fit our tree predictions to a ROC curve
fit.pr <- predict(fit.pruned, newdata = raw, type="prob")[,2]
fit.pred <- prediction(fit.pr, raw$limiter)
fit.perf <- performance(fit.pred, "tpr", "fpr")
yfit4 <- fit.perf

#### Plot our ROC Curves for our CART fits ####
plot(yfit1, lwd = 2, col = "blue",  # 16 variables
     main = "ROC: Classification Trees on Yelp Dataset")
plot(yfit2, add = TRUE, lwd = 2, col = "red",  # 6 variables
     ) # use "add = TRUE" to overlap the line
plot(yfit3, add = TRUE, lwd = 2, col = "green")  # 5 variables
plot(yfit4, add = TRUE, lwd = 2, col = "orange")  # 15 variables
abline(a = 0, b = 1)
legend("bottomright", c("16 predictors", "6 predictors", 
                        "5 predictors","15 predictors"),
       lty = 1, 
       col = c("blue", "red", "green", "orange"), bty = "o",)


###############################################################################
#### 10 exporting for map visualization ####
# Here we create segment the yelp data set and 
# export some data as csv's for processing in QGIS
###############################################################################

#### Create data sets for each state in our group ####

business_predictors <- read.csv("files/predictors_with_rate_of_reviews.csv")

# lets focus on Phoenix
business_phoenix <- subset(business_predictors, metroArea=="PHOENIX")

# also pull out Vegas
business_vegas <- subset(business_predictors, metroArea=="VEGAS")

# also get Madison
business_madison <- subset(business_predictors, metroArea=="MADISON")

# how about all the states
business_us <- business_predictors

#### export local data frames to csv ####
write.csv(business_phoenix, file = "files/business_phoenix.csv")
write.csv(business_vegas, file = "files/business_vegas.csv")
write.csv(business_madison, file ="files/business_madison.csv")
write.csv(business_us, file = "files/business_us.csv")

# Load the business_(location).csv files into GIS
# We used QGIS to produce our maps

###############################################################################
#### 11. Finding the most frequently rated businesses ####
# For each of the three states analyzed, generate
# list of frequently rated businesses.
###############################################################################

# import predictors table
rate_of_reviews <- read.csv("files/predictors_with_rate_of_reviews.csv")

top_NV <- rate_of_reviews %>% #create a new dataframe
  dplyr::select(business_id:rate) %>% #select all variables
  filter(metroArea=="VEGAS") %>% #filter for businesses in Nevada
  arrange(desc(rate)) #arrange by most frequently rated
View(top_NV, "NV")


top_AZ <- rate_of_reviews %>% #create a new dataframe
  dplyr::select(business_id:rate) %>% #select all variables
  filter(metroArea=="PHOENIX") %>% #filter for businesses in Arizona
  arrange(desc(rate)) #arrange by most frequently rated
View(top_AZ, "AZ")

top_WI <- rate_of_reviews %>% #create a new dataframe
  dplyr::select(business_id:rate) %>% #select all variables
  filter(metroArea=="MADISON") %>% #filter for businesses in Wisconsin
  arrange(desc(rate)) #arrange by most frequently rated
View(top_WI, "WI")

###############################################################################
####  11.2 Stacked Barplot  ####
###############################################################################


ordered <- predictors %>%
  select(business_id:name) %>%
  filter(review_count > 30) %>%
  arrange(desc(rate))


# Review Count > 30 = 7598 businesses 
# (in order to allow businesses to accumulate)

# at least 30 reviews
very_freq_rated2 <- ordered[1:1519,]
freq_rated2 <- ordered[1520:3039,]
middle_rated2 <- ordered[3040:4559,]
less_rated2 <- ordered[4560:6078,]
least_rated2 <- ordered[6079:7598,]


# add a variable to each frequency for ordering purposes
very_freq_rated2$variable <- 5
freq_rated2$variable <- 4
middle_rated2$variable <- 3
less_rated2$variable <- 2
least_rated2$variable <- 1

# recombine rows
star_barplot2 <- 
  rbind(very_freq_rated2, freq_rated2, 
        middle_rated2, less_rated2, least_rated2)

# calculate star frequency 
counts <- table(star_barplot2$variable, star_barplot2$stars)

# plot
barplot(counts, main="Business Frequency of Reviews versus Star Ratings",
        xlab="Star Rating", 
        col=c("snow4", "snow3", "snow1", "slategray1", "slategray3"),
        ylab="number of businesses")
mtext("Is frequency of reviews a proxy for excellence?", 3, font=2)

# add legend
legend(7.5,2750, c("Very Frequent","Frequent", "Average", 
                   "Less Frequent", "Least Frequent"), 
       lty=c(1,1), lwd=c(8,8), 
       col=c("slategray3","slategray1", "snow1", "snow3", "snow4"), 
       title="Rate of Reviews") 



###############################################################################
#### 12 Review of Skew ####
# The following process was used to look at individual businesses 
# review counts over time.We define the variable 'review_timing' 
# using skewness, using the D'Agostino test for 
# skewness included in the R package 'moments.'
# Skewness for individual businesses are visualized 
# below either by Review Count or Review Density over Review Date.
###############################################################################

# Libraries Used
library(plyr)
library(dplyr)
library(ggplot2)
library(moments)


predictors_with_rate_of_reviews <- 
  read.csv("files/predictors_with_rate_of_reviews.csv")
str(predictors_with_rate_of_reviews)
View(predictors_with_rate_of_reviews)

usBusinesses <- read.csv("files/usBusinesses.csv", stringsAsFactors=FALSE)
str(usBusinesses)
View(usBusinesses)

# adding a date formatted object to the usBusinesses dataframe.
datez <- as.Date(usBusinesses$date)
usBusinesses[,9] <- datez
names(usBusinesses)[9] <- "review_date"
names(usBusinesses)
summary(usBusinesses$new_date)



# Sorting businesses with review skewness over time of more than 0.
posSkewSet <- predictors_with_rate_of_reviews %>% 
  filter(review_count > 30) %>% 
  filter(review_timing > 0.0)

# A random sample of size 30 from the predictors_with_rate_of_reviews dataset
# that includes businesses with review skewness over time of more than 0.
samp_posSkew <- posSkewSet[sample(1:nrow(posSkewSet), 30, replace=FALSE),]

# To save list of the sample of 30 positive skew: 
# write.csv(samp_posSkew,"positive_skew_sample.csv")

# Using businesses from the sample with positive skew values, 
# we then filtered these businesses *out* of the larger 
# usBusinesses dataframe in order to create a new dataframe 
# that includes all of the individual dates in which the 
# specific businesses listed below were reviewed.
# This then allows us the see how reviews accumulate through time, by date.
pos_skew_biz <- usBusinesses %>% 
  filter(
    business_id=="0AZiq_OMKs3I9lTmwd5PSg" |
      business_id=="5niUestkeI0tBcBAv4SyKw" | 
      business_id=="LCeF1wK5OqP3PPEVBa5Xfw" | 
      business_id=="k2JlEtJIvZN-2bwt5BGJHQ" |
      business_id=="CtCJiB-BnHpr7ZJcSzEVaQ" | 
      business_id=="X-dpycbo5fy8AyoHCUCryA" | 
      business_id=="9Zy895GTvOVPVBvyp9deiw" | 
      business_id=="whRXOWa9E58yMXi5v-AT3A" |
      business_id=="f5eqFho2fWVggLKXdTh-zg" | 
      business_id=="IceZ4BtTz76eppjNWm-c6A" |
      business_id=="xY1sPHTA2RGVFlh5tZhs9g" | 
      business_id=="O_5RlNmiyBRzwGZAwfsZFA" | 
      business_id=="obp9GWaEnTplW3YxhdexGQ" | 
      business_id=="lvM9OzZaZWo1gC8Mwcthjw" |
      business_id=="kwD6P2-EV87u6aVOcnAcpQ" | 
      business_id=="008fyQR7HVmsaZHrrZWuLQ" |
      business_id=="iaEqryy7OwAOAQkLWDBquQ" | 
      business_id=="eq6lQI039SBLC6sHm3idGA" | 
      business_id=="5M7YgorCUTEZkKYNbKDHTA" | 
      business_id=="fUrErf3orbQN26ocjCgvlA" |
      business_id=="H6F8CVOFILdnSUQtmvy_zQ" | 
      business_id=="sKakNuBkT4-Jc7ugp9GGew" |
      business_id=="CMTDZRDnv_O0rwAvRVbjvg" | 
      business_id=="UeJJZwYkxjXQjBj-Pvu1NQ" | 
      business_id=="zJ-vTnhw7i3s7fbrhoksDA" | 
      business_id=="cRfn_vMQ9YExuKb378CwjQ" | 
      business_id=="YxQ1ssiSvjmprGHKygKfcg" | 
      business_id=="FM3JgsDg5vLJ2x1_NM9CcA" | 
      business_id=="Trar_9cFAj6wXiXfKfEqZA" | 
      business_id=="JOJtP3IQbCg65rssBewUXg"
  )

# This new dataframe can now be saved: 
# write.csv(pos_skew_biz, "pos_skew_biz_samp.csv")


### -----------

# We can now look at the individual businesses review counts 
# over time and visualize them using the process given below. 
# From the positive skewed sample set, we look at 
# three businesses with a positive skew value.


# Positve Skew, Business Number 1 (psb1)
# business id# 0AZiq_OMKs3I9lTmwd5PSg

# Filtering out business id# 0AZiq_OMKs3I9lTmwd5PSg to look at review counts. 
predictors_with_rate_of_reviews %>% 
  filter(business_id=="0AZiq_OMKs3I9lTmwd5PSg")


# Output:
# X.2            business_id  X.1    X open review_count longitude
# 1 1288 0AZiq_OMKs3I9lTmwd5PSg 1288 1288 TRUE           46 -112.0114
# latitude stars attributes.PriceRange attributes.Ambience.romantic
# 1 33.50987   3.5                     2                        FALSE
# attributes.Ambience.intimate attributes.Ambience.touristy
# 1                        FALSE                        FALSE
# attributes.Ambience.hipster attributes.Ambience.divey
# 1                       FALSE                     FALSE
# attributes.Ambience.classy attributes.Ambience.trendy
# 1                      FALSE                      FALSE
# attributes.Ambience.upscale attributes.Ambience.casual metroArea
# 1                       FALSE                       TRUE   PHOENIX
# popPerSqMi review_timing earliestReview open.1       rate
# 1   4266.041     0.8303225     2009-09-28   1752 0.02625571
# name
# 1 Tommy V's Osteria and Pizzeria

# Creating a df for only business id# 0AZiq_OMKs3I9lTmwd5PSg 
# to run in ggplot below.
psb1 <- usBusinesses %>% filter(business_id=="0AZiq_OMKs3I9lTmwd5PSg")
View(psb1)


# Histogram by review count
ggplot(psb1, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +  
  ggtitle("Positive Skewed Business # 1  \
          ID: 0AZiq_OMKs3I9lTmwd5PSg  \  
          Review skew: 0.8303225  \  
          Review count: 46  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(psb1, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=20, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") + 
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Positive Skewed Business # 1  \  
          ID: 0AZiq_OMKs3I9lTmwd5PSg  \  
          Review skew: 0.8303225  \  
          Review count: 46  ") + 
  theme(plot.title = element_text( face="bold"))


# This process is now repeated for two additional 
# positively skewed businesses below.

### -----------


# Positve Skew, Business Number 2 (psb2)
# business id# 5niUestkeI0tBcBAv4SyKw

predictors_with_rate_of_reviews %>% 
  filter(business_id=="5niUestkeI0tBcBAv4SyKw")

psb2 <- pos_skew_biz %>% 
  filter(business_id=="5niUestkeI0tBcBAv4SyKw")
View(psb2)

# Histogram by review count
ggplot(psb2, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=20, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +
  ggtitle("Positive Skewed Business # 2  \  
          ID: 5niUestkeI0tBcBAv4SyKw \  
          Review skew: 0.191877  \  
          Review count: 116  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(psb2, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=20, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") +
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Positive Skewed Business # 2  \  
          ID: 5niUestkeI0tBcBAv4SyKw \  
          Review skew: 0.191877  \  
          Review count: 116  ") + 
  theme(plot.title = element_text( face="bold"))


### -----------


# Positve Skew, Business Number 3 (psb3)
# business id# LCeF1wK5OqP3PPEVBa5Xfw


predictors_with_rate_of_reviews %>% 
  filter(business_id=="LCeF1wK5OqP3PPEVBa5Xfw")

psb3 <- pos_skew_biz %>% 
  filter(business_id=="LCeF1wK5OqP3PPEVBa5Xfw")
View(psb3)

# Histogram by review count
ggplot(psb3, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=20, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +
  ggtitle("Positive Skewed Business # 3  \  
          ID: LCeF1wK5OqP3PPEVBa5Xfw \  
          Review skew: 0.337253  \  
          Review count: 674  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(psb3, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=20, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") +
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Positive Skewed Business # 3  \  
          ID: LCeF1wK5OqP3PPEVBa5Xfw \  
          Review skew: 0.337253  \  
          Review count: 674  ") + 
  theme(plot.title = element_text( face="bold"))


### -----------
### -----------
### -----------

# The same process that has been descried above is now repeated 
# for businesses with negative skew values.
# We can now look at the individual businesses review counts 
# over time and visualize them using the process given below. 
# From the negative skewed sample set, we look at 
# three businesses with a negative skew value.

# Sorting businesses with review skewness over time of less than 0.
negSkewSet <- predictors_with_rate_of_reviews %>% 
  filter(review_count > 30) %>% filter(review_timing < 0.0)

# A random sample of size 30 from the predictors_with_rate_of_reviews dataset
# that includes businesses with review skewness over time of less than 0.
samp_negSkew <- negSkewSet[sample(1:nrow(negSkewSet), 30, replace=FALSE),]

# To save list of the sample of 30 negative skew: 
# write.csv(samp_negSkew,"negative_skew_sample.csv")  

# Using businesses from the sample with negative skew values, 
# we then filtered these businesses *out* of the larger 
# usBusinesses dataframe in order to create a new dataframe 
# that includes all of the individual dates in which the 
# specific businesses listed below were reviewed.
# This then allows us the see how reviews accumulate through time, by date.
neg_skew_biz <- usBusinesses %>% 
  filter(
    business_id=="_9BqmRowpWfy4wDPXLSDHA" |
      business_id=="-V44Z21fJclQb3NJRe-4dQ" | 
      business_id=="S85y5Iv3LlVsy2sjqHdGhg" |
      business_id=="yJj8GYCJdnbBsAT1rcB67w" | 
      business_id=="UrnRSzly4onGLUzZQrbT0A" |
      business_id=="SBaGTrc0WMhHOMLS3WVOTw" | 
      business_id=="vSAKKH1XIeKjo7R6tXt_5Q" |
      business_id=="ygI6Q8ck2qf3Y0iCM2Lkag" | 
      business_id=="rYXu8NasDNnLZrmHymaQWg" |
      business_id=="tR8-0OWg4UbwE16s6w6Y2A" | 
      business_id=="75qyCqXYNvQz00cqv02HZg" |
      business_id=="cQJM8VpY21O3HVu7j_b7Vw" | 
      business_id=="1EF9J1TGjxX5gXB3U8hjvA" |
      business_id=="YJVEaEsvgoZvI_s04hb7iQ" | 
      business_id=="iChdAsAIPgKIhV7fLOkt1Q" |
      business_id=="gCo--CY5aUoByxUAET2cTw" | 
      business_id=="Ymbxezwhkf8-8sDWUWOScQ" |
      business_id=="Ug97walz3YLxPMrpkktsFg" | 
      business_id=="lcVlSi_bCAmiSuJx61BSAQ" |
      business_id=="AuGGWt7dV11fRBl_LPQGwg" | 
      business_id=="nXKwzVKJCtIGd4HxXgjdnQ" |
      business_id=="kZW9u9U42WiX1UQlMEPLrw" | 
      business_id=="JxVGJ9Nly2FFIs_WpJvkug" |
      business_id=="fAJM5vQ4dJn2xSfWMAEu-A" | 
      business_id=="9JZzT2BMalNseFn11x5EIQ" |
      business_id=="fHX1t2iDrhBAiq5wkc4tRQ" | 
      business_id=="IW9yHeqnFCEbO5s1UCrqeA" |
      business_id=="r8CwFUEQtL8gAT9KHnNhuw" | 
      business_id=="8yxuxxKHRtyIzwE4cIVRiA" |
      business_id=="hVSuSlR4uYNnap2LU-6Tcg"
  )

# This new dataframe can now be saved: 
# write.csv(neg_skew_biz, "neg_skew_biz_samp.csv")


### -----------


# We can now look at the individual businesses review counts 
# over time and visualize them using the process given below. 
# From the negative skewed sample set, 
# we look at three businesses with a negative skew value.


# Negative Skew, Business Number 1 (nsb1)
# business id# _9BqmRowpWfy4wDPXLSDHA

# Filtering out business id# _9BqmRowpWfy4wDPXLSDHA to look at review counts.
predictors_with_rate_of_reviews %>% 
  filter(business_id=="_9BqmRowpWfy4wDPXLSDHA")

# Output
# X.2            business_id X.1   X open review_count longitude
# 1  93 _9BqmRowpWfy4wDPXLSDHA 684 684 TRUE          108  -115.152
# latitude stars attributes.PriceRange attributes.Ambience.romantic
# 1 36.15223     4                     1                        FALSE
# attributes.Ambience.intimate attributes.Ambience.touristy
# 1                        FALSE                        FALSE
# attributes.Ambience.hipster attributes.Ambience.divey
# 1                       FALSE                      TRUE
# attributes.Ambience.classy attributes.Ambience.trendy
# 1                      FALSE                      FALSE
# attributes.Ambience.upscale attributes.Ambience.casual metroArea
# 1                       FALSE                      FALSE     VEGAS
# popPerSqMi review_timing earliestReview open.1       rate
# 1   1615.555    -0.7058391     2005-08-18   3254 0.03318992
# name
# 1 Dino's Lounge

# Creating a df for only business id# _9BqmRowpWfy4wDPXLSDHA to run in ggplots below.
nsb1 <- neg_skew_biz %>% filter(business_id=="_9BqmRowpWfy4wDPXLSDHA")
View(nsb1)

# Histogram by review count
ggplot(nsb1, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +
  ggtitle("Negative Skewed Business # 1  \  
          ID: _9BqmRowpWfy4wDPXLSDHA  \  
          Review skew: -0.705839  \  
          Review count: 108  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(nsb1, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") +
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Negative Skewed Business # 1  \  
          ID: _9BqmRowpWfy4wDPXLSDHA  \  
          Review skew: -0.705839  \  
          Review count: 108  ") + 
  theme(plot.title = element_text( face="bold"))


# This process is now repeated for two other 
# negatively skewed businesses below.

### -----------


# Negative Skew, Business Number 8 (nsb8)
# business id# ygI6Q8ck2qf3Y0iCM2Lkag


predictors_with_rate_of_reviews %>% 
  filter(business_id=="ygI6Q8ck2qf3Y0iCM2Lkag")

nsb8 <- neg_skew_biz %>% filter(business_id=="ygI6Q8ck2qf3Y0iCM2Lkag")
View(nsb8)

# Histogram by review count
ggplot(nsb8, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +
  ggtitle("Negative Skewed Business # 8  \  
          ID: ygI6Q8ck2qf3Y0iCM2Lkag  \  
          Review skew: -1.294117  \  
          Review count: 38  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(nsb8, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") +
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Negative Skewed Business # 8  \  
          ID: ygI6Q8ck2qf3Y0iCM2Lkag  \  
          Review skew: -1.294117  \  
          Review count: 38  ") + 
  theme(plot.title = element_text( face="bold"))


### -----------


# Negative Skew, Business Number 21 (nsb21)
# business id# nXKwzVKJCtIGd4HxXgjdnQ


predictors_with_rate_of_reviews %>% filter(business_id=="nXKwzVKJCtIGd4HxXgjdnQ")

nsb21 <- neg_skew_biz %>% filter(business_id=="nXKwzVKJCtIGd4HxXgjdnQ")
View(nsb21)

# Histogram by review count
ggplot(nsb21, aes(x=review_date)) + 
  geom_histogram(aes(y=..count..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  xlab("Review  Date") + 
  ylab("Review  Count") +
  ggtitle("Negative Skewed Business # 21  \ 
          ID: nXKwzVKJCtIGd4HxXgjdnQ  \  
          Review skew: -0.4556506  \  
          Review count: 94  ") + 
  theme(plot.title = element_text( face="bold"))


# Histogram overlaid with density curve
ggplot(nsb21, aes(x=review_date)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth=30, colour="black", fill="steelblue1") + 
  geom_density(alpha=.5, fill="red") +
  xlab("Review  Date") + 
  ylab("Review  Density") +
  ggtitle("Negative Skewed Business # 21  \  
          ID: nXKwzVKJCtIGd4HxXgjdnQ  \  
          Review skew: -0.4556506  \  
          Review count: 94  ") + 
  theme(plot.title = element_text( face="bold"))



