#### Make sure to install jsonlite install.packages("jsonlite") ####
library("jsonlite")
library(plyr)
library(dplyr)

setwd("~/Desktop")

#### import json data from yelp ####
# We need to add the flatten the nested fields

business_data <- fromJSON(paste("[",paste(readLines(
  "C:/Users/iguest/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"),
  collapse=","),"]"), flatten = TRUE)

review_data <- fromJSON(paste("[",paste(readLines(
  "C:/Users/iguest/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"),
  collapse=","),"]"), flatten = TRUE)

# 42153 businesses listed, 112.5 k reviews, but only 10 fields in each review.
dim(business_data)
dim(review_data)
View(business_data)
View(review_data)

# looking at review data
summary(review_data)
View(head(review_data))
max(review_data$date)
min(review_data$date)

# repeating old business EDA
View(head(business_data))
business_trimmed <- business_data[,c('business_id', 'city',
                                'review_count','name',
                                'neighborhoods','longitude',
                                'state', 'stars', 'latitude',
                                'type')]
summary(business_trimmed$review_count)
dim(business_trimmed)

# lat long distribution
#can we cluster businesses based on latitude and longitude?
# ie do a scatterplot of lat/long data?

plot(business_trimmed$longitude, business_trimmed$latitude)
# ok so it looks like there's one place in scotland (edinburgh)
# and the Waterloo data are from Ontario!



# City distribution businesses
bus_city_distribution <- ddply(business_trimmed, c("city"), 
                               summarise, number = length(city))

city_dist_sorted <- arrange(bus_city_distribution, desc(number))

View(city_dist_sorted)


# figuring out range of latitudes and longitudes for Madison, Phoenix, and Las Vegas
# this will provide guidance on the census blocks to retrieve
cities <- c("Phoenix","Las Vegas","Madison")
min_lat_est <- c(32,35,42)
max_lat_est <-c(35,37,43)
min_long_est <-c(-114,-117,-90)
max_long_est <-c(-109,-114,-88)

city_latlong = data.frame(cities, min_lat_est, max_lat_est, min_long_est, max_long_est)
View(city_latlong)

for(i in 1:3){
# applying a rough filter to isolate city
biz_filtered <- filter(business_trimmed, 
                       latitude > city_latlong[i,2] 
                       & latitude< city_latlong[i,3]
                       & longitude> city_latlong[i,4]
                       & longitude< city_latlong[i,5])
#getting maximum & minimum lat long for this subset of businesses
city_latlong$maxlat[i] <- max(biz_filtered$latitude)
city_latlong$maxlong[i] <- max(biz_filtered$longitude)
city_latlong$minlat[i] <- min(biz_filtered$latitude)
city_latlong$minlong[i] <- min(biz_filtered$longitude)}

#histogram of review counts
library(ggvis)

business_trimmed %>% ggvis(~review_count, fill := "#fff8dc") %>%
  layer_histograms(width = 20) %>%
  add_axis("x", title = "Number of reviews") %>%
  add_axis("y", title = "Number of businesses")

# exploring how the number of reviews varies by business
library(plyr)
library(dplyr)
bus_rev_num <- ddply(business_trimmed, c("review_count"), 
                     summarise, number = length(review_count))

review_count_decile <- quantile(business_trimmed$review_count,  
                                prob = seq(0, 1, length = 11), type = 5)


review_count_percentile <- quantile(business_trimmed$review_count,  
                                    prob = seq(0, 1, length = 101), type = 5)

# so if we just look at businesses with more than 100 reviews, that's a bit 
# more than 2000 businesses (a couple hundred thousand reviews, however)...

bus_city_distribution <- ddply(business_trimmed, c("city"), 
                               summarise, number = length(city))

city_dist_sorted <- arrange(bus_city_distribution, desc(number))

#try to look at distributions of specific cities?


#can we cluster businesses based on latitude and longitude?
# ie do a scatterplot of lat/long data?
plot(business_trimmed$longitude, business_trimmed$latitude)


# Trying to call an API (may need rjson or rtexttools)
request <- "http://data.fcc.gov/api/block/find?format=json&latitude=28.35975&longitude=-81.421988&showall=true"
res1 <- fromJSON(file=request)
View(res1)


# let's trim business table for exploration of how US businesses with lots of reviews 
# are different from the other ones.
colnames(business_data)
business_trim1 <- filter(business_data,longitude<(-89))
business_trim2 <- business_trim1[,c('business_id', 'open','categories','review_count',
                                'longitude','latitude','stars','attributes.Price Range',
                                'attributes.Ambience.romantic','attributes.Ambience.intimate',                        
                                'attributes.Ambience.touristy','attributes.Ambience.hipster',
                                'attributes.Ambience.divey','attributes.Ambience.classy',
                                   'attributes.Ambience.trendy','attributes.Ambience.upscale',
                                   'attributes.Ambience.casual')]

# trying to split up business categories
# here's me playing around
test_vector <- c("Doctors, Health & Medical","Restaurants","American (Traditional), Restaurants",
                 "Home Services, Heating & Air Conditioning/HVAC",
                 "Hotels & Travel, Bed & Breakfast, Event Planning & Services, Hotels")

split_categories <- strsplit(test_vector[1], ", ")
View(split_categories)
split_categories[[1]][2]
length(split_categories[[1]])

# get the maximum number of business categories
business_trim2$categories
dim(business_trim2)
length(business_trim2$categories[[1]])

max_num_categories <-0
max_row <-0
for(i in 1:38889)
  {
  numcat <- length(business_trim2$categories[[i]])
  if(numcat > max_num_categories) {
    max_num_categories <- numcat 
    max_row <- i
  }
  }

max_num_categories
max_row
business_trim2$categories[[10129]][6]

# try to look at the most common categories of businesses
# Do this for all businesses
unique.cats <- unique(business_data$categories)
View(unique.cats)
unique.cats

# loop through to get the categories
total_catlist = NULL
for(i in 1:38889)
{
  numcat <- length(business_trim2$categories[[i]])
  for(j in 1:numcat){
      catname <- business_trim2$categories[[i]][j]
      total_catlist <- c(total_catlist, catname)
      }
  }
View(total_catlist_df)
total_catlist_df <- data.frame(total_catlist)
cat_count <- ddply(total_catlist_df, c("total_catlist"), 
                     summarise, number = length(total_catlist))
cat_count_sorted <- arrange(cat_count, desc(number))
View(cat_count_sorted)


# Now do this only for businesses that get >100 reviews
View(business_trim2)
business_trim3 <- filter(business_trim2, review_count>99)
View(business_trim3)

total_catlist_100plus = NULL
for(i in 1:2417)
{
  numcat <- length(business_trim3$categories[[i]])
  for(j in 1:numcat){
    catname <- business_trim3$categories[[i]][j]
    total_catlist_100plus <- c(total_catlist_100plus, catname)
  }
}

View(total_catlist_100plus)
total_catlist_100plus_df <- data.frame(total_catlist_100plus)
View(total_catlist_100plus_df)

cat_count_100plus <- ddply(total_catlist_100plus_df, c("total_catlist_100plus"), 
                   summarise, number_100plus = length(total_catlist_100plus))


cat_count_sorted_100plus <- arrange(cat_count_100plus, desc(number_100plus))

cat_count_sorted_100plus <- data.frame(cat_count_sorted_100plus)
names(cat_count_sorted_100plus) <- sub("total_catlist_100plus", "total_catlist", 
                                       names(cat_count_sorted_100plus))

View(cat_count_sorted_100plus)


# Now combine category counts
cat_counts_merged <- merge(cat_count_sorted, cat_count_sorted_100plus,
                             by=c("total_catlist"))
cat_counts_merged_sorted <- arrange(cat_counts_merged,desc(number_100plus))
View(cat_counts_merged_sorted)
