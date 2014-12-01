###Setting my working directory
setwd("~/R")

#### Make sure to install jsonlite install.packages("jsonlite") ####
library("jsonlite")


#### import json data from yelp ####
business = fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"),
  collapse=","),"]"), flatten=TRUE)

business_trimmed <- business[,c('business_id', 'city',
                                'review_count','name',
                                'longitude',
                                'state', 'stars', 'latitude',
                                'type')]
summary(business_trimmed$review_count)
dim(business_trimmed)
names(business)

#histogram of review counts
library(ggvis)

business_trimmed %>% ggvis(~review_count, fill := "#fff8dc") %>%
                    layer_histograms(width = 20) %>%
                    add_axis("x", title = "Number of reviews") %>%
                    add_axis("y", title = "Number of businesses")

# exploring how the number of reviews varies by business
library(plyr)
library(dplyr)
library(ggplot2)
bus_rev_num <- ddply(business_trimmed, c("review_count"), 
                                  summarise, number = length(review_count))

review_count_decile <- quantile(business$review_count,  
                              prob = seq(0, 1, length = 11), type = 5)


review_count_percentile <- quantile(business$review_count,  
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
# ok so it looks like there's one place in scotland (edinburgh)
# and the Waterloo data are from Ontario!

# lets focus on arizona by pulling just the records of that state
library(ggplot2)
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona"))
business_arizona <- subset(business_trimmed, state=="AZ")
large_phoenix <- subset(business_arizona, review_count > 1000)
# also pull out nevada
states <- subset(all_states, region %in% c( "nevada"))
business_nevada <- subset(business_trimmed, state=="NV")
large_nevada <- subset(business_nevada, review_count > 2000)
# also get wisconson
states <- subset(all_states, region %in% c( "wisconsin"))
business_wisconsin <- subset(business_trimmed, state=="WI")
large_wisconsin <- subset(business_nevada, review_count > 2000)
# how about US
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona", "nevada", "wisconsin"))
business_us <- subset(business_trimmed, state==c("AZ", "NV", "WI"))


# now we make a map of arizona
p <- ggplot()
p <- p + geom_polygon( 
  data = states, 
  aes(x=long, y=lat, group = group),
  color="white",
  fill = "grey")
p
# and fill the map with a scaterplot
p <- p + geom_point( 
  data = business_arizona, aes(
    x=longitude, y=latitude, color = review_count)
) + scale_colour_gradientn("review count",
                           colours = c("#f9f3c2","#660000"))
p

# lets try to zoom in at the county level
all_counties <- map_data("county")
maricopa <- subset(all_counties, region %in% c("maricopa"))
m <- ggplot()
m <- m + geom_polygon( 
  data = maricopa,  
  aes(x=long, y=lat, group = group),
  color="white",
  fill = "grey")
m <- m + geom_point( 
  data = business_arizona, aes(
    x=longitude, y=latitude, color = review_count)
) + scale_colour_gradientn("review count",
                           colours = c("#f9f3c2","#660000"))
m + ggtitle("Yelp reviews in Maricopa county, AZ")

# install.packages(ggmap)
library(ggmap)
# use ggmap to get a map
phoenix_map <- qmplot(longitude, latitude, data = large_phoenix, zoom = 10, 
                      color = review_count, size = review_count, 
                      source = "google", maptype = "toner", extent = "device")
phoenix_map + ggtitle("Yelp reviews in Phoenix, AZ")
vegas_map <- qmplot(longitude, latitude, data = large_nevada, zoom = 13,
                    color = review_count, size = review_count, 
                    source = "google", mapcolor = "color", extent = "device")
vegas_map + ggtitle("Yelp reviews in Las Vegas, NV")



# shapefiles from census tracts
# install.packages(rgeos)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(rworldmap)
library(ggmap)
library(qmap)

blockgroup_az <- readShapeSpatial("shapefiles_bg/cb_2013_04_bg_500k.shp")
blockgroup_nv <- readShapeSpatial("shapefiles_bg/cb_2013_32_bg_500k.shp")
blockgroup_az <- fortify(blockgroup_az) # convert shapefile to dataframe
blockgroup_nv <- fortify(blockgroup_nv)

q <- qmap('las vegas', zoom = 13, maptype = 'satellite') +
  geom_polygon(aes(x = long, y = lat, group = group), data = blockgroup_nv,
               colour = 'white', fill = 'black', alpha = .4, size = .3)
  
q
la <- ggplot() + 
  geom_point(data = large_nevada, aes(longitude, latitude, color = review_count, size = review_count)) 
la + ggtitle("Yelp reviews in Las Vegas, NV")

pop.maricopa <- read.csv("aff_maricopa/ACS_12_5YR_S0101_with_ann.csv")
sPDF <- joinData2Map(pov,nameMap='phoenix_map',nameJoinIDMap='VARNAME_1',nameJoinColumnData='Id1')
pop.clark <- read.csv("aff_clark/ACS_12_5YR_S0101_with_ann.csv")


#### export data frames to csv ####
write.csv(business_arizona, file = "business_arizona.csv")
write.csv(business_nevada, file = "business_nevada.csv")
write.csv(business_wisconsin, file ="business_wisconin.csv")
write.csv(business_us, file = "business_us.csv")


#### Random Forest ####
## http://r.789695.n4.nabble.com/Re-Fwd-Re-Party-extract-BinaryTree-from-cforest-td3878100.html
library("party") 
predictors <- read.csv("predictors_with_rate_of_reviews.csv")
cf <- cforest(review_count ~ stars, data = predictors) 
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
pt 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
nt 
plot(nt) 



###############
# MAPTREE
# http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
library(maptree)
library(cluster)
draw.tree( clip.rpart (rpart ( raw), best=7),
           nodeinfo=TRUE, units="species",
           cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)

plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)

