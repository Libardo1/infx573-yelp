<<<<<<< HEAD
#### work the data ####
install.packages("maps")
install.packages("rworldmaps")
library(ggvis)

business_data %>% ggvis(~review_count, fill := "#fff8dc") %>%
  layer_histograms(width = 20) %>%
  add_axis("x", title = "Number of reviews") %>%
  add_axis("y", title = "Number of businesses")

library(rworldmap)
library(maps)
library(ggplot2)
head(business_data)
newmap <- getMap(resolution ="low")
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona"))
business_arizona <- subset(business_data, state=="AZ")

states <- all_states
p <- ggplot()

# make a map of a state
p <- p + geom_polygon( 
  data = states, 
  aes(x=long, y=lat, group = group),
  color="white",
  fill = "grey")
p

# add plots to the map
p <- p + geom_point( 
  data = business_arizona, aes(
    x=longitude, y=latitude, color = review_count)
  ) + scale_colour_gradientn("review count",
                             colours = c("#f9f3c2","#660000"))
p

# ===============================================================
# lets focus on arizona by pulling just the records of that state
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona"))
business_arizona <- subset(business_trimmed, state=="AZ")

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
=======
#### work the data ####
library(ggvis)

business_data %>% ggvis(~review_count, fill := "#fff8dc") %>%
  layer_histograms(width = 20) %>%
  add_axis("x", title = "Number of reviews") %>%
  add_axis("y", title = "Number of businesses")

library(rworldmap)
library(maps)
library(ggplot2)
head(business_data)
newmap <- getMap(resolution ="low")
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "arizona"))
business_arizona <- subset(business_data, state=="AZ")

states <- all_states
p <- ggplot()

# make a map of a state
p <- p + geom_polygon( 
  data = states, 
  aes(x=long, y=lat, group = group),
  color="white",
  fill = "grey")
p

# add plots to the map
p <- p + geom_point( 
  data = business_arizona, aes(
    x=longitude, y=latitude, color = review_count)
  ) + scale_colour_gradientn("review count",
                             colours = c("#f9f3c2","#660000"))
p




>>>>>>> a76433c227709011dbeaddacf1f59598ec76988b
