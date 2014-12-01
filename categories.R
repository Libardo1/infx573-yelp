write.csv(review_data, file = "reviews.csv")
csv_review <- read.csv("yelp_academic_dataset_review.csv")

s <- data.frame(strsplit(as.character(business_data$categories), ','))
View(s)

df <- data.frame(ID=11:13, FOO=c('a|b','b|c','x|y'))
foo <- data.frame(do.call('rbind', strsplit(as.character(business_data$categories),',')))


