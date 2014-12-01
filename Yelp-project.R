#### Make sure to install jsonlite install.packages("jsonlite") ####
library("jsonlite")

#### import json data from yelp ####
# We need to add the flatten the nested fields

chekin_data <- fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"),
  collapse=","),"]"), flatten = TRUE)

business_data <- fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"),
  collapse=","),"]"), flatten = TRUE)

review_data <- fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"),
  collapse=","),"]"), flatten = TRUE)

tip_data <- fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json"),
  collapse=","),"]"), flatten = TRUE)

user_data <- fromJSON(paste("[",paste(readLines(
  "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"),
  collapse=","),"]"), flatten = TRUE)

