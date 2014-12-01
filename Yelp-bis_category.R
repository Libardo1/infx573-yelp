url <- "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"

con = file(url, "r")

input <- readLines(con, -1L)

my_results <- lapply(X=input,fromJSON)


b_categories <- business %>%
  select(categories) %>%
  paste(",")


splitter <- function(x) {
  x <- unlist(strsplit(x, ","))
  out <- data.frame(x[1], matrix(as.numeric(x[-1]), ncol = 2))
  colnames(out) <- c("Name", "Start", "End")
  return(out)
}
b_cats <- strsplit(as.character(b_categories,","))

cats <- as.character(b_categories$categories)
cats <- gsub("(\\[|\\])", "", cats)
cats <- strsplit(cats, ",")
cats <- t(sapply(cats, function(x)
  c(x, rep(NA, maxLen - length(x)))
  ))
data.frame(cats)
