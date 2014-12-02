#### citations ####
# http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
# adapted from http://rpubs.com/minma/cart_with_rpart

#### import predictor data ####
predictors <- read.csv("predictors_with_rate_of_reviews.csv")


#### Using rpart to generate classification and regression trees ####
library(rpart)
# create a variable to predict if a business will get above or below 100 reviews
predictors$limiter <- as.factor(ifelse(predictors$review_count > 100, c(1), c(0)))

# or maybe try with 1000 as the cutoff
# predictors$limiter <- as.factor(ifelse(predictors$review_count > 1000, c(1), c(0)))

# tidy the dataset, get rid of 1:1 predictors like name and review_count
raw = subset(predictors, select=c(open, stars:review_timing, rate, limiter))
raw = na.omit(raw) # remove rows with missing data

frmla = limiter ~ . # create a model
set.seed(123) # set a small seed
# set a small cp (complexity parameter)
fit = rpart(frmla, method="class", data=raw, control = rpart.control(cp = 0.0001))

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]

# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# confusion matrix (training data)
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# plot tree
plot(fit.pruned, uniform=TRUE, main="Classification Tree for Yelp Reviews")
text(fit.pruned, use.n=TRUE, all=TRUE, cex=.8)
# alternative plot
library(rattle)					   # Fancy tree plot
library(rpart.plot)				 # Enhanced tree plots
library(RColorBrewer)			 # Color selection for fancy tree plot
library(party)
library(partykit)
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1) 
# faclen = 0 for full lenght attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap
fancyRpartPlot(fit.pruned) 



#### Party ####
## http://r.789695.n4.nabble.com/Re-Fwd-Re-Party-extract-BinaryTree-from-cforest-td3878100.html
library(party)    			   # Alternative decision tree algorithm
library(partykit)				   # Convert rpart object to BinaryTree
cf <- cforest(frmla, data = raw) 
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
pt 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
nt 
plot(nt) 
prp(nt)


#### import predictor data for Category ####

predictors <- read.csv("categoryPredictors.csv")


#### Using rpart to generate classification and regression trees ####
library(rpart)
# create a variable to predict if a business will get above or below 100 reviews
predictors$limiter <- as.factor(ifelse(predictors$review_count > 100, c(1), c(0)))

# or maybe try with 1000 as the cutoff
# predictors$limiter <- as.factor(ifelse(predictors$review_count > 1000, c(1), c(0)))

# tidy the dataset, get rid of 1:1 predictors like name and review_count
raw = subset(predictors, select=c(Restaurants:limiter))
raw = na.omit(raw) # remove rows with missing data

# convert category predictors to factors
raw <- sapply(raw, as.factor )
raw <- as.data.frame(raw)

frmla = limiter ~ . # create a model
set.seed(123) # set a small seed
# set a small cp (complexity parameter)
fit = rpart(frmla, method="class", data=raw, control = rpart.control(cp = 0.0001))

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# find the best cp from our initial tree
bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]

# Prune Tree using best cp
fit.pruned <- prune(fit, cp = bestcp)

# confusion matrix (training data)
conf.matrix <- table(raw$limiter, predict(fit.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# plot tree
plot(fit.pruned, uniform=TRUE, main="Classification Tree for Yelp Reviews")
text(fit.pruned, use.n=TRUE, all=TRUE, cex=.8)

prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1) 
# faclen = 0 for full lenght attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap
fancyRpartPlot(fit.pruned) 
