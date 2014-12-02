

#### Random Forest ####
## http://r.789695.n4.nabble.com/Re-Fwd-Re-Party-extract-BinaryTree-from-cforest-td3878100.html
library(party)  				   # Alternative decision tree algorithm
library(partykit)				   # Convert rpart object to BinaryTree
predictors <- read.csv("predictors_with_rate_of_reviews.csv")
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

#### Training and Testing ####
n.points <- 37334 # number of rows in the dataset
sampling.rate <- 0.8 # sampling rate at 80%
# we need the number of points in the test set to calculate
# the misclassification rate
num.test.set.labels <- n.points * (1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate * n.points,
                   replace=FALSE)
# define the training set to be those rows, #set columns using select
train <- subset(predictors[training, ], select = c(open:name))
# the other rows are going into the test set
testing <- setdiff(1:n.points, training)
# define the test set to be the other rows
test <- subset(predictors[testing, ], select = c(open:name))
# this is the subset of labels for the training set
cl <- predictors$Credit[training]
# subset of labels for the test set, we're withholding these
true.labels <- predictors$Credit[testing]

##################
#### randomForest ####
library(pROC) # Useful for computing and plotting classifer metrics
library(arm) # For small datasets, more stable learning methods
library(randomForest) # R package to fit a random forest model
fit.rf <- randomForest(review_count ~ rate + stars + metroArea, data=train)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, predictors, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}
yhat <- predict(fit.rf, newdata = test, type="response")[]
plot(roc(test$review_count, yhat))
lines(roc(test$review_count, yhat), col='red')

#### rpart ####
library(rpart)
predictors <- read.csv("predictors_with_rate_of_reviews.csv")

# create a variable to predict if a business will get above or below 100 reviews
predictors$limiter <- as.factor(ifelse(predictors$review_count > 100, c(1), c(0)))

# tidy the dataset, get rid of 1:1 predictors like name and review_count
raw = subset(predictors, select=c(open, stars:review_timing, rate, limiter))
raw = na.omit(raw)

frmla = limiter ~ .
# adapted from http://rpubs.com/minma/cart_with_rpart
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
prp(fit.pruned, faclen = 0, cex = 0.8, extra = 1) 
# faclen = 0 for full lenght attribute names
prp(fit.pruned) # run with defaults to try to get the plot to not overlap
fancyRpartPlot(fit.pruned) 
