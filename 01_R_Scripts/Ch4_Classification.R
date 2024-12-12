# Chapter 4: Classification - Lab

# Install/call the required libraries
# This will install if not already installed, and the call the library
if (!require(ISLR2)) install.packages("ISLR2") 
if (!require(MASS)) install.packages("MASS")
if (!require(class)) install.packages("class")

# ---- 4.7.1 The Stock Market Data ----

# 1250 days of of the S&P 500 index with the previous 5 days
# our goal is to predict the direction
stockTib <- as_tibble(Smarket)
stockTib
summary(stockTib)

# Can print the correlation matrix (minus Direction because it's not numeric)
# as well as a matrix of scatterplots
pairs(stockTib)
cor(stockTib[,!(names(stockTib) %in% 'Direction')])

# The only substantial correlation appears between Year and Volume, which we see increases over time
plot(stockTib$Volume)

# ---- 4.7.2 Logistic Regression ----

# We'll fit a logistic regression model using glm() and tell it family = 'binomial'
# So it knows to run logistic regression
glmFit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = stockTib, family = binomial)
summary(glmFit)

# the smallest pvalue is for Lag1. the negative coefficient suggests that if that market 
# had a positive return yesterday, then it is less likely to go up today
coef(glmFit)
summary(glmFit)$coef # get some additonal details
summary(glmFit)$coef[,'Pr(>|z|)'] # extract just the pvalues by column name
summary(glmFit)$coef[,4] # extract just the pvalues by column position

# The predict() function can be used to predict the probability of the direction
# given the predictors. The type = 'response' tells R to output the probabilities of P(Y = 1| X)
# We'll print just the first 10
glmProbs <- predict(glmFit, type = 'response')
glmProbs[1:10]

# and we can check the coding of 'Direction' so we know what Y = 1 corresponds to
contrasts(stockTib$Direction)

# Convert these probabilities to their predicted value
glmPred <- ifelse(glmProbs < 0.5, 'Down','Up')
glmPred

# and we can check how well our predictions did
table(glmPred, stockTib$Direction)
(145+507)/1250
mean(glmPred == stockTib$Direction)

# This tells us the 52.2% of the time our model was correct, so our error rate was 47.8%
# however this is the training error rate, which is often optimistic
# to get a better idea of the real error rate we'll split our data into test and training sets

# We'll use 2005 as our test set and 2001-2004 as our training set (20:80) split
train <-(stockTib$Year < 2005)
stock2005 <- stockTib[!train,]


# Fit a model just on our training set
glmFitTrain <- glm( Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                    data = stockTib, family = binomial, subset = train)

# Now using this trained model, we'll test it on the 2005 data
glmProbs2005 <- predict(glmFitTrain, stock2005, type = 'response')
glmPred2005 <- ifelse(glmProbs2005 < 0.5, 'Down', 'Up')
table(glmPred2005, stock2005$Direction)
mean(glmPred2005 == stock2005$Direction)
mean(glmPred2005 != stock2005$Direction)

# We can try and improve performance by removing variables that had high pvalues
glmFitTrainv2 <- glm( Direction ~ Lag1 + Lag2,
                    data = stockTib, family = binomial, subset = train)

# Now using this n3ew trained model, we'll test it on the 2005 data
glmProbs2005v2 <- predict(glmFitTrainv2, stock2005, type = 'response')
glmPred2005v2 <- ifelse(glmProbs2005v2 < 0.5, 'Down', 'Up')
table(glmPred2005v2, stock2005$Direction)
mean(glmPred2005v2 == stock2005$Direction)

# Suppose we want to predict the returns associated with specific values of Lag1 and Lag2
predict(glmFitTrainv2, newdata = data.frame(Lag1 = c(1.2,1.5),
                                             Lag2 = c(1.1, -0.8)),
        type = 'response')

# ---- 4.7.3 Linear Discriminant Analysis ----

# Use the lda() function to fit a LDA model
# if LD1*Lag1+LD1*Lag2 is large then the LDA classifier will predict Up (Y = 1)
ldaFitTrain <- lda(Direction ~ Lag1 + Lag2, 
                   data = stockTib, subset = train)

ldaFitTrain

# The plot is of the linear discriminant obtain by the LD1 values multiplied by the observed values
plot(ldaFitTrain)

# predict() returns 3 values, but 'class' is the column that tells us the predicted class
ldaPred <- predict(ldaFitTrain, stock2005)
table(ldaPred$class, stock2005$Direction)
mean(ldaPred$class == stock2005$Direction)

# Apply a 50% threshold to the posterior probabilities we can recreate the predictions in 'class'
sum(ldaPred$posterior[,1] >= .5) # Down
sum(ldaPred$posterior[,1] < .5) # Up

# We can use any threshold. For instance we could use 90% if we wish to predict
# a decrease only if we are very certain
sum(ldaPred$posterior[,1] > 0.9)

# ---- 4.7.4 Quadratic Discriminant Analysis ----

# QDA is going to follow a similar process to LDA
qdaFit <- qda(Direction ~ Lag1 + Lag2,
              data = stockTib, subset = train)
qdaFit

# Predict and check classification rate
qdaClass <- predict(qdaFit, stock2005)$class
table(qdaClass, stock2005$Direction)
mean(qdaClass == stock2005$Direction)

# ---- 4.7.6 K-Nearest neighbors

# Create train and test set of just our predictors
trainX <- stockTib[train, c('Lag1','Lag2')]
trainY <- stockTib[train, 'Direction']
testX <- stockTib[!train, c('Lag1', 'Lag2')]

# We can 'set the seed' so any random results can be replicated
# try it first with k = 1
set.seed(1) 
knnPred <- knn(trainX, testX, trainY$Direction, k = 1)

table(knnPred, stock2005$Direction)
mean(knnPred == stock2005$Direction)

# What about k = 3?
knnPred3 <- knn(trainX, testX, trainY$Direction, k = 3)

table(knnPred3, stock2005$Direction)
mean(knnPred3 == stock2005$Direction)

# Not very impressive. We'll try with a different dataset
caravanTib <- as_tibble(Caravan)
caravanTib

summary(caravanTib$Purchase)

# Because the scale of the data is important for K-NN we will scale data to mean 0 std 1
# Scale all variables except our categorical response
stdX <- scale(caravanTib[,!(names(caravanTib) %in% 'Purchase')])

# Check to see it working
var(caravanTib[,1])
var(stdX[,1])

# We'll put our first 1000 observations into a test set
test <- 1:1000
trainX <- stdX[-test,]
testX <- stdX[test,]
trainY <- caravanTib[-test, 'Purchase']
testY <- caravanTib[test, 'Purchase']

# Set the seed and predict
set.seed(1)
knnPred <- knn(trainX, testX, trainY$Purchase, k =1)
mean(testY$Purchase == knnPred)

# try k = 3
knnPred3 <- knn(trainX, testX, trainY$Purchase, k = 3)
mean(testY$Purchase == knnPred3)
