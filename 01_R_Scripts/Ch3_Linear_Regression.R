# Chapter 3: Linear Regression - Lab

# Install/call the required libraries
# This will install if not already installed, and the call the library
if (!require(MASS)) install.packages("MASS") 
if (!require(ISLR2)) install.packages("ISLR2") 
if (!require(dplyr)) install.packages("dplyr") 
if (!require(car)) install.packages("car") 


# ---- 3.6.2 Simple Linear Regression ----

# Always a good idea to check out your data set
# This data set is apart of the ISLR2 package
head(Boston)

# A 'tibble' is a friendlier formatted data set in R
BostonTib <- as_tibble(Boston)
BostonTib

# Fit a simple linear regression model and print the model
# the form is lm(y ~ x, data)
lmFit <- lm(medv~lstat, data = BostonTib)
lmFit

# summary() provides more details
summary(lmFit)

# What else is stored in our object 'lmFit'
names(lmFit)

# We can extract just the coefficients if we like
coef(lmFit)

# We can also obtain confidence intervals for the coefficient estimates
# Default is to provide 95% confidence intervals, but we can modify the level =
confint(lmFit)
confint(lmFit, level = 0.99)

# We can use our model to produce confidence intervals and prediction intervals for y given new values of x
predict(lmFit, data.frame(lstat = c(5,10,15)), interval = 'confidence') # confidence interval
predict(lmFit, data.frame(lstat = (c(5, 10, 15))),interval = "prediction") # prediction interval

# Now we want to plot our x and y variables with our least squares regression line
plot(BostonTib$lstat, BostonTib$medv)
abline(lmFit) # add the least squares line
abline(lmFit, col = 'red') # we can change the color
abline(lmFit, col = 'red', lwd = 3) # we can change the color and size
plot(BostonTib$lstat, BostonTib$medv, pch = 8) # we can change the point type too

# We can plot some model diagnostics as well
# par(mfrow = c(rows, columns)) helps us plot multiple plots at the same time
par(mfrow = c(2,2)) # this will create a 2 x 2 grid (4 total plots)
plot(lmFit)

# We can plot just the residuals vs predicted values
# As well as the studentized residuals
plot(predict(lmFit), residuals(lmFit))
plot(predict(lmFit), rstudent(lmFit))

# We observe some non-linearity. We can compute some leverage statistics 
plot(hatvalues(lmFit))
which.max(hatvalues(lmFit))

# Reset your plot window to the standard
dev.off()

# ---- 3.6.3 Multiple Linear Regression ----

# Fit a multiple linear regression model using the same function and summarize the model
# the form is lm(y ~ x1 + x2 + x3, data)
multFit <- lm(medv ~ lstat + age, data = BostonTib)
summary(multFit)

# What if we want to use all variables?
# We can see that there are 13 variables, too many to type
BostonTib

# We can use the following short cut
allFit <- lm(medv ~ ., data = BostonTib)
summary(allFit)

# We can access individual components of the summary object by name
# For example the R^2 value
summary(allFit)$r.sq

# We installed the car package earlier so we have access to 
# functions that compute variance inflation factors, VIFs
vif(allFit)

# Let's say we want to remove 'age' from our model, we can still use our short cut
lmFit1 <- lm(medv ~ . - age, data = BostonTib)
summary(lmFit1)

# ---- 3.6.4 Interaction Terms ----

# It's easy to include interaction terms using the syntax lstat:age
# This tells R to include an intercation term between the lstat and age variables
summary(lm(medv ~ lstat + age + lstat:age, data = BostonTib))

# Or we can use the syntax lstat*age to simultaneously include lstat, age, and their interaction
summary(lm(medv ~ lstat*age, data = BostonTib))

# ---- 3.6.5 Non-Linear Transformations of the Predictors ----

# The lm() function also allows non-linear transformations
# For example if we have x and want to include x^2, we can include I(x^2)
# We need to include I() since ^ has special meaning in a formula object like lm()
lmFitSq <- lm(medv ~ lstat + I(lstat^2), data = BostonTib)
summary(lmFitSq)

# The near-zero pvalue indicates a better fit than before
# We can use anova() to further quantify this improvement introduced from the quadratic model
# The anova() function tests the null hypothesis that both models fit equally well
# against the alternative hypothesis that the full model is superior
anova(lmFit, lmFitSq)

# It's not surprising the quadratic model fits better given the evidence 
# for non-linearity we saw earlier
par(mfrow = c(2,2))
plot(lmFitSq)

# We can fit any order of polynomial using the poly() function
lmFitPoly <- lm(medv ~ poly(lstat, 5), data = BostonTib)
summary(lmFitPoly)

# ...but we are also able to fit other transformations like log()
lmFitLog <- lm(medv ~ log(rm), data = BostonTib)
summary(lmFitLog)

# ---- 3.6.6 Qualitative Predictors ----

# We can also look at the Carseats data set that has qualitative predtictors
# like the variable ShelveLoc that contains shelf location
carseatsTib <- as_tibble(Carseats)
carseatsTib

# We fit the model the same way. When R encounters the qualitative variable 
# it generates dummy variables. seen in the summary
lmFitQual <- lm(Sales ~. + Income:Advertising + Price:Age, data = carseatsTib)
summary(lmFitQual)

# We can view the coding that R using to generate these
contrasts(carseatsTib$ShelveLoc)

# ShelveLocGood is 1 when ShelveLoc = 'Good' otherwise it's 0
# ShelveLocMedium is 1 when ShelveLoc = 'Medium' otherwise it's 0
# a bad location is 0 for both dummy variables
# ShelveLocGood is positive coefficient which indicates a good location results in higher sales
# ShelveLocMedium is a smaller positive coefficient which indicates it's associated with higher sales
# than a bad location but lower sales than a good location

# ---- ** BONUS ** Plotting Linear Models in GGPlot ----
if (!require(ggplot2)) install.packages("ggplot2")

# Can plot the data AND the model without fitting the model explicitly
ggplot(BostonTib, aes(x = lstat, y = medv))+
  geom_point()+
  geom_smooth(method = 'lm')

# But to give more control, you can fit the model first and then plot
lmGGFit <- lm(medv ~ lstat, data = BostonTib)

# Add the confidence interval to the orignal data set using cbind()
plotData <- cbind(BostonTib, predict(lmGGFit, interval = 'confidence'))

# Plotting it all together
ggplot(plotData, aes(x = lstat, y = medv))+
  geom_point()+
  geom_line(aes(x = lstat, y = fit), color = 'blue', size = 1)+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)+
  theme_bw()+
  labs(title = 'GGPlot of our Linear Model', 
       y = 'Median value of Homes', 
       x = 'Lower Status of Population')

# We can also recreate the residuals vs fitted plot
ggplot(lmGGFit, aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(title = 'Residual vs. Fitted Values Plot',
       y = 'Residuals',
       x = 'Fitted Values')

# As well as the Normal Q-Q plot
ggplot(lmGGFit, aes(sample = .stdresid))+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  labs(title = 'Normal Q-Q',
       y = 'Standadrized Residuals',
       x = 'Theoretical Quantitles')
