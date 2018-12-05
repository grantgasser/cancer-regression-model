###################### 
# Final Exam
# Grant Gasser
# Fall 2018
#####################

#read data
dat <- read.csv('../Final/cancer.csv', header=T)

#3047 observations (counties), 33 variables (likely too many predictors here)
dim(dat)

#what kind of variables do we have?
str(dat)

#binnedInc and Geography are both categorical/factor variables
#You can also see some NA values, let's address those

#NOTE: each observation has a unique value for Geography because each observation is a different county
#this acts as a categorical variable. Thus, when fitting a model, the function will choose a Y value exactly
#the same as the observed Y value when it sees a particular county (basically overfitting on steroids)

#remove Geography from set and store in variable
geo <- dat$Geography
dat$Geography <- NULL

#Which columns have NA and how many do they have?
NAcol = which(colSums(is.na(dat)) > 0)
sort(colSums(sapply(dat[NAcol], is.na)), decreasing = TRUE)

#the lm function removes observations where there is NA
#with 3047 observations, there are too many NA values in PctSomeCol18_24
#and PctPrivateCoverageAlone

#Since we have over 30 variables, it does not seem too reckless to these 3 variables
#Time permitting, come back and fit a model including these variables and their NA's
dat <- dat[,-NAcol]

#before fitting a model, it is worth checking for linearity among the variables
#takes a couple minutes
'''
library(GGally)
library(reshape2)
dat.num <- dat
dat.num$binnedInc <- NULL
ggpairs(dat.num)
'''

#there are too many variables to check for linearity right now, will look after
#doing model selection

#We are trying to predict deathRate (the "response" variable), set as Y
Y <- dat$deathRate
dat$deathRate <- NULL

#Lets first try to fit a multiple linear regression model
fit <- lm(Y ~ ., data=dat)
summary(fit)

#Adj R^2 seems a bit low at .52, but hard to interpret, context dependent
#We can see with a low p-val and a very high F-stat that this model is significant

#Will check for multicollinearity (variables with high VIF (> 10) are redundant, the information they have
#is captured in other variables)
library(car)
vif(fit)

#Remove avgDeathsPerYear, highest at 31.6
fit <- lm(Y ~ . -avgDeathsPerYear, data=dat)
vif(fit)

#Remove PctPublicCoverage, highest at 23.8
fit <- lm(Y ~ . -avgDeathsPerYear -PctPublicCoverage, data=dat)
vif(fit)

#binnedInc is the highest, but it is a factor/categorical variable so that is OK
#Remove medIncome at 15.2
fit <- lm(Y ~ . -avgDeathsPerYear -PctPublicCoverage -medIncome, data=dat)
vif(fit)

#Remove PctPrivateCoverage at 12.8
fit <- lm(Y ~ . -avgDeathsPerYear -PctPublicCoverage -medIncome -PctPrivateCoverage, data=dat)
vif(fit)

#Everything now below 10, so removed the severe multicollinearity, will rely on new methods
#to remove more variables

#We will use the Lasso method to remove variables and select a model

#preprocess data for glmnet function
dat$binnedInc <- as.factor(dat$binnedInc)
dat.lasso <-  model.matrix(Y~.,data=dat)
dim(dat.lasso)

#Fit a linear lasso model
fit.lasso <-  cv.glmnet(x=dat.lasso,y=Y,alpha=1,nfolds=5)

#Lasso shows that a lambda just below 0 and 1SD from mean has low MSE and just 17 vars
plot(fit.lasso)

#Look at the variables Lasso gives
coef(fit.lasso, s='lambda.1se', exact=T)

#PctPrivateCoverage had multicollinearity with other vars so we will not use that, otherwise
#we will use the remaining variables
vars <- which(abs(coef(fit.lasso, s='lambda.1se', exact=T)[,1])>0)
vars





