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

#we will check to see if a linear model will be good
plot(fit, which=1)

#constant variance and linearity looks good here, although there is a weird outlier
#on the right of the plot that throws off the red line


### Multicollinearity and Model Selection ###
#If variables are multicollinear or dependent in any way, we want to removeone of them.
#We want to have fewer variables in the model to have more degrees of freedom and prevent
#overfitting

#Will check for multicollinearity (variables with high VIF (> 10) are redundant, the information they have
#is captured in other variables)
library(car)
vif(fit)

#Remove avgDeathsPerYear, highest at 31.6
fit <- lm(Y ~ . -avgDeathsPerYear, data=dat)
vif(fit)

#avgDeathsPerYear was multicollinear with popEst2015 (went from 26 to 8)

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
library(glmnet)
fit.lasso <-  cv.glmnet(x=dat.lasso,y=Y,alpha=1,nfolds=5)

#Lasso shows that a lambda just below 0 and 1SD from mean has low MSE and just 17 vars
plot(fit.lasso)

#Look at the variables Lasso gives
#NOTE: they sometimes change each time the lasso model is fit
coef(fit.lasso, s='lambda.1se', exact=T)

#PctPrivateCoverage had multicollinearity with other vars so we will not use that, otherwise
#we will use the remaining variables

#Fit a linear model with the variables provided by Lasso
fit.final <- lm(Y ~ incidenceRate + povertyPercent + binnedInc + MedianAgeMale + PctHS18_24
                        + PctBachDeg25_Over + PctUnemployed16_Over + PctPublicCoverageAlone + PctOtherRace
                        + PctMarriedHouseholds + BirthRate, data=dat)
summary(fit.final)

#all good, povertyPercent is a bit high; that info is likely captured in binnedInc or the variables relating
#to education levels
vif(fit.final)


### OUTLIER ANALYSIS ###
plot(fit.final, which=1) #1221, 1366, 282, and point on right of plot

#Outliers wrp to Y: #1221, 1366, 282, 1497
outlierTest(fit.final)

#Outliers wrp to X:
dat.tmp = data.frame(fitted.values = fit.final$fitted.values, residuals = rstudent(fit.final))

n = nrow(dat.tmp)
p = length(coef(fit.final))

#Outliers wrp to X: many
hii <- hatvalues(fit.final)
names(which(hii>2*p/n))

#Now look at which points have the most influence on the model

#Influential Points
influ <- dffits(fit.final)
names(which(abs(influ) >  2*sqrt(p/n)))

#lots of influential points, look at Cook's distance to see which are most influential

#Cook's Distance: 282 by far, 627, 1059 
#282 outlier wrp to X and Y, 627 and 1059 outliers wrp to X
plot(fit.final, which = 4)

#REMEDIAL MEASURE: We could look further into these data points. If they are erroneous, we could remove them.
#If not, we could use robust regression and weight the outlying cases with weights 
#inversely proportional to the residual

### TESTING ASSUMPTIONS ###
#Linear regression models have the following assumptions that need to be check

#Assuming the residuals or observed errors of the model are normally distributed
qqPlot(rstudent(fit.final)) #looks like they are certainly not

shapiro.test(rstudent(fit.final))

#With shapiro test, we reject H0: that the residuals are normal

#REMEDIAL MEASURE: since we do not have normality, we could try to transform the response variable
#We could also remove outlying cases if the situation allows for that (context-dependent)

#Assuming residuals are independent
library(lmtest)
dwtest(fit.final) 
bgtest(fit.final)

#Both tests: Enough evidence to suggest the residuals are not independently distributed (because of spatial data?)

#REMEDIAL MEASURE: remove outliers (if possible), check if data collect in time, if so -> time series
#likely not indpedent because of spatial data
#this may suggest a linear model is not appropriate or an important predictor variable is missing
plot(geo, Y)

#Assuming residuals have constant variance
bptest(fit.final)

#Enough evidence to suggest residuals do not have constant variance

#REMEDIAL MEASURE: remove outliers (if possible), try weighted least squares, which is is applying more weight
#to observations with less variance and vice versa (inverse relationship)


### PREDICTION ON NEW VALUE ###
newX <- data.frame(incidenceRate=445, povertyPercent=15, binnedInc='(45201, 48021.6]', MedianAgeMale=35, 
                   PctHS18_24=13, PctBachDeg25_Over=16.5, PctUnemployed16_Over=9.5, PctPublicCoverageAlone=18, 
                   PctOtherRace=1.2, PctMarriedHouseholds=42, BirthRate=4.5)
newX

predict(fit.final, newX, interval="prediction", level=.95)

mean(Y)

#The model predicts a deathRate of 171 for this county. 
#The Prediction interval is (168.1, 174.5).

### SUMMARY ###
#Overall, this model is useful. Good is subjective and may not be the right word. The outliers pose a concern
# and are likely the culprits of the violated assumptions. The nonindependence is concerning as well and may have to do
#with the fact that the nature of the data is spatial.

#Of the three variables that are difficult to collect, I would suggest focusing on incidenceRate. avgDeathsPerYear had 
#multicollinearity with other variables and avgAnnCount was removed by Lasso. We can still have a good model with just
#using incidenceRate out of those three.

#The prediction is fairly reliable since it is at the 95% level. There is a 95% probability another observation
#with the same values will be in this range. 5% probability it won't be.


