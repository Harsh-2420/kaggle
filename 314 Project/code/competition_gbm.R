corr = cor(data) %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)

View(corr)


# R case competion - UTM, STA314
# Harsh | University of Toronto
# Karan Argawal  | University of Toronto 
# Tahir Muhammad | University of Toronto

# Set the working directory
# setwd("/home/tahir/Desktop/STA314/Competition/")

# Import relevant libraries
library(leaps) # this contains the function regsubsets - This only works for Linear Transformations
library(MASS)
library(ISLR)
library(caret)
library(tidyverse)
library(PerformanceAnalytics)
library(dplyr)
library(splines)
library(gam)
library(devtools)
library(nlcor)
library(gbm)
# Read in the train and test data provided 
data <- read.csv(file = 'trainingdata.csv')
test_data <- read.csv(file = 'test_predictors.csv') # this dataset doesn't contain predictors, not to be touched until the end. 

attach(data)

# Set seed for reproducability
set.seed(1)

# Get the best predictors for our model
nlcor(X23, data$y, plt = T)
nlcor(X54, data$y, plt = T)
nlcor(X90, data$y, plt = T)
nlcor(X47, data$y, plt = T)
nlcor(X50, data$y, plt = T)
nlcor(X44, data$y, plt = T)
nlcor(X95, data$y, plt = T)
nlcor(X85, data$y, plt = T)
nlcor(X9, data$y, plt = T)
nlcor(X25, data$y, plt = T)
nlcor(X61, data$y, plt = T)
nlcor(X27, data$y, plt = T)
nlcor(X48, data$y, plt = T)
# Get a Prediction for the model you think are useful. 

indxTrain <- createDataPartition(y = data$y,p = 0.99,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

#gam <- gam(y ~ ns(X23, df = 5) + ns(X54, df = 3) + s(X4) + s(X56) + s(X58) + s(X62) + s(X66) + X7 + X10 + X9 + X13, data = training)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


xgbTree <- train( y ~ X23 + X54 + X90 +X47+ X50+ X44 +X95+X85+X9+X25+X61+X81+X48,  data = training, 
                  method = "xgbTree", 
                  trControl = fitControl,
                  ## This last option is actually one
                  ## for gbm() that passes through
                  verbose = FALSE)

gbmFit2 <- train( y ~ s(X23, df=3) + s(X54, df=2) + X90 +X47+ X50+ X44 +X95+X85+X9+X25+X61*X27+X70*X79+X81+X48,  data = training, 
                  method = "gbm", 
                  trControl = fitControl,
                  ## This last option is actually one
                  ## for gbm() that passes through
                  verbose = FALSE)

gbmFit3 <- train( y ~ smooth.spline(X23) + X54 + X90 +X47+ X50+ X44 +X95+X85+X9+X25+smooth.spline(X61)+X81+X48,  data = training, 
                  method = "gbm", 
                  trControl = fitControl,
                  ## This last option is actually one
                  ## for gbm() that passes through
                  verbose = FALSE)

gbmFit4 = gbm(y~.,data=training,
                   distribution='gaussian',
                   bag.fraction = 0.5,
                   train.fraction = 1,
                   n.trees = 5000,
                   interaction.depth = 15,
                   cv.folds=5,
                   shrinkage = 0.01)
gbmFit2

prediction <- predict(gbmFit2, newdata = testing)

example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='gbm_spline.csv',row.names=FALSE)






x <- rep(0, times=length(data))
x <- colnames(data)
x <- x[-1];
counter = 1
for(elm in x){
  temp <- as.name(elm)
  #c <- nlcor( temp, y, plt = T)
  plot(temp,y)
  #print(c)
}


empty_vec <- rep(0, times=length(data))
x <- rep(0, times=length(data))
x <- colnames(data)
counter=0
for (elm in x) {   
  counter=counter+1
  c <- (nlcor(elm, y, plt = T))
  c <-c$cor.estimate
  if (c > 0.5){
    empty_vec[counter]<-c
  }
}

for (val in length(data)) {
  
  if(val %% 2 == 0)  count = count+1
}
print(count)
c <- nlcor(X1, y, plt = T)

# Look for Co-relations
# chart.Correlation(data[-1], col=data$y)

# Normalize the data - Making the values between -1 and 1
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization to the dataset
data_norm <- as.data.frame(lapply(data[2:112], min_max_norm))

# Add back the y column
data_norm$y <- data$y

# Split the normalized data into training and test set
train_index <- sample(1:nrow(data_norm), 0.8 * nrow(data_norm))
test_index <- setdiff(1:nrow(data_norm), train_index)
train <- data[train_index, ]
test <- data[test_index, ]








# METHOD 2 - KNN regression after forward subset, choosing best 50 predictors. 

# look at forward stepwise
regfit.forward = regsubsets(data$y~.,data = data,method='forward', nvmax = 50)
# performs forward selection
summary(regfit.forward)

# note: for example for k = 4 we get a different model compared
# to the result from best subset selection.

coef(regfit.forward,10)

set.seed(400)

# data <- data %>%
#select(y, X54, X19, X5, X58, X111, X22, X99)

indxTrain <- createDataPartition(y = data$y,p = 0.80,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

predictors = training$X54+ training$X19 + training$X5 + training$X58
ctrl <- trainControl(method="repeatedcv",repeats = 10) 

knnFit <- train(training$y~ training$X18+ training$X23 + training$27 + training$X44, training$X50, training$X61, trainingX90 data = training, method = "knn", trControl = ctrl, preProcess = c("range"))

knnFit
# Test out data on the test set
test_pred <- predict(knnFit, newdata = test_data)

example_pred = data.frame(cbind(1:6000,test_pred))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='third.csv',row.names=FALSE)

# install.packages('gam')
# install.packages('ISLR')

# example: GAM with natural splines for year and age, both degree 4
# this can be done by simply using the lm() function
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# prepare plotting region
# plot.gam will produce three plots for the three predictors
# note: we need plot.Gam, not just plot
# the gam package should be loaded to use this
par(mfrow=c(1,3))
plot.Gam(gam1, se=TRUE, col="red")

# now use gam package
# example: GAM with smoothing splines for year and age
# use dummy for education
# note: can not use linear model
# now plot works

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")

# outcome not too different from previous model

# example: GAM with local regression for age and smoothing sline for year
# note: can not use linear model

gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.lo, se=TRUE, col="green")

# look at the output using 'summary'
# compare lectures for interpretation
summary(gam.lo)

# next consider 3 different models and use anova to compare
# see lectures for explanation of output
gam.m1=gam(wage ~ s(age,df=5)+education,data=Wage)
gam.m2=gam(wage ~ year +s(age,df=5)+education,data=Wage)
gam.m3=gam(wage ~ s(year,df=4)+s(age,df=5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3)

#-----------------------------------------------------------------------
# Extended ANOVA models
# load Advertising data

# setwd('C:/users/stas/Dropbox/________________STA314-2019') # change to a different directory
Adv = read.csv(file = 'Advertising.csv') # read a file from the working directory
attach(Adv)


# load a package called akima
# will be used for plotting
# install.packages('akima')
library(akima)

gam_adv = gam(sales ~ radio + TV:radio + s(TV,df=5) + newspaper,data = Adv)
summary(gam_adv)

# sumary indicates that Newspaper can be dropped

# now consider a model with 'non-parametric interaction'
# between TV and Radio

gam.np=gam(sales~lo(TV,radio,span=0.7)+newspaper,data=Adv)
par(mfrow=c(1,2))
plot.Gam(gam.np)






# ------------------------- Method 1 / Junk ------------------------------------
tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

lm1 <- train(train$y~., data= train[2:113] , method = "lm")
rf1 <- train(train$y~., data = train, method = "rf")


X <- subset(train, select = -c(y) )

# Set up a 10-fold cross validation
tc <- trainControl(method = "cv", number = 10)
# Include the setup in your model
lm1 <- train(train$y ~ ., data = X, method = "lm",
             trControl = tc) 
lm1

# Use L fold cross validation to find the best K. 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(train$y ~., data = train, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



# Random sample indexes
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

train <- data[train_index, ]
test <- data[test_index, ]


library(caret)

## KNN 
valset_knn = function(X,Y,krange){
  n = length(Y)
  l.tr = floor(n/2)
  l.val = n - l.tr
  train_index = sample(1:n,l.tr) # randomly sample l.tr points from 1,...,n
  # this will correspond to the training set
  
  # create array where results of validation error will be stored
  valset_error = array(0,length(krange))
  
  # loop over values of K, for each store error on validation set
  for(k in 1:length(krange)){
    # only use data with index in train_index to fit the model
    K = krange[k]
    fit = knnreg(as.matrix(x[train_index]),y[train_index],k=K)
    
    # now use fitted model to predict data which are not in train_index
    # recall from lecture 0: a[-b] gives entris of a which are not indexed by b
    pr = predict(fit, newdata = data.frame(x = x[-train_index]))
    
    # compute and store the validation error
    valset_error[k] = mean((y[-train_index] - pr)^2)
  }
  
  return(valset_error) # this will be the output of the function
}

# make reproducible by setting seed
set.seed(1)

# create data
n = 100 # 100 observations
x = (1:n)/n # values for predictors
f = .5*sin(-2+12*x) # true function f
y = f + rnorm(n,mean=0,sd = .5) # add noise

# call function 
valset_knn(X=x,Y=y,krange = 1:50)

# call again, save output in array 
err_vs = valset_knn(X=x,Y=y,krange = 1:50)

# Standardize the data 

# Create a 
# Fit the linear regression to all predictors
x <- lm(y~ ., data=test_data)
# Summarize the results
summary(x) # We can see that some are significant, in regards to other variables which are present.
# Thus, do best subset selection to get better predictors

# note: R has automatically turned qualitative predictors into dummies
# this inly works if originally predictors are coded as strings or factors
#-----------------------------------------------------------------------
# now look at different ways of selecting models
# first: best subset selection
# Non-linearity
regfit_best = regsubsets(y~.,data = train_data, really.big = T)
# performs exhaustive search
x<- summary(regfit_best)

# note: by default only search up to 8 predictors
regfit_best = regsubsets(Balance~.,data = Credit,nvmax = 11)
summary(regfit_best)

# to extract coefficients from one particular model, 
# for example model with 4 predictors
coef(regfit.best,4)

# =======================================================================
# look at forward stepwise
regfit.forward = regsubsets(data$y~.,data = data,method='forward', nvmax = 50)
# performs forward selection
summary(regfit.forward)

# note: for example for k = 4 we get a different model compared
# to the result from best subset selection.

coef(regfit.forward,50)



# Split the training data into train and test set for validation.

mean_pred = rep(mean(data$y),6000)

example_pred = data.frame(cbind(1:6000,mean_pred))
names(example_pred) = c('id','y')

# this will create a .csv file in your working directory
# this is the file you should upload to the competition website
write.csv(example_pred,file='first.csv',row.names=FALSE)
