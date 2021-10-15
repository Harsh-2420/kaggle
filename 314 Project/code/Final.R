# R case competion - UTM, STA314
# Harsh | University of Toronto
# Karan Argawal  | University of Toronto 
# Tahir Muhammad | University of Toronto

# Set the working directory
setwd("/home/tahir/Desktop/STA314/Competition/")

# Import relevant libraries
library(leaps) # this contains the function regsubsets - This only works for Linear Transformations
library(MASS)
library(ISLR)
library(caret)
library(PerformanceAnalytics)
library(dplyr)
library(splines)
library(gam)
library(nlcor)
library(randomForest)
library(mgcv)

# Read in the train and test data provided 
data <- read.csv(file = 'trainingdata.csv')
test_data <- read.csv(file = 'test_predictors.csv') # this dataset doesn't contain predictors, not to be touched until the end. 


setwd("/Users/harshjhunjhunwala/Desktop/STA314")
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
nlcor(X50, data$y, plt = T)
nlcor(X47, data$y, plt = T)
nlcor(X95, data$y, plt = T)
nlcor(X85, data$y, plt = T)
nlcor(X9, data$y, plt = T)
nlcor(X25, data$y, plt = T)
nlcor(X61, data$y, plt = T)
nlcor(X48, data$y, plt = T)


# Get a Prediction for the model you think are useful. 
# x19,20, 26, 29, 31, 35 - categorical
indxTrain <- createDataPartition(y = data$y,p = 0.99,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]
ytest = y[-indxTrain]

# Look for interactions) 

# ALL Predictors
lm.fit=lm(y ~ (X1 + X2 +X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112)^2,data=training)
anova(lm.fit)

# Predictors we know are good From GBM
Good_predictors <- X23 + X54 + X50 + X90 + X47 + X95 + X44 + X85 + X25 + X9 + X48 + X61 + X87 + X73 + X68 + X49 + X107 + X55 + X96 + X18 + X43 + X70 + X28 + X92 + X101 + X78 + X92 + X101 + X78 + X112 + X69 + X12 + X39 + X104 + X63 + X1 + X98 +X81 + X16 +X27 + X13 + X102 + X11
Good_predictors2 <- X30 + X80 + X41 + X51 + X72 + X82 + X46 + X93 + X8 + X67 + X84 + X6
# Interactions between Good Predictors
lm.fit=lm(y ~ ( X26 + X68 + X49 + X107 + X55 + X27 + X13 + X102 + X11+X30 + X80 + X41 )^,data=training)
anova(lm.fit)

# Possible Good Interactions
X23:X49 # 2 stars
X50:X55 # 2 stars
X50:X73 # 2 stars
x <- X50:X90:X104 # 1 star
# X83:X108     1    91.3   91.34  4.0985   0.04311 *  

gamFinal <- gam(y ~ s(X23) + s(X54) +X56 + lo(X26)+  lo(X54,X73) + lo(X37,X112) + lo(X54,X95,span=0.8)+ lo(X50,X73,span=0.7) + lo(X50,X90,X104, span=0.75) + lo(X50, X95,X94,span=0.85) +
                  lo(X56,X16,span=0.7) + X50 + s(X90) +s(X47) + s(X95) +s(X44) +s(X85) +s(X25) + X95 + s(X44) + s(X9) +s(X48) + s(X61) + s(X87) + s(X73) + s(log(X112+1))+ s(X92) + 
                  lo(X101,X53,0.3) + lo(X101,X28) + X78 + X92 + X101 + X78 + X112 + X69 + X12 + X39 + 
                  X104 + X63+ X98 +X81 + X16 +X27 + s(X13) + X30 + X80 + X41 + s(X46) + s(X93) +s(X81), data=training, method = "REML")
summary(gamFinal)
prediction <- predict(gamFinal, newdata = testing) 
mean((prediction-ytest)^2) 




-example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')
 For submission 
write.csv(example_pred,file='Final.csv',row.names=FALSE)



# lo0 = loess(y~X79:X70, data=data)
# summary(lo0)
gam0 = gam(y~X10+s(X23, by=X48)+s(X1, by=X31)+X44+X47+X50+s(X54)+X61+X90+X95+s(X2, by=X5)+X61:X27, data=training, method="REML")
summary(gam0)

prediction <- predict(gam0, newdata = testing) 
mean((prediction-ytest)^2) 

# X55 is BAD
# X95 +X44 +X85 +X25+ X9 +X48 + X61 + X87 + X73 + X68 + X49 + X107+ X55 + X96 + X18 + X43 + X70 + X28+ X9 + X101 + X78
gam1 = gam(y~ s(X23) + s(X54)+ s(X50) + s(X90) +s(X47) + s(X95) +s(X44) +s(X85) +s(X25) + s(X95) + s(X44) + s(X9) +s(X48) + s(X61) + s(X87) + s(X73) + X68 + X49 + X107 + X96 + X43 + X70 + X101 + X69 + X12 + X104 + X27, data=training, method="REML")
summary(gam1)
prediction <- predict(gam1, newdata = testing) 
mean((prediction-ytest)^2) 

# 5.77
# 5.715


#print(sqrt(min(gam0$cv.error)))

# Potential Interactions:  X12:X15 + X85:X9 + X85:25 + X9:25 + X6:X19

example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')



# gbm:::interact.gbm(gbmFit4, data=training, i.var=c("X23", "X54"), best.iter)


data(boston,package="stima")
m1 <- lm(c.medv~.^2,data=boston)
scboston <- boston
scboston[-2] <- scale(boston[-2])
m2 <- lm(c.medv~(chas+dis+tax+b+rm+lstat+age+nox+zn+
                   crim+rad+indus+ptratio)^2,data=scboston)
library(dotwhisker)
dwplot(m2)+geom_vline(xintercept=0,lty=2)

#lm.fit=lm(y ~ (X1*X2 + X3*X4 + X5 + X6 + X7 + X8 + X9+ X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53 + X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112)^2,data=training)
# this will create a .csv file in your working directory
# this is the file you should upload to the competition website

# For submission 
# write.csv(example_pred,file='gbmLecture9.csv',row.names=FALSE)