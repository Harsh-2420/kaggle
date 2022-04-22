


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

# setwd("/home/tahir/Desktop/STA314/Competition/")
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

indxTrain <- createDataPartition(y = data$y,p = 0.80,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

# Potential Interactions:  X12:X15 + X85:X9 + X85:25 + X9:25 + X6:X19 + X :X 
gbmFit4 = gbm(y~ X1 + X2 + X5+ X3 + X4 + X5 + X6 + X7 + X8 + X9+ X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53 + X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112, data=training,
              distribution='gaussian',
              bag.fraction = 0.5,
              train.fraction = 1,
              n.trees = 15000,
              interaction.depth = 15,
              cv.folds=5,
              shrinkage = 0.01)

prediction <- predict(gbmFit4, newdata = testing) 
print(sqrt(min(gbmFit4$cv.error)))

# Potential Interactions:  X12:X15 + X85:X9 + X85:25 + X9:25 + X6:X19 + X :X 
gbmFitBest = gbm(y~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9+ X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53 + X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112, data=training,
                 distribution='gaussian',
                 bag.fraction = 0.65,
                 train.fraction = 1,
                 n.trees = 20000,
                 interaction.depth = 6,
                 cv.folds=5,
                 shrinkage = 0.01)

prediction <- predict(gbmFitBest, newdata = testing) 
print(sqrt(min(gbmFitBest$cv.error)))

# Potential Interactions:  X12:X15 + X85:X9 + X85:25 + X9:25 + X6:X19 + X :X 
gbmFit7 = gbm(y~., data=training,
                 distribution='gaussian',
                 bag.fraction = 0.65,
                 train.fraction = 1,
                 n.trees = 10000,
                 interaction.depth = 6,
                 cv.folds=5,
                 shrinkage = 0.001)


prediction <- predict(gbmFit6, newdata = testing) 
print(sqrt(min(gbmFit6$cv.error)))



example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')



# gbm:::interact.gbm(gbmFit4, data=training, i.var=c("X23", "X54"), best.iter)




#lm.fit=lm(y ~ (X1*X2 + X3*X4 + X5 + X6 + X7 + X8 + X9+ X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53 + X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112)^2,data=training)
# this will create a .csv file in your working directory
# this is the file you should upload to the competition website

# For submission 
# write.csv(example_pred,file='gbmLecture9.csv',row.names=FALSE)



