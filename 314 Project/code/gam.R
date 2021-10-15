


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
library(gbm)
library(mgcv)

setwd("/Users/harshjhunjhunwala/Desktop/STA314")
# Read in the train and test data provided 
data <- read.csv(file = 'trainingdata.csv')
test_data <- read.csv(file = 'test_predictors.csv') # this dataset doesn't contain predictors, not to be touched until the end. 

attach(data)

# Set seed for reproducability
set.seed(1)

corr = cor(data)
new_corr <- as.data.frame(as.table(corr))
View(new_corr[order(new_corr$Freq, decreasing=TRUE),])

corr = cor(data) %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>%
  filter(row_number()==1)

View(corr)




# Get the best predictors for our model


plot(X23, data$y, plt = T)
plot(X1, data$y, plt=T)
plot(X2, data$y, plt=T)
plot(X3, data$y, plt=T)
plot(X4, data$y, plt=T)
plot(X5, data$y, plt=T)
plot(X6, data$y, plt=T)
plot(X7, data$y, plt=T)
plot(X8, data$y, plt=T)
plot(X9, data$y, plt=T)
plot(X10, data$y, plt=T)
plot(X11, data$y, plt=T)
plot(X12, data$y, plt=T)
plot(X13, data$y, plt=T)
plot(X14, data$y, plt=T)
plot(X15, data$y, plt=T)
plot(X16, data$y, plt=T)
plot(X17, data$y, plt=T)
plot(X18, data$y, plt=T)
plot(X19, data$y, plt=T)
plot(X20, data$y, plt=T)
plot(X21, data$y, plt=T)
plot(X22, data$y, plt=T)
plot(X23, data$y, plt=T)
plot(X24, data$y, plt=T)
plot(X25, data$y, plt=T)
plot(X26, data$y, plt=T)
plot(X27, data$y, plt=T)
plot(X28, data$y, plt=T)
plot(X29, data$y, plt=T)
plot(X30, data$y, plt=T)
plot(X31, data$y, plt = T)
plot(X32, data$y, plt=T)
plot(X33, data$y, plt=T)
plot(X34, data$y, plt = T)
plot(X35, data$y, plt=T)
plot(X36, data$y, plt=T)
plot(X37, data$y, plt=T)
plot(X38, data$y, plt=T)
plot(X39, data$y, plt=T)
plot(X40, data$y, plt=T)
plot(X41, data$y, plt=T)
plot(X42, data$y, plt=T)
plot(X43, data$y, plt=T)
plot(X44, data$y, plt=T)
plot(X45, data$y, plt=T)
plot(X46, data$y, plt=T)
plot(X47, data$y, plt = T)
plot(X50, data$y, plt = T)
plot(X44, data$y, plt = T)
plot(X95, data$y, plt = T)
plot(X85, data$y, plt = T)
plot(X9, data$y, plt = T)
plot(X25, data$y, plt = T)
plot(X61, data$y, plt = T)
plot(X27, data$y, plt = T)
plot(X48, data$y, plt = T)
# Get a Prediction for the model you think are useful. 
# x19,20, 26, 29, 31, 35 - categorical
indxTrain <- createDataPartition(y = data$y,p = 0.80,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]
ytest = y[-indxTrain]

lo0 = loess(y~X79:X70, data=data)
summary(lo0)
gam0 = gam(y~X10+s(X23, by=X48)+s(X1, by=X31)+X44+X47+X50+s(X54)+X61+X90+X95+s(X2, by=X5)+X61:X27, data=training, method="REML")
summary(gam0)

prediction <- predict(gam0, newdata = testing) 
mean((prediction-ytest)^2) 
#print(sqrt(min(gam0$cv.error)))

# Potential Interactions:  X12:X15 + X85:X9 + X85:25 + X9:25 + X6:X19

example_pred = data.frame(cbind(1:6000,prediction))
names(example_pred) = c('id','y')



# gbm:::interact.gbm(gbmFit4, data=training, i.var=c("X23", "X54"), best.iter)




#lm.fit=lm(y ~ (X1*X2 + X3*X4 + X5 + X6 + X7 + X8 + X9+ X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53 + X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85+X86+X87+X88+X89+X90+X91+X92+X93+X94+X95+X96+X97+X98+X99+X100+X101+X102+X103+X104+X105+X106+X107+X108+X109+X110+X111+X112)^2,data=training)
# this will create a .csv file in your working directory
# this is the file you should upload to the competition website

# For submission 
# write.csv(example_pred,file='gbmLecture9.csv',row.names=FALSE)



