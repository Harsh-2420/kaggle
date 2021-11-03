#install.packages('glmnet')
#install.packages('ISLR')
#install.packages('plotmo')

library(plotmo) # nicer plotting of glmnet output
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set 'Hitters'

data = read.csv(file = 'trainingdata.csv') # read a file from the working directory
attach(data) # works since we loaded the package ISLR

#------------------------------------------------------------
# note: some values of salary are NA, ie unknown
# Hitters = na.omit(Hitters) # remove missing values

x = model.matrix(y ~ . ,data )[,-1]  
# bring data in format that glmnet can deal with
# [,-1] is to remove intercept


y = data$y

grid = 10^seq(10,-2,length = 100) 
# produce a grid of lambda values
# those lambda values will be used for  lasso and ridge

# ----------------------------------------------------
# now look at ridge
# glmnet with alpha = 0 is ridge regression

ridge.mod = glmnet(x,y,alpha = 0, lambda =grid) # alpha = 0 is ridge regression
coe = coef(ridge.mod) # extract estimated coefficients for different lambda values 
dim(coe) 

# coe is a matrix
# each column corresponds to coefficients obtianed 
# from ridge regression with lambda = grid[columnnumber]

plot(ridge.mod,xvar = 'lambda')

plot_glmnet(ridge.mod)

# plot seems to indicate that coefficients are exactly zero
# but in fact they are just very close to zero 
# note grid[1] is maximal
grid[1]
coe[,1]


# ----------------------------------------------------
# next, we compare the performance of different methods
# in terms of prediction on the Hitters data set

set.seed(1)

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
train = sample(1:nrow(x),nrow(x)/2) 

test = (-train)
ytest = y[test]

# fit ridge and lasso models
fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)

# do cross-validation
# this is implemented for ridge and lasso in glmnet
set.seed(1)
cv.ri = cv.glmnet(x[train,],y[train],alpha =0, lambda = grid) # cross validation for ridge

# look at the plot
# left dashed line: lambda selected by cross-validation
# right dashed line: lambda by cross-validation with one standard error rule
# red dots are cv values
# bars correspond to standard errors
plot(cv.ri)

names(cv.ri)
cv.ri$lambda.min
# lambda.min is by cross-validation
cv.ri$lambda.1se
# lambda.1se is obtained from one standard error rule

# do the same for lasso
set.seed(1)
cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso

# compare predictive performance of ridge and lasso
# first extract the lambda values obtained by cross-validation
best.ri = cv.ri$lambda.min
best.la = cv.la$lambda.min

# now predict using those lambda values
# s correspnds to lambda value
pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
pred.la = predict(fit.la, s = best.la, newx = x[test,])

mean((pred.ri-ytest)^2)  # error of ridge
mean((pred.la-ytest)^2)  # error of lasso

# compare with results obtained from one standard error rule

ri.1se = cv.ri$lambda.1se
la.1se = cv.la$lambda.1se

pred.ri = predict(fit.ri, s = ri.1se, newx = x[test,])
pred.la = predict(fit.la, s = la.1se, newx = x[test,])

mean((pred.ri-ytest)^2)  # error of ridge
mean((pred.la-ytest)^2)  # error of lasso

# results are a lot worse
# models that are good for interpretation are not always good for prediction...

# compare this with linear model including all predictors

predi.lm = predict(fit.ri, s = 0, newx = x[test,])
mean((predi.lm-ytest)^2)

# all of the above was done for one random slit of data
# do this for another random split
# go back to beginning of code and set.seed(2), set.seed(3) etc   


set.seed(2)
train = sample(1:nrow(x),nrow(x)/2) 

test = (-train)
ytest = y[test]

# fit ridge and lasso models
fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)

cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso

best.ri = cv.ri$lambda.min
best.la = cv.la$lambda.min

ri.1se = cv.ri$lambda.1se
la.1se = cv.la$lambda.1se

pred.ri.1se = predict(fit.ri, s = ri.1se, newx = x[test,])
pred.la.1se = predict(fit.la, s = la.1se, newx = x[test,])
pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
pred.la = predict(fit.la, s = best.la, newx = x[test,])
predi.lm = predict(fit.ri, s = 0, newx = x[test,])

mean((pred.ri-ytest)^2)  # error of ridge
mean((pred.la-ytest)^2)  # error of lasso
mean((pred.ri.1se-ytest)^2)  # error of ridge
mean((pred.la.1se-ytest)^2)  # error of lasso
mean((predi.lm-ytest)^2)



# ----------------------------------------------------
# Next, repeat above a couple of times without setting seed to see how much results change

R = 100

t.ri = numeric(R)
t.la = numeric(R)
t.nu = numeric(R)
t.lm = numeric(R)

set.seed(1)

for(r in 1:R){
  
  train = sample(1:nrow(x),nrow(x)/2) 
  # random sample from numbers 1:nrow(x)
  # this will correspond to the training observations
  test = (-train)
  ytest = y[test]
  
  fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
  fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)
  
  cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
  cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso
  
  best.ri = cv.ri$lambda.min  # best lambda value for ridge
  best.la = cv.la$lambda.min  # best lambda value for lasso
  
  lim = lm(y[train]~., data = data.frame(x[train,]))
  
  pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
  pred.la = predict(fit.la, s = best.la, newx = x[test,])
  pred.lm = predict(fit.ri, s = 0, newx = x[test,])
  
  
  t.ri[r] = mean((pred.ri-ytest)^2)  # error of ridge
  t.la[r] = mean((pred.la-ytest)^2)  # error of lasso
  t.lm[r] = mean((pred.lm-ytest)^2)  # error of linear model
  t.nu[r] = mean((mean(y[train])-ytest)^2) # error with no predictors
  
  print(r) # output progress on screen
}

boxplot(t.ri,t.la,t.lm,t.nu, names = c('ridge','lasso','full lm','mean'))

# how often is ridge better than lasso etc? 
sum(t.ri < t.la)/R
sum(t.la < t.lm)/R
sum(t.ri < t.lm)/R

#------------------------------------------------- 
# what if our training set was smaller? 
# in the code below, just one line is changed
# which one?

R = 100

t.ri = numeric(R)
t.la = numeric(R)
t.nu = numeric(R)
t.lm = numeric(R)

set.seed(1)

for(r in 1:R){
  
  train = sample(1:nrow(x),70) 
  # random sample from numbers 1:nrow(x)
  # this will correspond to the training observations
  test = (-train)
  ytest = y[test]
  
  fit.ri = glmnet(x[train,],y[train],alpha =0, lambda = grid)
  fit.la = glmnet(x[train,],y[train],alpha =1, lambda = grid)
  
  cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
  cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso
  
  best.ri = cv.ri$lambda.min  # best lambda value for ridge
  best.la = cv.la$lambda.min  # best lambda value for lasso
  
  pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
  pred.la = predict(fit.la, s = best.la, newx = x[test,])
  pred.lm = predict(fit.ri, s = 0, newx = x[test,])
  
  t.ri[r] = mean((pred.ri-ytest)^2)  # error of ridge
  t.la[r] = mean((pred.la-ytest)^2)  # error of lasso
  t.lm[r] = mean((pred.lm-ytest)^2)  # error of lasso
  t.nu[r] = mean((mean(y[train])-ytest)^2)
  
  print(r)
}

boxplot(t.ri,t.la,t.lm, names = c('ridge','lasso','full lm'))

# how often is ridge better tan lasso etc? 
sum(t.ri < t.la)/R
sum(t.la < t.lm)/R
sum(t.ri < t.lm)/R
