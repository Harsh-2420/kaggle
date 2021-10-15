# Libraries
library(caret)

# Importing data
data = read.csv(file = 'trainingdata.csv') # read a file from the working directory
attach(data)

# Make data reproducible
set.seed(1)

# define variables
n = 2000
y = data[1]
x = data[,-1]

# 10-CV KNN Function
do_10cv_knn = function(X,Y,krange){
  n = nrow(y) # smaple size
  
  # permute index set
  permidx = sample(1:n,n)
  X = as.matrix(X)[permidx, 1:112]
  Y = Y[permidx, 1]
  
  # size of fold
  foldsize = 10
  
  # for saving errors
  valset_error = array(0,length(krange))
  
  for(k in 1:length(krange)){
    K = krange[k]
    # the inner loop will be over the folds
    for(j in 1:10){
      # for fold 1-9 do same as last time
      if(j < 10){
        testidx = foldsize*(j-1) + (1:foldsize)
        # take care with last fold, it might be larger
        # reason: n/10 might not be an integer
      }else{
        testidx = (foldsize*(j-1)):n
      }
      
      fit = knnreg(as.matrix(X[-testidx,1:112,drop=FALSE]),Y[-testidx, 1],k=K)
      pr = predict(fit, newdata = as.matrix(X[testidx,1:112,drop=FALSE]))
      valset_error[k] = valset_error[k] + mean((Y[testidx, 1] - pr)^2)
    } # end loop over folds
  } # end loop over k
  
  # the next line will output the result
  return(valset_error)
}


cv10 = which.min(do_10cv_knn(x,y,1:50))






r = 10

# the following arrays are for storing errors
cv10 = array(0,r)
err_cv10 = array(0,r)
err_opt = array(0,r)


for(j in 1:r){
  # run 10-fold cv
  cv10[j] = which.min(do_10cv_knn(x,y,1:50))
  
  # x0 = (1:100)/101
  
  # compute fit and error with k given by 10-cv
  fit_cv10 = knnreg(as.matrix(x),as.matrix(y),k=cv10[j])
  pr_cv10 = predict(fit_cv10,data.frame(x=as.matrix(x0)))
  err_cv10[j] = sum((pr_cv10 - f)^2)
  
  # compute fit and error with optimal k (determined earlier)
  fit_opt = knnreg(as.matrix(x),as.matrix(y),k=15)
  pr_opt = predict(fit_opt,data.frame(x=as.matrix(x0)))
  err_opt[j] = sum((pr_opt - f)^2)
  
  # print progress
  print(j)
}

mean(err_cv10)
mean(err_opt)



# plot results
par(mfrow = c(1,2))

hist(cv10,main = '10-fold cross-validation', xlab = 'k', breaks = (0:20)*2.5)
lines(c(15,15),c(0,100), col= 'red',lty=2,lwd=3)
