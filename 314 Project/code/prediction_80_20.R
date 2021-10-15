# Use a k of 35. Gives error = 20.8


# Libraries
library(caret)

# Importing data
data = read.csv(file = 'trainingdata.csv') # read a file from the working directory
attach(data)

# Make data reproducible
set.seed(1)

# 80-20 Function
valset_knn = function(X,Y,krange){
  n = nrow(y)
  l.tr = 0.8*n
  l.val = n - l.tr
  train_index = sample(1:n,l.tr) # randomly sample l.tr points from 1,...,n
  # this will correspond to the training set
  
  # create array where results of validation error will be stored
  valset_error = array(0,length(krange))
  
  # loop over values of K, for each store error on validation set
  for(k in 1:length(krange)){
    # only use data with index in train_index to fit the model
    K = krange[k]
    fit = knnreg(as.matrix(x[train_index, 1:112]),y[train_index, 1],k=K)
    
    # now use fitted model to predict data which are not in train_index
    # recall from lecture 0: a[-b] gives entris of a which are not indexed by b
    pr = predict(fit, newdata = data.frame(x = x[-train_index, 1:112]))
    
    # compute and store the validation error
    valset_error[k] = mean((y[-train_index, 1] - pr)^2)
  }
  
  return(valset_error) # this will be the output of the function
}


# define variables
n = 2000
y = data[1]
x = data[,-1]


# Call function
error_vs = valset_knn(X=x,Y=y,krange = 1:50)
 
# Plot Output
plot(1:50, error_vs,xlab='k',ylab='error',main='80-20 aproach')
selected_k = which.min(error_vs) 
selected_k

# Minimization point
lines(c(selected_k,selected_k), c(0,100), col= 'red',lwd=3)


# # Repeat 100 times
# r = 3
# k_vs = array(0,r) # for storing results

# for(j in 1:r){
#   k_vs[j] = which.min(valset_knn(x,y,1:50))
#   print(j) # print progress on screen
# }

# # make a histogram of the results
# hist(k_vs,main = 'Validation set approach', xlab = 'k', breaks = (0:10)*2.5)
# lines(c(15,15),c(0,100), col= 'red',lty=2, lwd=3)



