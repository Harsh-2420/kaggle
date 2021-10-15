library(caret)
library(class)
data <- read.csv('trainingdata.csv', header=TRUE)

# Create index column
# data$ID <- seq.int(nrow(data))
# data <- data[,c(114,1:113)]


# normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(data[2:113], normalize))
summary(data_norm[c("X1", "X2", "X3")])

# Setting train and test
train_data <- data_norm[1:1600,]
test_data <- data_norm[1601:2000,]


#Need to give labels to the new sets
train_labels <- data[1:1600,1]
test_labels <- data[1601:2000,1]



# Implementing KNN
test_pred <- knn(train=train_data, test=test_data, cl=train_labels, k=21)

conf_matrix <- table(test_labels, test_pred)

