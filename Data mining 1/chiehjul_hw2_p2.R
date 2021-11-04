rm(list = ls())

#install.packages("ggplot2")
library("ggplot2")


load("~/Documents/DS/EAS 506/HW2/zip.test.RData")
load("~/Documents/DS/EAS 506/HW2/zip.train.RData")

### Consider only the 2’s and 3’s 

#ncol(zip.train)
#nrow(zip.train)

train <- data.frame(zip.train[zip.train[, 1] == 2 | zip.train[, 1] == 3,] )
#ggplot(train, aes(X1,X2)) + geom_point(aes(colour = as.factor(Y))) + theme(legend.position = "none")

test <- data.frame(zip.test[zip.test[, 1] == 2 | zip.test[, 1] == 3,] )

### Classification performance of linear regression
mod <- lm(train[,1] ~ ., data = train[,-1]) # -1 for ignore col 1
summary(mod)

train_y <- predict(mod, newdata = train[,-1])
test_y <- predict(mod, newdata = test[,-1])
# If y > mean, then classify it to 3, else 2
train_y_appro <- ifelse(train_y>mean(train_y), 3, 2)
test_y_appro <- ifelse(test_y>mean(test_y), 3, 2)

#Calculate train/test error
mean(train_y_appro!=train[,1])
mean(test_y_appro!=test[,1])

### k-nearest neighbor classification
require(class)

knn(train[,-1], test[,-1], train[,1], 1)

k_vals <- c(1, 3, 5, 7, 9, 11, 13, 15)
train_error <- c()
test_error <- c()

for (i in 1:8){
  # select "k"
  kk <- k_vals[i]
  
  # apply the algorithm
  train_y <- knn(train[,-1], train[,-1], train[,1], k = kk)
  test_y <- knn(train[,-1], test[,-1], train[,1], k = kk)
  
  # check the answer
  #test_y
  
  # make a call --- you may have 2.1 2.7 ----> "round"
  # ?round
  
  # calculate the error
  train_error[i] <- mean(train_y != train[,1])
  test_error[i] <- mean(test_y != test[,1])
  
  cat("\nTrain error with k = ", kk, "is", mean(train_y != train[,1]), "\n")
  cat("Test error with k = ", kk, "is", mean(test_y != test[,1]), "\n")
  
  
}

df <- data.frame(dataset=rep(c("Train", "test"), each=8),
                  error_rate=c(train_error,test_error),
                  k_val=c(k_vals, k_vals))
quartz()

ggplot(data=df, aes(x=k_val, y=error_rate, group=dataset)) +
  geom_line(aes(color=dataset))+
  geom_point(aes(color=dataset))

