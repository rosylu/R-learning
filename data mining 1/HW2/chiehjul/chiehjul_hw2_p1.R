library(readr)
cereal <- read_csv("Documents/DS/EAS 506/HW2/cereal.csv")

# Remove name from cereal csv
df <- data.frame(cereal[,-1])

### a. Divide the data into test and training.  Fit a linear model and report the MSE.
# Divide the data into test and training
set.seed(7)
random_data <- sample(1:2,size=nrow(df),replace=TRUE,prob=c(0.8,0.2))

# Drop the col of string type 
#train <- df[random_data==1,]
#test <- df[random_data==2,]
train <- df[random_data==1,3:15]
test <- df[random_data==2,3:15]

#Fit a linear model
mod <- lm(train[,ncol(train)] ~ ., data = train[,-ncol(train)]) # Ignore last col
summary(mod)


train_y <- predict(mod, newdata = train[,-ncol(train)])
test_y <- predict(mod, newdata = test[,-ncol(test)])

# Error rate
#mean(train_y!=round(train[,ncol(train)],5))
#mean(test_y!=round(test[,ncol(test)],5))

# MSE
mean((train_y - train[,ncol(train)]) ^2 )
mean((test_y - test[,ncol(test)]) ^2 )


### b) With the data in (a) perform either forward or backwards subset selection.
library(leaps)

regfit.bwd <- regsubsets(train[,ncol(train)]~., data = train[,-ncol(train)], nbest = 1, nvmax = 12, method = "backward")
bwd_sum <- summary(regfit.bwd)
bwd_size <- as.numeric(attr(bwd_sum$which, "dimnames")[[1]])
bwd_best_rss <- tapply(bwd_sum$rss, bwd_size, min)
#bwd.dummy <- lm(train[,ncol(train)] ~ ., data = train[,-ncol(train)])

#plot(1:12, bwd_best_rss, type = "b", xlab = "subset size", ylab = "RSS", col= "red2", main = "Backwards subset selection")
par(mfrow = c(2,2))
#quartz()

plot(bwd_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(bwd_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(bwd_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(bwd_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

which(bwd_sum$cp == min(bwd_sum$cp))
which(bwd_sum$bic == min(bwd_sum$bic))

quartz()
plot(regfit.bwd, scale = "Cp")
quartz()
plot(regfit.bwd, scale = "bic")

### C) With the data in (a) perform exhaustive subset selection.
regfit.full <- regsubsets(train[,ncol(train)]~., data = train[,-ncol(train)], nbest = 1, nvmax = 12, method = "exhaustive")
my_sum <- summary(regfit.full)

par(mfrow = c(2,2))
#quartz()

plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

#They both agree model with ? variables is the best.
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))


quartz()
plot(regfit.full, scale = "r2")
quartz()
plot(regfit.full, scale = "adjr2")
quartz()
plot(regfit.full, scale = "Cp")
quartz()
plot(regfit.full, scale = "bic")


#
# Examine the best 5 (or whatever) variable models
summary(regfit.full)$outmat[5,]
summary(regfit.bwd)$outmat[5,]

# Look at the regression models determined by the different methods
coef(regfit.full, 5)
coef(regfit.bwd, 5)

# Draw some conclusions through comparisons between models (a-c). 
# Reflect on the comparative predictive accuracy, and model interpretation. 
# Which model would you say is the “best one” based on your results?  


##Backward subset selection

train_error <- c()
test_error <- c()


for (i in 2:12){

  train_subset <- train[,c(which(bwd_sum$which[i,-1]),ncol(train))]
  test_subset <- test[,c(which(bwd_sum$which[i,-1]),ncol(test))]
  
  #Fit a linear model
  mod_sub <- lm(train_subset[,ncol(train_subset)] ~ ., data = train_subset[,-ncol(train_subset)]) # Ignore last col
  summary(mod_sub)
  
  train_subset_y <- predict(mod_sub, newdata = train_subset[,-ncol(train_subset)])
  test_subset_y <- predict(mod_sub, newdata = test_subset[,-ncol(test_subset)])
  
  # MSE
  train_error[i] <- mean((train_subset_y - train_subset[,ncol(train_subset)]) ^2 )
  test_error[i] <- mean((test_subset_y - test_subset[,ncol(test_subset)]) ^2 )
  
}

df <- data.frame(dataset=rep(c("Train", "test"), each=11),
                 mse=c(train_error[-1],test_error[-1]),
                 subset_val=c(2:12, 2:12))
quartz()

ggplot(data=df, aes(x=subset_val, y=mse, group=dataset)) +
  geom_line(aes(color=dataset))+
  geom_point(aes(color=dataset))+ xlim(1,13)+
  ggtitle("Backsward subset selection")

## Exhaustive subset selection

train_error <- c()
test_error <- c()


for (i in 2:12){
  
  train_subset <- train[,c(which(my_sum$which[i,-1]),ncol(train))]
  test_subset <- test[,c(which(my_sum$which[i,-1]),ncol(test))]
  
  #Fit a linear model
  mod_sub <- lm(train_subset[,ncol(train_subset)] ~ ., data = train_subset[,-ncol(train_subset)]) # Ignore last col
  summary(mod_sub)
  
  train_subset_y <- predict(mod_sub, newdata = train_subset[,-ncol(train_subset)])
  test_subset_y <- predict(mod_sub, newdata = test_subset[,-ncol(test_subset)])
  
  # MSE
  train_error[i] <- mean((train_subset_y - train_subset[,ncol(train_subset)]) ^2 )
  test_error[i] <- mean((test_subset_y - test_subset[,ncol(test_subset)]) ^2 )
  
}

df <- data.frame(dataset=rep(c("Train", "test"), each=11),
                 mse=c(train_error[-1],test_error[-1]),
                 subset_val=c(2:12, 2:12))
quartz()

ggplot(data=df, aes(x=subset_val, y=mse, group=dataset)) +
  geom_line(aes(color=dataset))+
  geom_point(aes(color=dataset))+ xlim(1,13)+
  ggtitle("Exhaustive subset selection")
