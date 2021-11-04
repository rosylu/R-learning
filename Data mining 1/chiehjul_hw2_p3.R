install.packages("caret")
install.packages("glmnet")
install.packages("pls")


require(caret)
require(ISLR)
library(glmnet) 

data('College')

set.seed(7)

df <- data.frame(na.omit(College))
random_data <- sample(1:2,size=nrow(df),replace=TRUE,prob=c(0.8,0.2))

train <- df[random_data==1,]
test <- df[random_data==2,]

mod <- lm(Apps ~ ., data = train)
summary(mod)

test_y <- predict(mod, newdata = test)
mean(test_y!=test$Apps)
mean((test_y-test$Apps)^2)

### Transforming Private

df_trans_yes <- df
df_trans_yes$Private <- ifelse(df_trans_yes$Private=="Yes", 1, 0)

random_data <- sample(1:2,size=nrow(df_trans_yes),replace=TRUE,prob=c(0.8,0.2))

train_tr <- df_trans_yes[random_data==1,]
test_tr <- df_trans_yes[random_data==2,]

mod_tr <- lm(Apps ~ ., data = train_tr)
summary(mod_tr)

test_y_tr <- predict(mod_tr, newdata = test_tr)
mean((test_y_tr-test_tr$Apps)^2)

### b) Fit a ridge regression model on the training set, with λ chosen by cross-validation. 
## Report the test error obtained.
X <- data.matrix(train)
Y <- train$Apps

ridge.mod = glmnet(X, Y, alpha=0)
names(ridge.mod)
coef(ridge.mod)
dim(coef(ridge.mod))

X_test <- data.matrix(test)
Y_test  <- test$Apps

ridge.pred2 <- predict(ridge.mod, newx = X_test)

test_error <- mean((ridge.pred2 - Y_test)^2) 

## λ chosen by cross-validation.
train_set <- sample(1:nrow(X), round(nrow(X)/2))
cv.out <- cv.glmnet(X[train_set,], Y[train_set], alpha = 0)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min

X_test <- data.matrix(test)
Y_test  <- test$Apps

ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X_test)
## error
test_error <- mean((ridge.pred2 - Y_test)^2) 

### c) Fit a lasso model on the training set, with λ chosen by crossvalidation.
### Report the test error obtained, along with the number of non-zero coefficient estimates.

lasso.mod <- glmnet(X, Y, alpha = 1)
plot(lasso.mod)

lasso.pred <- predict(lasso.mod, newx = X_test)
test_error <- mean((lasso.pred - Y_test)^2) 

cv_l.out <- cv.glmnet(X[train_set,], Y[train_set], alpha = 1)
plot(cv_l.out)

bestlam_l <- cv_l.out$lambda.min
lasso.mod2 =glmnet(X,Y,alpha=1,lambda=bestlam)
lasso.pred2 <- predict(lasso.mod2, s = bestlam_l, newx = X_test)

## error
test_error <- mean((lasso.pred2 - Y_test)^2) 

lasso.pred2 <- predict(lasso.mod2, s = bestlam_l, newx = X_test,  type="coefficients")

### e) pcr

library(pls)

pcr_model <- pcr(Apps~., data=train, scale=T, validation="CV")
validationplot(pcr_model, val.type="MSEP")

pcr.pred <- predict(pcr_model, test, ncomp=1)
test_error <- mean((pcr.pred - Y_test)^2)

pcr.pred <- predict(pcr_model, test, ncomp=2)
test_error <- mean((pcr.pred - Y_test)^2)

pcr.pred <- predict(pcr_model, test, ncomp=3)
test_error <- mean((pcr.pred - Y_test)^2)

pcr.pred <- predict(pcr_model, test, ncomp=4)
test_error <- mean((pcr.pred - Y_test)^2)

pcr.pred <- predict(pcr_model, test, ncomp=5)
test_error <- mean((pcr.pred - Y_test)^2)

### f) pls
plsr_model <- plsr(Apps~., data=train, scale=T, validation="CV")
validationplot(plsr_model, val.type="MSEP")

plsr.pred <- predict(plsr_model, test, ncomp=5)
test_error <- mean((plsr.pred - Y_test)^2)

plsr.pred <- predict(plsr_model, test, ncomp=6)
test_error <- mean((plsr.pred - Y_test)^2)