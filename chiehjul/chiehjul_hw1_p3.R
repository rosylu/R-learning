# Initial env
rm(list = ls())
graphics.off()

# Loading package
library(rpart)
library(rpart.plot)
load("~/Desktop/marketing.rdata")

# Create reference sample
ref_sample <- data.frame(
  Income = sample(seq(1,9), size = 8993, replace = T),
  Sex = sample(  c(1,2), size = 8993, replace = T),
  Marital = sample(seq(1,5), size = 8993, replace = T),
  Age = sample(seq(1,7), size = 8993, replace = T),
  Edu = sample(seq(1,6), size = 8993, replace = T),
  Occupation = sample(seq(1,9), size = 8993, replace = T),
  Lived = sample(seq(1,5), size = 8993, replace = T),
  Dual_Income = sample(seq(1,3), size = 8993, replace = T),
  Household = sample(seq(1,9), size = 8993, replace = T),
  Householdu18 = sample(seq(0,9), size = 8993, replace = T),
  Status = sample(seq(1,3), size = 8993, replace = T),
  Home_Type = sample(seq(1,5), size = 8993, replace = T),
  Ethnic = sample(seq(1,8), size = 8993, replace = T),
  Language = sample(seq(1,3), size = 8993, replace = T),
  row.names = NULL
)

ref_sample$class = 0

# Copy the original data and add class 1
train_sample <- marketing
train_sample$class = 1

# Combination of two sample
comb_sample = rbind(ref_sample, train_sample)

# Grow a classification tree
#mod.control <- rpart.control(minsplit = 5, cp = 0)
mod.control <- rpart.control(cp = 0)
class_sample <- rpart(class~., method = "class", data = comb_sample, control = mod.control)

# Pruned tree
min_cp <- which.min(class_sample$cptable[,4])
pruned <- prune(class_sample, cp = class_sample$cptable[min_cp,1])

# Generate pdf
pdf("full_tree.pdf")
rpart.plot(class_sample, main="Classification full tree")
dev.off()

pdf("pruned_tree.pdf")
rpart.plot(pruned, main="Classification pruned tree")
dev.off()

# Calculate the accuracy
## origin model
predict_class <- predict(class_sample, comb_sample[,-15], type = "class")
confMat <- table(comb_sample$class,predict_class)
confMat
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

## pruned
pruned_predict_class <- predict(pruned, comb_sample[,-15], type = "class")
pruned_confMat <- table(comb_sample$class,pruned_predict_class)
pruned_confMat
pruned_accuracy <- sum(diag(pruned_confMat))/sum(pruned_confMat)
pruned_accuracy
