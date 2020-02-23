# Initial env
rm(list = ls())
graphics.off()

###install.packages("MASS")
# Loading package
library(MASS)
library("arules")
data(Boston)

# Problem 2_a: 
## Create Histogram of the diff variables
hist(Boston$crim, breaks = 20)
hist(Boston$zn, breaks = 20)
hist(Boston$indus, breaks = 20)
hist(Boston$chas, breaks = 20)
hist(Boston$nox, breaks = 20)
hist(Boston$rm, breaks = 20)
hist(Boston$age, breaks = 20)
hist(Boston$dis, breaks = 20)
hist(Boston$rad, breaks = 20)
hist(Boston$tax, breaks = 20)
hist(Boston$ptratio, breaks = 20)
hist(Boston$black, breaks = 20)
hist(Boston$lstat, breaks = 20)
hist(Boston$medv, breaks = 20)

## Create label_bos
label_bos <-Boston
names(label_bos)

## Deal with odered variable
quantile(label_bos$crim, c(0, 0.5, 0.75, 0.80, 0.90, 1))
## 0%       50%       75%       80%       90%      100% 
## 0.006320  0.256510  3.677083  5.581070 10.753000 88.976200 
label_bos[["crim"]] <- ordered( cut( label_bos$crim, c(0, 3, 5, 100)), labels = c("Low", "Mid", "High"))

ordered(unique(label_bos$zn))
## 26 Levels: 0 < 12.5 < 17.5 < 18 < 20 < 21 < 22 < 25 < 28 < 30 < 33 < 34 < 35 < 40 < 45 < ... < 100
label_bos[["zn"]] <- ordered( cut( label_bos$zn, c(-1, 12.5, 60, 80, 101)), labels = c("Not-above", "Small", "Mid", "Large"))

label_bos[["indus"]] <- ordered( cut( label_bos$indus, c(0, 10, 20, 28)), labels = c("Low", "Mid", "High"))

# Only two types of value, 1 if tract bounds river; 0 otherwise.
## Eliminate variables
label_bos$chas <- NULL

label_bos[["nox"]] <- ordered( cut( label_bos$nox, c(0.3, 0.5, 0.8, 0.9)), labels = c("Low", "Mid", "High"))

quantile(label_bos$rm)
## 0%    25%    50%    75%   100% 
## 3.5610 5.8855 6.2085 6.6235 8.7800 
label_bos[["rm"]] <- ordered( cut( label_bos$rm, c(0, 5, 7, 9)), labels = c("Small", "Mid", "Large"))

label_bos[["age"]] <- ordered( cut( label_bos$age, c(0, 40, 80, 101)), labels = c("Youth", "Senior", "Elderly"))

label_bos[["dis"]] <- ordered( cut( label_bos$dis, c(0, 4, 8, 13)),labels = c("Low", "Mid","High"))

quantile(label_bos$rad)
## 0%  25%  50%  75% 100% 
## 1    4    5   24   24
## Eliminate variable due to polarize
label_bos$rad <- NULL

label_bos[["tax"]] <- ordered( cut( label_bos$tax, c(180, 300, 500, 720)),labels = c("Low", "Mid", "High"))

label_bos[["ptratio"]] <- ordered( cut( label_bos$ptratio, c(12, 16, 20, 23)),labels = c("Low", "Mid", "High"))

quantile(label_bos$black)
## 0%      25%      50%      75%     100% 
## 0.3200 375.3775 391.4400 396.2250 396.9000 
## Eliminate variable due to concentrative
label_bos$black <- NULL

label_bos[["lstat"]] <- ordered( cut( label_bos$lstat, c(1, 10, 20, 38)),labels = c("Low", "Mid", "High"))

label_bos[["medv"]] <- ordered( cut( label_bos$medv, c(0, 20, 40, 51)),labels = c("Low", "Mid", "High"))

## Convert to a binary incidence matrix
trans_bos <- as(label_bos,"transactions")
summary(trans_bos)

# Problem 2_b: 
itemFrequencyPlot(trans_bos,support = 0.05, cex.name = 0.8)

## Apply the apriori algorithm
bos_rules <- apriori(trans_bos,parameter = list(support = 0.01, confidence = 0.8))
summary(bos_rules)

# Problem 2_c:
## Set the rule of low crime and low dis
low_crime_rules <- subset(bos_rules,subset =rhs %in% "crim=Low" & lhs %in% "dis=Low" & lift>1.2)
low_crime_rules <- subset(bos_rules,subset =rhs %in% "crim=Low,dis=Low" & lift>1.2)
## Check the rules
low_crime_rules
inspect(head(sort(low_crime_rules, by = "lift" ), n = 10))

# Problem 2_d:
## set the rule of low crime and low dis
low_ptr_rules <- subset(bos_rules,subset =rhs %in% "ptratio=Low" & lift>1.2)
## Check the rules
low_ptr_rules
inspect(head(sort(low_ptr_rules, by = "lift" ), n = 10))

# Problem 2_e:
## Loading package
## install.packages("rpart)
## install.packages("rpart.plot)

library(rpart)
library(rpart.plot)

## Generate regression rule and pruned
bos_reg <- rpart(ptratio~., method="anova", data=Boston)

## The min_cp is the last data of cptable, skip prune tree.
min_cp <- which.min(bos_reg$cptable[,4])

rpart.plot(bos_reg, main="Regression tree")
