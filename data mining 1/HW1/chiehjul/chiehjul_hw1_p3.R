# Initial env
rm(list = ls())
graphics.off()

###install.packages("MASS")
# Loading package
library(MASS)
data(Boston)

# a) Make pairwise scatterplots of the predictors, and describe your findings. 
pairs(Boston)

# b) Are any of the predictors associated with per capita crime rate? 
cor(Boston$crim,Boston)

# c) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor. 
hist(Boston$crim, breaks =20, xlim = c(0,100))
quantile(Boston$crim, c(0, 0.5, 0.75, 0.90, 0.95, 1))
length(Boston$crim[Boston$crim>10])/length(Boston$crim)
length(Boston$crim[Boston$crim>15])/length(Boston$crim)

hist(Boston$tax, breaks =20)
length(Boston$tax[Boston$tax>650])/length(Boston$tax)

hist(Boston$ptratio, breaks =20)
quantile(Boston$ptratio, c(0, 0.5, 0.75, 1))

# d) In this data set, how many of the suburbs average more than seen rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling. 
summary(Boston$rm)

length(Boston$rm[Boston$rm > 7])
length(Boston$rm[Boston$rm > 8])

summary(Boston$rm)
length(Boston$rm[Boston$rm > 8])/length(Boston$rm)

