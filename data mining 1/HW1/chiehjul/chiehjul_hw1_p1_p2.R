
# install package
install.packages("ISLR")
library(ISLR)

names(Auto)

plot(Auto$cylinders , Auto$mpg )
attach(Auto)

ori.mpg <- mpg

hist(mpg)
hist(mpg, col=3,breaks=15)
hist(mpg, col=4,breaks=10, ylim = c(0,100),main="Histogram of original mpg")

summary(mpg)

# Check number of NA
length(mpg[is.na(mpg) == TRUE])

# Slicing data
mpg.sli <- mpg[mpg>10&mpg<45]
Auto.sli <- subset(Auto,mpg>10&mpg<45)

hist(Auto.sli$mpg, col=4,breaks=10, ylim = c(0,100),main="Histogram after slicing")


summary(Auto.sli$mpg)

save(Auto.sli, file = "chiehjul_hw1_p1.RData")

# Q2

# a. Which predictors appear to have a significant relationship to the response.
lm.fit <- lm(mpg ~ cylinders+year+weight+horsepower+displacement+acceleration+origin, 
   data= Auto.sli)
lm.fit$coefficients

# lm(Y ~ a*b)
# same as lm(Y ~ a + b + a:b)

# c. Use the * and : symbols to fit models with interactions. Are there any interactions that are significant? 
lm.fit <- lm(mpg ~ cylinders*year*weight*horsepower*displacement*acceleration*origin, 
             data= Auto.sli)

head(sort(lm.fit$coefficients,decreasing=TRUE), n = 10)

