library(ISLR)
library(tidyverse)
library(boot)

RNGkind(sample.kind="Rounding") 

set.seed(1)

train <- sample(392, 196)

attach(Auto)


# Validation Set Approach -------------------------------------------------


lm_fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm_fit, Auto))[-train]^2)

lm_fit2=lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm_fit2,Auto))[-train]^2)

lm_fit3=lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm_fit3,Auto))[-train]^2)



# LOOCV -------------------------------------------------------------------

glm_fit <- glm(mpg ~ horsepower)

cv_err <- cv.glm(Auto, glm_fit)
cv_err$delta

cv_error = rep(0, 5)

for (i in 1:5) {
    glm_fit <- glm(mpg ~ poly(horsepower, i))
    cv_error[i] <- cv.glm(Auto, glm_fit)$delta[1]
    
}

cv_error


# k-Fold ------------------------------------------------------------------

set.seed(17)

cv_error_10 <- rep(0, 10)

for(i in 1:10){
    glm_fit <- glm(mpg ~ poly(horsepower, i))
    cv_error_10[i] <- cv.glm(Auto, glm_fit, K = 10)$delta[1]
}

cv_error_10
