library(ISLR)
library(tidyverse)
library(e1071)
library(ROCR)


RNGkind(sample.kind = "Rounding")



# Q1 ----------------------------------------------------------------------

set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1

plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))

svmfit_c10 <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit_c10, dat)
svmfit_c10$index
summary(svmfit_c10)

svmfit_c0.1 <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit_c0.1, dat)
svmfit_c0.1$index
summary(svmfit_c0.1)


# Q2 ----------------------------------------------------------------------

set.seed(1)

tune.out <- tune(svm, y ~ ., 
                 data = dat, 
                 kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out)

bestmod <- tune.out$best.model

summary(bestmod)

xtest <- matrix(rnorm(20 * 2), ncol = 2)

ytest <- sample(c(-1, 1), 20, rep = TRUE)

xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1

testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)

table(predict = ypred, truth = testdat$y)

svmfit_c0.01 <- svm(y ~ ., data = dat, kernel = "linear", cost = .01, scale = FALSE)
svmfit_c1 <- svm(y ~ ., data = dat, kernel = "linear", cost = 1, scale = FALSE)

ypred_c0.1 <- predict(svmfit_c0.1, testdat)
ypred_c0.01 <- predict(svmfit_c0.01, testdat)
ypred_c1 <- predict(svmfit_c1, testdat)

table(predict = ypred_c0.1, truth = testdat$y)
table(predict = ypred_c0.01, truth = testdat$y)
table(predict = ypred_c1, truth = testdat$y)


# Q3 ----------------------------------------------------------------------

set.seed(1)

x <- matrix(rnorm(200 * 2), ncol = 2)

x[1:100, ] <- x[1:100, ] + 2

x[101:150, ] <- x[101:150, ] - 2

y <- c(rep(1, 150), rep(2, 50))

dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)

train <- sample(200, 100)

svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1)

plot(svmfit, dat[train, ])

summary(svmfit)

table(true = dat[-train, "y"], pred = predict(svmfit, newdata = dat[-train, ]))
