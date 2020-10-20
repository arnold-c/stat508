# Set Up ------------------------------------------------------------------
library(ISLR)
library(glmnet)
library(tidyverse)

RNGkind(sample.kind="Rounding") 
set.seed(1)

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary


# Q1 ----------------------------------------------------------------------


grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

plot(ridge.mod, xvar="lambda", label=T)

data.frame(colnames(x))

ridge.mod$lambda[100]
coef(ridge.mod)[, 100]
sort(coef(ridge.mod)[, 100])


# Q2 ----------------------------------------------------------------------

set.seed(1)

train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Ridge Regression
ridge.mod <- glmnet(
    x[train, ],
    y[train],
    alpha = 0, 
    lambda = grid, 
    thresh = 1e-12
)

ridge.pred <- predict(ridge.mod, s = 50, newx = x[test, ])
mean((ridge.pred - y.test)^2)


# Q3 ----------------------------------------------------------------------

set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
lam_1se <- cv.out$lambda.1se
lam_1se

ridge.pred <- predict(ridge.mod, s = lam_1se, newx = x[test, ])
mean((ridge.pred - y.test)^2)


# Q4 ----------------------------------------------------------------------

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
lam_1se <- cv.out$lambda.1se
lam_1se

lasso.pred <- predict(lasso.mod, s = lam_1se, newx = x[test, ])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = lam_1se)
lasso.coef

# Q5 ----------------------------------------------------------------------

set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
# Substantially lower than linear regression and similar to Ridge
mean((lasso.pred - y.test)^2)
