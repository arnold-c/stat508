
# Set Up ------------------------------------------------------------------
library(ISLR)
library(glmnet)

RNGkind(sample.kind="Rounding") 
set.seed(1)

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary


# Ridge Regression --------------------------------------------------------

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))

# Coef when lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]

sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# Coef when lambda = 705
# Much larger l2 norm of the coefficients associated with smaller lambda
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]

sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# Predictions for lambda = 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


train <- sample(1:nrow(x), nrow(x)/2)

# Estimating Test Errors of Ridge Regression ------------------------------


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

ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])

# Test MSE is 101037
mean((ridge.pred - y.test)^2)

# Can use the mean of training obs if we had only fit with intercept
mean((mean(y[train])-y.test)^2)

# Can also fit prediction with very large lambda (s = 1e10)
ridge.pred <- predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Check if ridge regression is any better than 
# simple linear regression (lambda = 0)
ridge.pred <- predict(ridge.mod, s = 0.01, newx = x[test, ], exact = TRUE)
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0.01, exact = T, type = "coefficients")[1:20, ]

#Use cross-validation to find lambda tuning value
set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]



# Estimating Test Errors of Lasso Regression ------------------------------

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
# Substantially lower than linear regression and similar to Ridge
mean((lasso.pred - y.test)^2)

# Advantage over Ridge is more parsimonious model (7 vs 19 variables)
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef

