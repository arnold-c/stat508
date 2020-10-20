library(ISLR)
library(glmnet)
library(tidyverse)
library(mlbench)
library(car)

RNGkind(sample.kind="Rounding") 
set.seed(1)

data("BostonHousing2")

colnames(BostonHousing2)

summary(BostonHousing2)

# Multicollinearity -------------------------------------------------------


# Drop `medv` as has been corrected so two values would cause issues of 
# collinearity. Drop town as have census tract

df <- BostonHousing2 %>%
    select(everything(), -c(medv, town))

par(mar = c(1, 1, 1, 1))
pairs(df)

# Potential collinearities based on pairs plot
# trac and dis
# nox and trac
# rm and cmedv
# cmedv and lstat
# rm and lstat

pairs(df[, c(1, 4, 9, 10, 12, 17)])

lm_fit <- lm(cmedv ~., data = df)
summary(lm_fit)
vif(lm_fit)

# rm and cmedv display collinearity in pair plot and a priori - remove rm from dataset
# lstat and cmedv display collinearity in pair plot and a priori - remove lstat
# nox and dis display collinearity in pair plot - remove nox
# Remove lat and lon as have that information in tract/not particularly relevant

df <- df %>%
    select(-c(rm, lstat, nox, lon, lat))


# Transformations ---------------------------------------------------------

for(i in seq(1, 12)){
    plot(df[, i], df$crim, main = i)
}
    
data.frame(colnames(df))   

# No major indications of quadratic/exponential relationship between crim and variables


# Ridge Regression --------------------------------------------------------

x <- model.matrix(crim ~ ., df)[, -1]
y <- df$crim

train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]


grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

plot(ridge.mod, xvar="lambda", label=T)

data.frame(colnames(x))

# Ridge Regression
ridge.mod <- glmnet(
    x[train, ],
    y[train],
    alpha = 0, 
    lambda = grid, 
    thresh = 1e-12
)

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out, mar = c(10, 10, 10, 10))
title ("Ridge Regression - log lambda vs MSE", line = 3)

# Calculate lambda min
ridge_bestlam <- cv.out$lambda.min
ridge_bestlam

ridge.pred <- predict(ridge.mod, s = ridge_bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = ridge_bestlam)[1:12, ]

# Calculate lambda 1se
ridge_lam_1se <- cv.out$lambda.1se
ridge_lam_1se

ridge.pred <- predict(ridge.mod, s = ridge_lam_1se, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = ridge_lam_1se)[1:12, ]


# Lasso Regression --------------------------------------------------------


lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out, mar = c(10, 10, 10, 10))
title ("Lasso Regression - log lambda vs MSE", line = 3)

# Calculate lambda min
lasso_bestlam <- cv.out$lambda.min
lasso_bestlam

lasso.pred <- predict(lasso.mod, s = lasso_bestlam, newx = x[test, ])

mean((lasso.pred - y.test)^2)


out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = lasso_bestlam)[1:12, ]
lasso.coef


# Calculate lambda 1se
lasso_lam_1se <- cv.out$lambda.1se
lasso_lam_1se

lasso.pred <- predict(lasso.mod, s = lasso_lam_1se, newx = x[test, ])

mean((lasso.pred - y.test)^2)


out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = lasso_lam_1se)[1:12, ]
lasso.coef

