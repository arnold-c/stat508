library(ISLR)
library(boot)
library(leaps)

RNGkind(sample.kind = "Rounding")


# Q1 -------------------------------------------------

set.seed(3)

train <- sample(x = 392, size = 196, replace = FALSE)

attach(Auto)

lm_fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm_fit, Auto))[-train]^2)

lm_fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm_fit2, Auto))[-train]^2)

lm_fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm_fit3, Auto))[-train]^2)


# Q3 ----------------------------------------------------------------------

Hitters <- na.omit(Hitters)

regfit_full <- regsubsets(Salary ~., Hitters)
summary(regfit_full)


# Q4 ----------------------------------------------------------------------

# 8-variable model
regfit_full_8 <- regsubsets(Salary ~., data = Hitters, nvmax = 8)
reg_summary_8 <- summary(regfit_full_8)

## Cp
which.min(reg_summary_8$cp)
round(reg_summary_8$cp[8], digits = 1)

## BIC
which.min(reg_summary_8$bic)
round(reg_summary_8$bic[6], digits = 1)




# 12-variable model
regfit_full_12 <- regsubsets(Salary ~., data = Hitters, nvmax = 12)
reg_summary_12 <- summary(regfit_full_12)

## Cp
which.min(reg_summary_12$cp)
round(reg_summary_12$cp[10], digits = 1)

## BIC
which.min(reg_summary_12$bic)
round(reg_summary_12$bic[6], digits = 1)


# Q5 ----------------------------------------------------------------------

set.seed(1)

train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)

test <- (!train)

regfit_best <- regsubsets(Salary ~.,
                          data = Hitters[train,],
                          nvmax=19)

test_mat <- model.matrix(Salary ~.,
                         data = Hitters[test,])

val_errors <- rep(NA, 19)

for(i in 1:19){
    coefi <- coef(regfit_best, id = i)
    pred <- test_mat[, names(coefi)] %*% coefi
    val_errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

val_errors
which.min(val_errors)
coef(regfit_best, 10)

# Make a prediction function as there isn't one built into regsubsets
predict_regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

regfit_best <- regsubsets(Salary ~.,
                          data = Hitters,
                          nvmax=19)

coef(regfit_best, 10)



k <- 5

set.seed(1)

folds <- sample(1:k, nrow(Hitters), replace=TRUE)

cv_errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
    best_fit <- regsubsets(Salary ~ .,
                           data = Hitters[folds != j, ],
                           nvmax = 19
    )
    for (i in 1:19) {
        pred <- predict_regsubsets(best_fit,
                                   Hitters[folds == j, ],
                                   id = i
        )
        cv_errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
    }
}

mean_cv_errors <- apply(cv_errors, 2, mean)
mean_cv_errors

par(mfrow = c(1, 1))
plot(mean_cv_errors, type="b")
which.min(mean_cv_errors)

reg_best <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg_best, 10)

