library(ISLR)
library(leaps)
library(tidyverse)


Hitters <- na.omit(Hitters)

regfit_full <- regsubsets(Salary ~., Hitters)
summary(regfit_full)

regfit_full <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
reg_summary <- summary(regfit_full)

names(reg_summary)

#R^2 increases with more variables (as expected)
reg_summary$rsq


# Plots -------------------------------------------------------------------

par(mfrow=c(2,2), mar = c(1, 1, 1, 1))
plot(reg_summary$rss,
     xlab="Number of Variables",
     ylab="RSS",
     type="l")
plot(reg_summary$adjr2,
     xlab="Number of Variables",
     ylab="Adjusted RSq",
     type="l")

which.max(reg_summary$adjr2)

points(11, 
       reg_summary$adjr2[11], 
       col="red",
       cex=2,
       pch=20)

plot(reg_summary$cp,
     xlab="Number of Variables",
     ylab="Cp",
     type="l")

which.min(reg_summary$cp)
points(10,
       reg_summary$cp[10],
       col="red",
       cex=2,
       pch=20)

which.min(reg_summary$bic)
plot(reg_summary$bic,
     xlab="Number of Variables",
     ylab="BIC",
     type="l")

points(6,
       reg_summary$bic[6],
       col="red",
       cex=2,
       pch=20)

par(mar=c(1,1,1,1))

plot(regfit_full, scale="r2", mar = c(1, 1, 1, 1))
plot(regfit_full, scale="adjr2", mar = c(1, 1, 1, 1))
plot(regfit_full, scale="Cp", mar = c(1, 1, 1, 1))
plot(regfit_full, scale="bic", mar = c(1, 1, 1, 1))


coef(regfit_full, 6)


# Stepwise Selection ------------------------------------------------------

regfit_fwd <- regsubsets(Salary ~.,
                         data = Hitters,
                         nvmax = 19,
                         method = "forward")

summary(regfit_fwd)

regfit_bwd <- regsubsets(Salary ~.,
                         data = Hitters,
                         nvmax = 19,
                         method = "backward")

summary(regfit_bwd)

coef(regfit_full, 7)
coef(regfit_fwd, 7)
coef(regfit_bwd, 7)



# Selecting Models --------------------------------------------------------

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



k <- 10

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
coef(reg_best, 11)
