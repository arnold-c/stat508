library(MASS)
library(ISLR)
library(ggplot2)
library(car)

# fix(Boston)

names(Boston)


# Linear Regression -------------------------------------------------------


lm_fit <- lm(data = Boston, medv ~ lstat)
names(lm_fit)
coef(lm_fit)
confint(lm_fit)

predict(lm_fit,
        data.frame(lstat = c(5, 10, 15)),
        interval = "confidence")

predict(lm_fit,
        data.frame(lstat = c(5, 10, 15)),
        interval = "prediction")

# We notice that the prediction interval is wider than the confidence interval

ggplot(data = Boston, aes(x = lstat, y = medv)) +
    geom_point() +
    geom_smooth(method = lm)


ggplot(data = lm_fit) +
    geom_point(aes(x = predict(lm_fit), y = residuals(lm_fit)))

ggplot(data = lm_fit) +
    geom_point(aes(x = predict(lm_fit), y = rstudent(lm_fit)))

ggplot() + geom_bar(aes(x = hatvalues(lm_fit)))


# Multiple Linear Regression ----------------------------------------------

lm_fit <- lm(medv ~ lstat + age, data = Boston)

summary(lm_fit)

lm_fit <- lm(medv ~ ., data = Boston)

summary(lm_fit)

# compute the variance inflation factors
vif(lm_fit)


# Interaction Terms -------------------------------------------------------

# Interaction between lstat and age and treat all as predictors
summary(lm(medv ~ lstat * age, data = Boston))


# Non Linear Transformation of Predictors ---------------------------------

#I() function required to use exponentiation in formula
lm_fit2 <- lm(data = Boston, medv ~ lstat + I(lstat ^ 2))
summary(lm_fit2)

lm_fit <- lm(data = Boston, medv ~ lstat)
anova(lm_fit, lm_fit2)

ggplot(data = lm_fit2) +
    geom_point(aes(x = predict(lm_fit2), y = residuals(lm_fit2)))

ggplot(data = lm_fit2) +
    geom_point(aes(x = predict(lm_fit2), y = rstudent(lm_fit2)))
