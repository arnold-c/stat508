library(ISLR)
library(tidyverse)
library(splines)
library(gam)
library(akima)

RNGkind(sample.kind = "Rounding")
set.seed(1)


# Q1 ----------------------------------------------------------------------

data(Wage)

# Fit polynomial regression
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit2))

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(summary(fit2))

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# Polynomial predictions
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# fit2 predictions
preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)

# Demonstrate fit and fit2 produce same predictions
max(abs(preds$fit - preds2$fit))

# ANOVA
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)

# p-values equate to diff between successive nested models e.g. 1 - 2, 2 - 3 ...
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# ANOVA inc. other variables
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)

anova(fit.1, fit.2, fit.3)

# t-statistic for last orthogonal polynomial
coef(summary(fit.3))
sqrt(4.4936)


# Q2 ----------------------------------------------------------------------

# confirm results
table(cut(age, 4))

fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# answer question
table(cut(age, c(0, 25, 40, 60, 80)))

fit <- lm(wage ~ cut(age, c(0, 25, 40, 60, 80)), data = Wage)
coef(summary(fit))

preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Step Function", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

table(preds$fit, age.grid)


# Q3 ----------------------------------------------------------------------
# Fit cubic regression spline (cubic is the default of bs() function)
cub_fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
cub_pred <- predict(cub_fit, newdata = list(age = age.grid), se = T)

plot(age, wage, col = "gray")
lines(age.grid, cub_pred$fit, lwd = 2)
lines(age.grid, cub_pred$fit + 2 * cub_pred$se, lty = "dashed")
lines(age.grid, cub_pred$fit - 2 * cub_pred$se, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))

# Allow R to choose knots based on percentiles of age instead of specifying
attr(bs(age, df = 6), "knots")

# Fit natural regression spline
nat_fit <- lm(wage ~ ns(age, df = 4), data = Wage)
nat_pred <- predict(nat_fit, newdata = list(age = age.grid), se = T)

plot(age, wage, col = "gray")
lines(age.grid, nat_pred$nat_fit, col = "red", lwd = 2)

# Fit smoothing spline
fit <- smooth.spline(age, wage, df = 16)
# Use CV to determine lambda and therefore df (cv=F = generalized, T = LOOCV)
smooth_fit <- smooth.spline(age, wage, cv = FALSE) 

smooth_fit$df

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
lines(fit, col = "red", lwd = 2)
lines(smooth_fit, col = "blue", lwd = 2)
legend(
  "topright",
  legend = c("16 DF", "6.8 DF"),
  col = c("red", "blue"),
  lty = 1,
  lwd = 2,
  cex = .8
)

# Local regression
fit <- loess(wage ~ age, span = .2, data = Wage)
local_fit <- loess(wage ~ age, span = .5, data = Wage)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
lines(
  age.grid, predict(fit, data.frame(age = age.grid)),
  col = "red", lwd = 2
)
lines(
  age.grid, predict(local_fit, data.frame(age = age.grid)),
  col = "blue", lwd = 2
)
legend(
  "topright",
  legend = c("Span=0.2", "Span=0.5"),
  col = c("red", "blue"),
  lty = 1,
  lwd = 2,
  cex = .8
)

predict(cub_fit, data.frame(age = 80))
predict(nat_fit, data.frame(age = 80))
predict(local_fit, data.frame(age = 80))
smooth_fit$y[smooth_fit$x==80]


# Q4 ----------------------------------------------------------------------

# Fit GAM with natural spline
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# Fit GAMs with smoothing spline
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

# Have to use plot.Gam as gam1 is an lm class
plot.Gam(gam1, se = TRUE, col = "red")

# Testing best form of GAM - M2 is best based on ANOVA
anova(gam.m1, gam.m2, gam.m3, test = "F")

summary(gam.m3)

#Answer q4
broom::tidy(coef(gam.m2))

par(mfrow = c(1, 3))
plot(gam.m2, se = TRUE, col = "blue")

# Q5 ----------------------------------------------------------------------

round(
  predict(
    gam.m2,
    data.frame(year = 2006, age = 50, education = "4. College Grad")
  ),
  0
)

# Rest of ISLR Lab 7.8.3 --------------------------------------------------

# Predictions using GAMs
preds <- predict(gam.m2, newdata = Wage)

# Fit local regression GAM
gam.lo <- gam(
  wage ~ s(year, df = 4) + lo(age, span = 0.7) + education,
  data = Wage
)

plot.Gam(gam.lo, se = TRUE, col = "green")

# Fit local regression GAM with interaction
gam.lo.i <- gam(
    wage ~ lo(year, age, span = 0.5) + education,
    data = Wage
    )

plot(gam.lo.i)

# Fit logistic regression GAM
gam.lr <- gam(
  I(wage 250) ~ year + s(age, df = 5) + education,
  family = binomial,
  data = Wage
)

par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

table(education, I(wage > 250))

# Refit LR GAM as no high earners in <HS category
gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education,
  family = binomial,
  data = Wage,
  subset = (education != "1. < HS Grad")
)

plot(gam.lr.s, se = T, col = "green")
