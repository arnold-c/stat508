library(ISLR)
library(tidyverse)
library(hrbrthemes)
library(leaps)
library(glmnet)
library(pls)

RNGkind(sample.kind="Rounding") 
set.seed(1)

data("BostonHousing2")

# Remove the same variables as with best subset selection for direct comparisons
df <- BostonHousing2 %>%
    select(everything(), -c(medv, town, rm, lstat, nox, lon, lat))

x <- model.matrix(crim ~ ., df)[, -1]
y <- df$crim

train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]


# Best subset -------------------------------------------------------------

regfit_train <- regsubsets(
    crim ~ ., 
    data = df,
    subset = train,
    nvmax = 12
)

reg_summary <- summary(regfit_train)
reg_summary

which.min(reg_summary$cp)
which.min(reg_summary$bic)
which.max(reg_summary$adjr2)

reg_summary_df <- tibble(
    predictors = 1:11,
    "Adjusted R2" = reg_summary$adjr2,
    Cp = reg_summary$cp,
    BIC = reg_summary$bic
) %>%
    pivot_longer(cols = `Adjusted R2`:BIC, names_to = "method", values_to = "values")

# Add this to be able to highlight optimal number of variables in ggplot
reg_summary_df <- reg_summary_df %>%
    mutate(optimal = case_when(
        method == "Cp" & predictors == 4 ~ values,
        method == "BIC" & predictors == 2 ~ values,
        method == "Adjusted R2" & predictors == 5 ~ values,
        TRUE ~ NA_real_))

ggplot(data = reg_summary_df, 
       aes(x = predictors, y = values, color = method), show.legend = F) +
    geom_line(show.legend = F) +
    geom_point(aes(x = predictors, y = optimal), size = 3, show.legend = F) +
    facet_wrap(~method, scales = "free_y", nrow = 1) +
    labs(
        title = "Model Selection Methods by Predictors (training data)",
        x = "Number of Variables",
        y = "Score"
    ) +
    theme_ipsum_rc() +
    scale_x_continuous(breaks = round(seq(0, 10, by = 2), 1))

coef(regfit_train, 2)


regfit_best <- regsubsets(crim ~.,
                          data = df[train,])

test_mat <- model.matrix(crim ~.,
                         data = df[test,])

val_errors <- rep(NA, 12)

for(i in 1:12){
    coefi <- coef(regfit_best, id = i)
    pred <- test_mat[, names(coefi)] %*% coefi
    val_errors[i] <- mean((df$crim[test] - pred)^2)
}

val_errors
which.min(val_errors)
coef(regfit_best, 2)
plot(val_errors, type="b", 
     main = "MSE of `crim` in test data using best subset selection",
     xlab = "Number of parameters",
     ylab = "MSE")

# PCR ---------------------------------------------------------------------

pcr.fit.train <- pcr(crim ~ ., 
               data = df, 
               subset = train, 
               scale = TRUE, 
               validation = "CV")

# Significant reduction by 3 comps
# continues to decrease slightly above 9 comps
validationplot(pcr.fit.train, val.type = "MSEP",
               main = "10-fold cross-validation MSE for `crim` PCR (training data)")

pcr.pred <- predict(pcr.fit.train, x[test, ], ncomp = 3)

# MSE = 40.82
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(crim ~ ., data = df, scale = TRUE, ncomp = 3)

# 3 comps explains 39.9% of variance in crim in whole dataset
summary(pcr.fit)


# Partial Least Squares ---------------------------------------------------

pls.fit.train <- plsr(crim ~ ., 
                data = df, 
                subset = train, 
                scale = TRUE, 
                validation = "CV")

summary(pls.fit.train)

# Significant reduction by 5 comps and very little difference after
validationplot(pls.fit.train, val.type = "MSEP",
               main = "10-fold cross-validation MSE for `crim` PLS (training data)")

pls.pred <- predict(pls.fit.train, x[test, ], ncomp = 5)

# MSE = 38.27
mean((pls.pred - y.test)^2)

pls.fit <- plsr(crim ~ ., 
                data = df, 
                scale = TRUE, 
                ncomp = 5)

# 5 comps explains 44.4% of variance in crim in whole dataset
summary(pls.fit)


