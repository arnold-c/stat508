library(ISLR)
library(tidyverse)
library(leaps)
library(boot)
library(hrbrthemes)
library(gridExtra)

set.seed(1)


# Q1 ----------------------------------------------------------------------

X <- rnorm(n = 100)
eps <- rnorm(n = 100)


# Q2 ----------------------------------------------------------------------

b0 <- 0
b1 <- 3
b2 <- 2
b3 <- 1

Y <- b0 + (b1 * X) + (b2 * X^2) + (b3 * X^3) + eps

df <- data.frame("X" = X, "Y" = Y)

# Data Summary ------------------------------------------------------------

empty <- ggplot() +
    geom_point(aes(1, 1), colour = "white") +
    theme(
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()
    )

scatter <- ggplot(data = df) +
    geom_point(aes(x = X, y = Y)) +
    scale_fill_ipsum()

hist_top <- ggplot(data = df) +
    geom_histogram((aes(x = X))) + 
    scale_fill_ipsum()

hist_right <- ggplot(data = df) +
    geom_histogram((aes(x = Y))) + 
    coord_flip() +
    scale_fill_ipsum()  

grid.arrange(hist_top, empty, scatter, hist_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))


# Q3 ----------------------------------------------------------------------

regfit_full <- regsubsets(
  Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10),
  data = df,
  nvmax = 10
)

reg_summary <- summary(regfit_full)
reg_summary

which.min(reg_summary$cp)
which.min(reg_summary$bic)
which.max(reg_summary$adjr2)


reg_summary_df <- tibble(
    predictors = 1:10,
    "Adjusted R2" = reg_summary$adjr2,
    Cp = reg_summary$cp,
    BIC = reg_summary$bic
) %>%
    pivot_longer(cols = `Adjusted R2`:BIC, names_to = "method", values_to = "values")

# Add this to be able to highlight optimal number of variables in ggplot
reg_summary_df <- reg_summary_df %>%
    mutate(optimal = case_when(
        method == "Cp" & predictors == 4 ~ values,
        method == "BIC" & predictors == 3 ~ values,
        method == "Adjusted R2" & predictors == 4 ~ values,
        TRUE ~ NA_real_))

ggplot(data = reg_summary_df, 
       aes(x = predictors, y = values, color = method), show.legend = F) +
    geom_line(show.legend = F) +
    geom_point(aes(x = predictors, y = optimal), size = 3, show.legend = F) +
    facet_wrap(~method, scales = "free_y", nrow = 1) +
    labs(
        title = "Model Selection Methods by Predictors",
        x = "Number of Variables",
        y = "Score"
    ) +
    theme_ipsum_rc() +
    scale_x_continuous(breaks = round(seq(0, 10, by = 2), 1))

# Best model has 4 variables according to Cp and Adjusted R2. Only 3 variables
# according to BIC (which is more conservative and favours simpler models). Will
# use 4 variables as satisfies two of the methods

coef(regfit_full, 4)

# Q4 ----------------------------------------------------------------------

# Forwards Selection
regfit_fwd <-
  regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10),
    data = df,
    nvmax = 10,
    method = "forward"
  )

reg_fwd_summary <- summary(regfit_fwd)

which.min(reg_fwd_summary$cp)
which.min(reg_fwd_summary$bic)
which.max(reg_fwd_summary$adjr2)


reg_fwd_summary_df <- tibble(
    predictors = 1:10,
    "Adjusted R2" = reg_fwd_summary$adjr2,
    Cp = reg_fwd_summary$cp,
    BIC = reg_fwd_summary$bic
) %>%
    pivot_longer(cols = `Adjusted R2`:BIC, names_to = "method", values_to = "values")

## Add this to be able to highlight optimal number of variables in ggplot
reg_fwd_summary_df <- reg_fwd_summary_df %>%
    mutate(optimal = case_when(
        method == "Cp" & predictors == 4 ~ values,
        method == "BIC" & predictors == 3 ~ values,
        method == "Adjusted R2" & predictors == 4 ~ values,
        TRUE ~ NA_real_))

ggplot(data = reg_fwd_summary_df, 
       aes(x = predictors, y = values, color = method), show.legend = F) +
    geom_line(show.legend = F) +
    geom_point(aes(x = predictors, y = optimal), size = 3, show.legend = F) +
    facet_wrap(~method, scales = "free_y", nrow = 1) +
    labs(
        title = "Forwards Selection Model Selection Methods by Predictors",
        x = "Number of Variables",
        y = "Score"
    ) +
    theme_ipsum_rc() +
    scale_x_continuous(breaks = round(seq(0, 10, by = 2), 1))

# Best model has 4 variables according to Cp and Adjusted R2. Only 3 variables
# according to BIC (which is more conservative and favours simpler models). Will
# use 4 variables as satisfies two of the methods

coef(regfit_fwd, 4)

# Backwards Selection
regfit_bwd <-
    regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10),
               data = df,
               nvmax = 10,
               method = "backward"
    )

reg_bwd_summary <- summary(regfit_bwd)

which.min(reg_bwd_summary$cp)
which.min(reg_bwd_summary$bic)
which.max(reg_bwd_summary$adjr2)

reg_bwd_summary_df <- tibble(
    predictors = 1:10,
    "Adjusted R2" = reg_bwd_summary$adjr2,
    Cp = reg_bwd_summary$cp,
    BIC = reg_bwd_summary$bic
) %>%
    pivot_longer(cols = `Adjusted R2`:BIC, names_to = "method", values_to = "values")

## Add this to be able to highlight optimal number of variables in ggplot
reg_bwd_summary_df <- reg_bwd_summary_df %>%
    mutate(optimal = case_when(
        method == "Cp" & predictors == 6 ~ values,
        method == "BIC" & predictors == 6 ~ values,
        method == "Adjusted R2" & predictors == 8 ~ values,
        TRUE ~ NA_real_))

ggplot(data = reg_bwd_summary_df, 
       aes(x = predictors, y = values, color = method), show.legend = F) +
    geom_line(show.legend = F) +
    geom_point(aes(x = predictors, y = optimal), size = 3, show.legend = F) +
    facet_wrap(~method, scales = "free_y", nrow = 1) +
    labs(
        title = "Backwards Selection Model Selection Methods by Predictors",
        x = "Number of Variables",
        y = "Score"
    ) +
    theme_ipsum_rc() +
    scale_x_continuous(breaks = round(seq(0, 10, by = 2), 1))

# Best model has 6 variables according to Cp and BIC. 8 variables according 
# to Adjusted R2. Will use 4 variables as satisfies two of the methods and is
# simpler, reducing risk of over-fitting.

coef(regfit_bwd, 6)




