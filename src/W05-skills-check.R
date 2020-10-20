# Set Up ------------------------------------------------------------------
library(ISLR)
library(glmnet)
library(tidyverse)

RNGkind(sample.kind = "Rounding")
set.seed(1)


# Q1 ----------------------------------------------------------------------

states <- row.names(USArrests)
states

names(USArrests)

apply(USArrests, 2, mean)

apply(USArrests, 2, var)

pr.out <- prcomp(USArrests, scale = FALSE)

pr.out$rotation

# Q2 ----------------------------------------------------------------------

pr.out <- prcomp(USArrests, scale = TRUE)

pr.out$x

# Q3 ----------------------------------------------------------------------

pr.out$center

pr.out$scale

pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)

pr.out$rotation <- -pr.out$rotation

pr.out$x <- -pr.out$x

biplot(pr.out, scale = 0)

pr.out$sdev

pr.var <- pr.out$sdev^2

sum(pr.var) == sum(apply(scale(USArrests), 2, var))

sum(pr.var)
sum(apply(scale(USArrests), 2, var))


# Q4 ----------------------------------------------------------------------

cumsum(pve)

summary(pr.out)
