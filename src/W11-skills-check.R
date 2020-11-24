library(ISLR)
library(tidyverse)
library(tree)
library(caret)

RNGkind(sample.kind = "Rounding")
set.seed(1)

# Q1 ----------------------------------------------------------------------

attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")

Carseats <- data.frame(Carseats, High)
Carseats$High <- as.factor(Carseats$High)

tree.carseats <- tree(High ~ . - Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats


# Q2 ----------------------------------------------------------------------


set.seed(2)

train <- sample(1:nrow(Carseats), 200)

Carseats.test <- Carseats[-train, ]

High.test <- High[-train]

tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)

tree.pred <- predict(tree.carseats, Carseats.test, type = "class")

table(tree.pred, High.test)

confusionMatrix(factor(tree.pred), factor(High.test), positive = "Yes")

set.seed(3)

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats, best = 9)

par(mfrow = c(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

prune.carseats

# Q3 ----------------------------------------------------------------------


tree.pred <- predict(prune.carseats, Carseats.test, type = "class")

table(tree.pred, High.test)
confusionMatrix(factor(tree.pred), factor(High.test), positive = "Yes")

prune.carseats <- prune.misclass(tree.carseats, best = 15)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
confusionMatrix(factor(tree.pred), factor(High.test), positive = "Yes")

prune.carseats <- prune.misclass(tree.carseats, best = 13)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
confusionMatrix(factor(tree.pred), factor(High.test), positive = "Yes")
