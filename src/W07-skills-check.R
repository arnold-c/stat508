library(ISLR)
library(MASS)
library(class)

RNGkind(sample.kind = "Rounding")
set.seed(1)


# Data Description --------------------------------------------------------

names(Smarket)

dim(Smarket)

summary(Smarket)

pairs(Smarket)

cor(Smarket[,-9])

attach(Smarket)

plot(Volume)


# Q1 ----------------------------------------------------------------------

train <- (Year < 2005)

Smarket.2005 <- Smarket[!train, ]

dim(Smarket.2005)

Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)

glm.probs <- predict(glm.fit, Smarket.2005, type = "response")

glm.pred <- rep("Down", 252)

glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, Direction.2005)

# Sensitivity (Up = +)
106 / (106 + 35)


# Q2 ----------------------------------------------------------------------

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

lda.pred <- predict(lda.fit, Smarket.2005)

lda.class <- lda.pred$class

table(lda.class, Direction.2005)

# Specificity (Up = +)
35 / (35 + 76)


# Q3 ----------------------------------------------------------------------

plot(lda.pred$posterior)

range(lda.pred$posterior[, 1])


# Q4 ----------------------------------------------------------------------

set.seed(1)
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

qda.fit

qda.class <- predict(qda.fit,Smarket.2005)$class

table(qda.class, Direction.2005)

# PPV (Up = +)
121 / (121 + 81)


# Q5 ----------------------------------------------------------------------

train.X <- cbind(Lag1, Lag2)[train, ]

test.X <- cbind(Lag1, Lag2)[!train, ]

train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)

table(knn.pred, Direction.2005)
