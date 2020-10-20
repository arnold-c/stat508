library(ISLR)
library(pls)

RNGkind(sample.kind="Rounding") 

# Data Pre-Processing -----------------------------------------------------

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ ., Hitters)[, -1]

y <- Hitters$Salary

train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

y.test <- y[test]


# Q1 ----------------------------------------------------------------------
set.seed(2)

pcr.fit <- pcr(Salary ~ ., 
               data = Hitters, 
               scale = TRUE, 
               validation = "CV")

summary(pcr.fit)


# Q2 ----------------------------------------------------------------------

validationplot(pcr.fit, val.type = "MSEP")

MSEP(pcr.fit)


# Q3 ----------------------------------------------------------------------

set.seed(1)

pls.fit <- plsr(Salary ~ ., 
                data = Hitters, 
                subset = train, 
                scale = TRUE, 
                validation = "CV")

summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)

mean((pls.pred - y.test)^2)

pls.fit <- plsr(Salary ~ ., 
                data = Hitters, 
                scale = TRUE, 
                ncomp = 2)

summary(pls.fit)

pls.fit <- plsr(Salary ~ ., 
                data = Hitters, 
                scale = TRUE, 
                ncomp = 4)

summary(pls.fit)
