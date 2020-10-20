library(ISLR)
library(tidyverse)
library(caret)
library(MASS)
library(class)

RNGkind(sample.kind = "Rounding")
set.seed(1)

data("Weekly")
glimpse(Weekly)

# Data Summary ------------------------------------------------------------

summary(Weekly)

pairs(Weekly[, -9])
abs(cor(Weekly[, -9]))

plot(Volume)

Weekly %>%
  filter(lead(Lag1) != Today) %>%
  nrow()

Weekly$Week <- 1:nrow(Weekly)

year_breaks <- Weekly %>%
  group_by(Year) %>%
  summarize(Week = min(Week))

ggplot(Weekly, aes(x = Week, y = Volume)) +
  geom_line() +
  geom_smooth() +
  scale_x_continuous(
    breaks = year_breaks$Week,
    minor_breaks = NULL,
    labels = year_breaks$Year
  ) +
  labs(
    title = "Average Daily Shares Traded vs Time",
    x = "Time"
  ) +
  theme_light()

ggplot(Weekly, aes(x = Year, fill = Direction)) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.5, col = "grey45") +
  scale_x_continuous(breaks = seq(1990, 2010), minor_breaks = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_light() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle("% of Up/Down Weeks vs Time")

prop.table(table(Weekly$Direction))

ggplot(Weekly, aes(x = Week, y = Today / 100)) +
  geom_line() +
  scale_x_continuous(
    breaks = year_breaks$Week,
    minor_breaks = NULL,
    labels = year_breaks$Year
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(-0.2, 0.2, 0.05)
  ) +
  geom_hline(yintercept = 0, col = "grey55") +
  theme_light() +
  labs(
    title = "Weekly Percentage Return vs Time",
    x = "Time",
    y = "Percentage Return"
  )


# Logistic Regression -----------------------------------------------------

glm_dir <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Weekly,
  family = "binomial"
)

summary(glm_dir)


# Confusion Matrix --------------------------------------------------------

predicted <- factor(
  ifelse(
    predict(glm_dir, type = "response") < 0.5,
    "Down",
    "Up"
  )
)

confusionMatrix(predicted, Weekly$Direction, positive = "Up")

prop.table(table(predicted))


# Logistic Regression - test train ----------------------------------------

train <- Weekly[Weekly$Year <= 2008, ]
test <- Weekly[Weekly$Year > 2008, ]

glm_dir <- glm(Direction ~ Lag2,
  data = train,
  family = "binomial"
)

predicted <- factor(
  ifelse(
    predict(glm_dir, newdata = test, type = "response") < 0.5,
    "Down",
    "Up"
  )
)

confusionMatrix(predicted, test$Direction, positive = "Up")


# LDA - test train --------------------------------------------------------

lda_dir <- lda(Direction ~ Lag2, data = train)


predicted_lda <- predict(lda_dir, newdata = test)

confusionMatrix(
  data = predicted_lda$class,
  reference = test$Direction,
  positive = "Up"
)

identical(
  as.character(predicted_lda$class),
  as.character(ifelse(predicted_lda$posterior[, 2] < 0.5, "Down", "Up"))
)


# QDA - test train --------------------------------------------------------

qda_dir <- qda(Direction ~ Lag2, data = train)


predicted_qda <- predict(qda_dir, newdata = test)

confusionMatrix(
  data = predicted_qda$class,
  reference = test$Direction,
  positive = "Up"
)


# KNN - test train --------------------------------------------------------

test[100, "Lag2"]
train[c(10, 808), c("Lag2", "Direction")]

predicted_knn <- knn(
  train = data.frame(Lag2 = train$Lag2),
  test = data.frame(Lag2 = test$Lag2),
  cl = train$Direction,
  k = 1,
  prob = T
)

attr(predicted_knn, "prob")[100]

predicted_knn[100]

confusionMatrix(
  data = predicted_knn,
  reference = test$Direction,
  positive = "Up"
)


# Best Performing Classifier ----------------------------------------------

# kNN

train$Today <- NULL

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

set.seed(111)

knn_train <- train(
  y = train$Direction,
  x = train[, -8],
  method = "knn",
  metric = "Accuracy",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(k = seq(1, 50, 2)),
  trControl = ctrl
)

varImp(knn_train)

knn_train

ggplot(knn_train) +
  geom_smooth() +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("KNN - 'K' Selection (5-repeated 5-fold cross-validation)")

knn_pred <- predict(knn_train, newdata = test)

confusionMatrix(
  data = knn_pred,
  reference = test$Direction,
  positive = "Up"
)
