library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)

data = read.csv("~/Downloads/car-purchase/assets/data/car_data.csv")
head(data)

set.seed(42)
outer_train.idx = createDataPartition(
  data$shouldBuy,
  times = 1,
  p = 0.95,
  list=FALSE
)
outer_train = data[outer_train.idx,]

set.seed(42)
inner_train.idx <- createDataPartition(
  outer_train$shouldBuy,
  times = 1,
  p = 0.85,
  list=FALSE
)
inner_train = outer_train[inner_train.idx,]
valid = outer_train[-inner_train.idx,]

valid.true = valid$shouldBuy
valid = select(valid, -c(shouldBuy))

set.seed(42)
kfold = trainControl(method="cv",
                     number=10, 
                     classProbs = TRUE, 
                     summaryFunction = multiClassSummary)

inner_train$is_high_maintenance = ifelse(inner_train$maintenance %in% c("vhigh", "high"), 1, 0)
valid$is_high_maintenance = ifelse(valid$maintenance %in% c("vhigh", "high"), 1, 0)


set.seed(42)
dt <- train(shouldBuy ~ .,
            data = inner_train,
            method = "rpart",
            parms = list(split="gini"),
            trControl = kfold,
            metric = "Accuracy")

inner_train.pred = predict(dt, inner_train)
valid$shouldBuy = predict(dt, valid)


confusionMatrix(data = valid$shouldBuy,
                reference = as.factor(valid.true))


valid.roc = multiclass.roc(valid.true, as.numeric(valid$shou))
auc(valid.roc)


