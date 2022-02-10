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

set.seed(42)
kfold = trainControl(method="cv",
                     number=10, 
                     classProbs = TRUE, 
                     summaryFunction = multiClassSummary)

# model_weights = ifelse(inner_train$shouldBuy == "acc",
#                        (1/table(inner_train$shouldBuy)[1]) * 0.25,
#                        ifelse(inner_train$shouldBuy == "good",
#                               (1/table(inner_train$shouldBuy)[2]) * 0.25,
#                               ifelse(inner_train$shouldBuy == "unacc",
#                                      (1/table(inner_train$shouldBuy)[3]) * 0.25,
#                                      (1/table(inner_train$shouldBuy)[4]) * 0.25)))


inner_train$is_high_maintenance = ifelse(inner_train$maintenance %in% c("vhigh", "high"), 1, 0)
valid$is_high_maintenance = ifelse(valid$maintenance %in% c("vhigh", "high"), 1, 0)


set.seed(42)
dt <- train(shouldBuy ~ .,
            data = inner_train,
            method = "rpart",
            parms = list(split="information"),
            trControl = kfold,
            metric = "Accuracy")

inner_train.pred = predict(dt, inner_train)
valid.pred = predict(dt, valid)

confusionMatrix(data = valid.pred,
                reference = as.factor(valid$shouldBuy))


valid.roc = multiclass.roc(valid$shouldBuy, as.numeric(valid.pred))
auc(valid.roc)


