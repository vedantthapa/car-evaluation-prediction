library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)
library(rpart.plot)


# testing on the test set
test = read.csv("~/Downloads/car-purchase/assets/data/test.csv")
dt = readRDS("~/Downloads/car-purchase/assets/models/dt_tuned.rds")

test.preds = predict(dt, test)
confusionMatrix(data = test.preds,
                reference = as.factor(test$shouldBuy))
test.roc = multiclass.roc(test$shouldBuy, as.numeric(test.preds))
auc(test.roc)
