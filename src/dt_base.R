library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)
library(rpart.plot)

inner_train = read.csv("~/Downloads/car-purchase/assets/data/inner_train.csv")
valid = read.csv("~/Downloads/car-purchase/assets/data/valid.csv")

valid.true = valid$shouldBuy
valid = select(valid, -c(shouldBuy))

dt_base_base = rpart(shouldBuy ~ ., 
                     data = inner_train, 
                     method = "class",
                     parms = list(split = "information"))

inner_train.pred = predict(dt_base, inner_train, type = "class")
valid$shouldBuy = predict(dt_base, valid, type = "class")

valid.probs = predict(dt_base, valid)

valid$shouldBuy

confusionMatrix(data = valid$shouldBuy,
                reference = as.factor(valid.true))
confusionMatrix(data = inner_train.pred,
                reference = as.factor(inner_train$shouldBuy))

valid.roc = multiclass.roc(valid.true, as.numeric(valid.probs))
auc(valid.roc)


