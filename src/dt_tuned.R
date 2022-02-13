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

set.seed(42)
kfold = trainControl(method="cv",
                     number=10, 
                     classProbs = TRUE, 
                     summaryFunction = multiClassSummary)

set.seed(42)
dt <- train(x = inner_train[-7],
            y = inner_train$shouldBuy,
            method = "rpart",
            parms = list(split="information"),
            trControl = kfold,
            tuneLength = 15)

inner_train.pred = predict(dt, inner_train)
valid$shouldBuy = predict(dt, valid)


confusionMatrix(data = valid$shouldBuy,
                reference = as.factor(valid.true))
confusionMatrix(data = inner_train.pred,
                reference = as.factor(inner_train$shouldBuy))

valid.roc = multiclass.roc(valid.true, as.numeric(valid$shouldBuy))
auc(valid.roc)

rpart.plot(dt$finalModel)

saveRDS(dt, "~/Downloads/car-purchase/assets/models/dt_tuned.rds")
