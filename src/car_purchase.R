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
valid$shouldBuy = NA

set.seed(42)
kfold = trainControl(method="cv",
                     number=10,
                     verboseIter = TRUE,
                     summaryFunction = multiClassSummary)

mask.valid = (valid$safety == "low") | (valid$seats == 2)
mask.train = (inner_train$safety == "low") | (inner_train$seats == 2)

inner_train.rule = inner_train[mask.train, ]
inner_train.model = inner_train[!mask.train, ]

valid.rule = valid[mask.valid, ]
valid.model = valid[!mask.valid, ]

set.seed(42)
dt <- train(shouldBuy ~ .,
            data = inner_train.model,
            method = "rpart",
            parms = list(split="information"),
            trControl = kfold)

inner_train.model$shouldBuy = predict(dt, inner_train.model)
valid.model$shouldBuy = predict(dt, valid.model)

inner_train.rule$shouldBuy = rep("unacc", dim(inner_train.rule)[1])
valid.rule$shouldBuy = rep("unacc", dim(valid.rule)[1])

valid[row.names(valid.model), "shouldBuy"] = as.vector(valid.model$shouldBuy)
valid[row.names(valid.rule), "shouldBuy"] = valid.rule$shouldBuy

confusionMatrix(data = as.factor(valid$shouldBuy),
                reference = as.factor(valid.true))

valid.model$shouldBuy = predict(dt, valid.model, prob = TRUE)
valid.roc = multiclass.roc(response = valid.true[!mask.valid], predictor=as.numeric(valid.model$shouldBuy))
auc(valid.roc)




