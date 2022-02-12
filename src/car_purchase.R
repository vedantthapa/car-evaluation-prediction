library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)
library(rpart.plot)

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
test = data[-outer_train.idx, ]

write.csv(outer_train, "~/Downloads/car-purchase/assets/data/outer_train.csv", row.names = F)
write.csv(test, "~/Downloads/car-purchase/assets/data/test.csv", row.names = F)

set.seed(42)
inner_train.idx <- createDataPartition(
  outer_train$shouldBuy,
  times = 1,
  p = 0.85,
  list=FALSE
)
inner_train = outer_train[inner_train.idx,]
valid = outer_train[-inner_train.idx,]

write.csv(inner_train, "~/Downloads/car-purchase/assets/data/inner_train.csv", row.names = F)
write.csv(valid, "~/Downloads/car-purchase/assets/data/valid.csv", row.names = F)