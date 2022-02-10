library(dplyr)

inner_train = read.csv("~/Downloads/car-purchase/assets/data/trainCar.csv")
valid = read.csv("~/Downloads/car-purchase/assets/data/validCar.csv")
dim(inner_train)

inner_train %>%
  group_by(shouldBuy) %>%
  summarise(n=n())

inner_train %>%
  group_by(shouldBuy, safety) %>%
  summarise(n=n())

inner_train %>%
  group_by(shouldBuy, seats) %>%
  summarise(n=n())

valid.dummy = valid[, !(names(valid) %in% c("shouldBuy"))]

dim(valid)
dim(valid.dummy)

valid.dummy$shouldBuy = NA
valid.dummy[(valid.dummy$safety == "low") | (valid.dummy$seats == 2), "shouldBuy"] = "unacc"

valid.subset = valid.dummy[is.na(valid.dummy$shouldBuy), ]
valid.subset

dim(valid.subset)
dim(valid.dummy[!((valid.dummy$safety == "low") | (valid.dummy$seats == 2)), ])


inner_train.rule = inner_train[(inner_train$safety == "low") | (inner_train$seats == 2), ]
inner_train.model = inner_train[!((inner_train$safety == "low") | (inner_train$seats == 2)), ]

valid.rule = valid[(valid$safety == "low") | (valid$seats == 2), ]
valid.model = valid[!((valid$safety == "low") | (valid$seats == 2)), ]

dim(inner_train)
dim(valid)

dim(inner_train.rule)
dim(inner_train.model)

dim(valid.rule)
dim(valid.model)

