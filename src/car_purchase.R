library(dplyr)
library(ggplot2)
library(GGally)


data = read.csv("~/Downloads/car-purchase/assets/data/car-data.csv")
head(data)

unique(data$shouldBuy)

data %>%
  group_by(shouldBuy) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

rapply(data,function(x)length(unique(x)))

head(data)
dim(data)
