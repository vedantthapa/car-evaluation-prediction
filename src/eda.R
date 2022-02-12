library(dplyr)
library(naniar)
library(ggplot2)
library(GGally)
library(patchwork)

inner_train = read.csv("~/Downloads/car-purchase/assets/data/trainCar.csv")
valid = read.csv("~/Downloads/car-purchase/assets/data/validCar.csv")
dim(inner_train)

# missing val plot
inner_train %>%
  vis_miss()

# checking for duplicates
inner_train[duplicated(inner_train) | duplicated(inner_train, fromLast = TRUE), ]

# individual distributions
p1 = inner_train %>% 
  count(price) %>% 
  ggplot(aes(n, price, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Price")

p2 = inner_train %>% 
  count(maintenance) %>% 
  ggplot(aes(n, maintenance, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Maintenance")

p3 = inner_train %>% 
  count(doors) %>% 
  ggplot(aes(n, doors, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Doors")

p4 = inner_train %>% 
  count(safety) %>% 
  ggplot(aes(n, safety, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Safety")

p5 = inner_train %>% 
  count(storage) %>% 
  ggplot(aes(n, storage, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Storage")

p6 = inner_train %>% 
  count(shouldBuy) %>% 
  ggplot(aes(shouldBuy, n, fill = as.factor(n))) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "shouldBuy", y = "", title = "Target")

p6 = inner_train %>% 
  count(shouldBuy) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = shouldBuy, y = pct, fill = as.factor(n), label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = "shouldBuy", y = "", title = "Target")

((p1 + p2) / (p3 + p4) / p5) | p6

##########################################################


