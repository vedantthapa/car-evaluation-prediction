library(dplyr)
library(naniar)
library(ggplot2)
library(GGally)
library(patchwork)
library(viridis)
library(hrbrthemes)


outer_train = read.csv("~/Downloads/car-purchase/assets/data/outer_train.csv")

dim(outer_train)

# missing val plot
outer_train %>%
  vis_miss()

# checking for duplicates
outer_train[duplicated(outer_train) | duplicated(outer_train, fromLast = TRUE), ]

# individual distributions
p1 = outer_train %>% 
  count(price) %>% 
  ggplot(aes(n, price, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Price")
p1
p2 = outer_train %>% 
  count(maintenance) %>% 
  ggplot(aes(n, maintenance, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Maintenance")

p3 = outer_train %>% 
  count(doors) %>% 
  ggplot(aes(n, doors, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Doors")

p4 = outer_train %>% 
  count(safety) %>% 
  ggplot(aes(n, safety, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Safety")

p5 = outer_train %>% 
  count(storage) %>% 
  ggplot(aes(n, storage, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Storage")

p6 = outer_train %>% 
  count(seats) %>% 
  ggplot(aes(n, seats, fill = as.factor(n))) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Seats")

p7 = outer_train %>% 
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

((p1 + p2) / (p3 + p4) / (p5 + p6)) | p7


# relationship with target
p1 =  outer_train %>%
  group_by(price, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = price, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Price") + 
  theme(legend.position="none")

p2 =  outer_train %>%
  group_by(maintenance, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = maintenance, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Maintenance") + 
  theme(legend.position="none")

p3 =  outer_train %>%
  group_by(seats, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = seats, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Seats") +
  theme(legend.position="none")

p4 =  outer_train %>%
  group_by(safety, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = safety, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Safety") 

p5 =  outer_train %>%
  group_by(storage, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = storage, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Storage") + 
  theme(legend.position="none")

p6 =  outer_train %>%
  group_by(doors, shouldBuy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = shouldBuy, y = doors, x = n)) + 
  geom_bar(position = "fill", stat = "identity",  colour = "black", alpha = 0.75) + 
  scale_fill_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "", y = "",title = "Doors") + 
  theme(legend.position="none")

(p1 + p2) / (p3 + p4) / (p5 + p6)

# roc analysis
acc = roc(test$shouldBuy,test.probs[,1])
good = roc(test$shouldBuy,test.probs[,2])
unacc = roc(test$shouldBuy,test.probs[,3])
vgood = roc(test$shouldBuy,test.probs[,4])

acc.auc = round(auc(acc), 4)
good.auc = round(auc(good), 4)
unacc.auc = round(auc(unacc), 4)
vgood.auc = round(auc(vgood), 4)

p1 = ggroc(acc, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Label: Acc ', '(AUC = ', acc.auc, ')')) + 
  labs(x = "FPR (1-Specificity)", y = "TPR (Sensitivity)")

p2 = ggroc(good, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Label: Good ', '(AUC = ', good.auc, ')')) + 
  labs(x = "FPR (1-Specificity)", y = "TPR (Sensitivity)")

p3 = ggroc(unacc, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Label: Unacc ', '(AUC = ', unacc.auc, ')')) + 
  labs(x = "FPR (1-Specificity)", y = "TPR (Sensitivity)")

p4 = ggroc(vgood, colour = 'steelblue', size = 2) +
  ggtitle(paste0('Label: Vgood ', '(AUC = ', vgood.auc, ')')) + 
  labs(x = "FPR (1-Specificity)", y = "TPR (Sensitivity)")

(p1 + p2) / (p3 + p4)
