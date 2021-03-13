library(readr)
library(dplyr)
library(h2o)
library(caret)

mod_dat <- read_csv("data/mod_dat.csv") %>%
  mutate(
    win = as.factor(win),
    round = as.factor(round),
    region = as.factor(region),
    day_of_week = as.factor(day_of_week)
  )

set.seed(25)

train_ind <- createDataPartition(mod_dat$win, p = 0.8)

train_dat <- mod_dat %>%
  slice(train_ind[[1]])
test_dat <- mod_dat %>%
  slice(-train_ind[[1]])

h2o.init()

train <- as.h2o(train_dat)
test <- as.h2o(test_dat)

rf_mod <- h2o.randomForest(
  y = "win",
  training_frame = train,
  validation_frame = test,
  mtries = 3,
  ntrees = 1000,
  max_depth = 10,
  min_split_improvement = 0.1,
  sample_rate = 0.7,
  seed = 25,
  binomial_double_trees = TRUE
)

rf_mod

# first try, teset logloss at 0.67
# second try 0.57
# 0.57 
# 0.56

# Max accuracy
# 0.69
# 0.702

preds <- predict(rf_mod, test) %>%
  as.data.frame() %>%
  mutate(actual = test_dat$win)

mean(preds$predict == preds$actual)
