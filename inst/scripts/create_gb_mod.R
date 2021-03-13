library(readr)
library(dplyr)
library(h2o)
library(caret)

mod_dat <- read_csv("data/mod_dat.csv") %>%
  mutate(
    is_higher_seed = as.factor(team_seed > opponent_seed),
    win = as.factor(win)# ,
    # round = as.factor(round),
    # region = as.factor(region),
    # day_of_week = as.factor(day_of_week)
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

gb_mod <- h2o.gbm(
  y = "win",
  training_frame = train,
  validation_frame = test,
  seed = 25,
  learn_rate = 0.01,
  learn_rate_annealing = 0.95,
  ntrees = 1000,
  max_depth = 2,
  distribution = "bernoulli",
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  histogram_type = "RoundRobin",
  categorical_encoding = "OneHotExplicit"
)

gb_mod

# Max accuracy
# 0.706
# 0.707

preds <- predict(gb_mod, test) %>%
  as.data.frame() %>%
  mutate(actual = test_dat$win)

mean(preds$predict == preds$actual)

contribs <- predict_contributions.H2OModel(gb_mod, test) %>%
  as.data.frame()
