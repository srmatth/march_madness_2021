library(readr)
library(dplyr)
library(h2o)
library(caret)

mod_dat <- read_csv("data/mod_dat.csv") %>%
  mutate(
    difference_in_seed = team_seed - opponent_seed,
    win = as.factor(win)
  ) %>%
  select(-region, -round, -day_of_week)

set.seed(25)

train_ind <- createDataPartition(mod_dat$win, p = 0.8)

train_dat <- mod_dat %>%
  slice(train_ind[[1]])
test_dat <- mod_dat %>%
  slice(-train_ind[[1]])

lm <- glm(win ~ difference_in_seed, data = train_dat, family = "binomial")

summary(lm)


lm_preds <- predict(lm, test_dat, type = "response") %>%
  as.data.frame()%>%
  magrittr::set_colnames("probability") %>%
  mutate(prediction = as.numeric(probability > 0.5))

mean(lm_preds$prediction == test_dat$win)

mod_dat %>%
  ggplot() +
  aes(x = difference_in_seed, y = as.numeric(win)) +
  geom_point() +
  geom_jitter()
