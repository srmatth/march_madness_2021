library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(janitor)

# Read in and clean the data
historical <- read_csv("data/historical_results.csv") %>%
  clean_names()

View(historical)

cleaned <- historical %>%
  mutate(
    day_of_week = weekdays(mdy(date)),
    is_upset = losing_seed < winning_seed,
    med_upset = winning_seed - losing_seed > 3,
    big_upset = winning_seed - losing_seed > 7,
    had_overtime = !is.na(overtime),
    margin_of_victory = winning_score - losing_score
  ) %>%
  select(-overtime)

## Create the model data
dat_1 <- cleaned %>%
  select(
    day_of_week,
    round,
    region,
    team_seed = winning_seed,
    opponent_seed = losing_seed,
    margin_of_victory
  )

dat_2 <- cleaned %>%
  select(
    day_of_week,
    round,
    region,
    team_seed = losing_seed,
    opponent_seed = winning_seed,
    margin_of_victory
  ) %>%
  mutate(margin_of_victory = margin_of_victory * (-1))

mod_dat <- bind_rows(dat_1, dat_2) %>%
  slice_sample(prop = 0.5)

lm <- lm(margin_of_victory ~ team_seed + opponent_seed, data = mod_dat)

summary(lm)

hist(lm$residuals)

preds <- predict(lm, mod_dat)

hist(preds)

wins <- preds > 0

real_wins <- mod_dat$margin_of_victory > 0

mean(wins == real_wins)
