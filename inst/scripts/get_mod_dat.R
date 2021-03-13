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
    opponent_seed = losing_seed
  ) %>%
  mutate(win = 1)

dat_2 <- cleaned %>%
  select(
    day_of_week,
    round,
    region,
    team_seed = losing_seed,
    opponent_seed = winning_seed
  ) %>%
  mutate(win = 0)

mod_dat <- bind_rows(dat_1, dat_2)

write_csv(mod_dat, "data/mod_dat.csv")
