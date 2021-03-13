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

# some basic visualizations
cleaned %>%
  group_by(day_of_week) %>%
  summarize(pct_upset = mean(med_upset)) %>%
  ggplot() +
  aes(x = day_of_week, y = pct_upset) +
  geom_bar(stat = "identity")

cleaned %>%
  mutate(difference_in_seed = abs(winning_seed - losing_seed)) %>%
  ggplot() +
  aes(x = difference_in_seed, y = margin_of_victory, color = day_of_week) +
  geom_point() +
  geom_jitter()

cleaned %>%
  group_by(region) %>%
  summarize(pct_upset = mean(big_upset)) %>%
  ggplot()+
  aes(x = region, y = pct_upset) +
  geom_bar(stat = "identity") +
  coord_flip()


