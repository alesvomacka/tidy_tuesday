# Packages ----------------------------------------------------------------

library(tidyverse)
library(mice)
library(splines)
library(ggeffects)
library(DHARMa)
library(lme4)
library(brms)

# Data import -------------------------------------------------------------

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')

# Descriptivs -------------------------------------------------------------

ultra_rankings %>% 
  summarise(unique_races       = length(unique(race_year_id)),
            unique_runners     = length(unique(runner)),
            unique_nationality = length(unique(nationality)),
            prop_female        = length(gender[gender == "W"]) / length(gender),
            mean_age           = mean(age),
            mean_seconds       = mean(time_in_seconds, na.rm = TRUE),
            mean_minutes       = mean_seconds / 60,
            missings_time      = sum(is.na(time_in_seconds)) / length(time_in_seconds))

ultra_rankings %>% 
  select(age, time_in_seconds) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(min = min(value, na.rm = TRUE),
            max  = max(value, na.rm = TRUE))


hist(ultra_rankings$age)

# Data cleaning -----------------------------------------------------------

ultra_rankings$time_in_minutes <- ultra_rankings$time_in_seconds / 60
ultra_rankings <- filter(ultra_rankings, age > 18 & age < 80)
