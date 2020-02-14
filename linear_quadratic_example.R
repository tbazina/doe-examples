######## Simple quaratic dataset model ##################
## Libraries
# Cleaning data
library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(broom)

# Clean input data
dat <- as_tibble(ChickWeight)
dat <- dat %>% 
  mutate(chick = as.integer(Chick), diet = as.integer(Diet), day = Time) %>%
  group_by(Chick) %>%
  filter(n() >= 12) %>%
  ungroup() %>%
  select(weight, day, diet, chick) 

# EDA
dat %>% 
  mutate(diet = as.factor(diet), day.sq = day^2) %>%
  filter(between(chick, 34, 39) & day %in% c(0, 10, 20)) %>%
  group_by(diet) %>%
  do(fitDay = lm(weight ~ day, data = .),
     fitDay.sq = lm(weight ~ day.sq, data = .)) %>%
  # summary(.$fitDay.sq[[1]])
  summarise(diet = diet,
            # chick = chick,
            fit.day = summary(fitDay)$adj.r.squared,
            fit.day.sq = summary(fitDay.sq)$adj.r.squared
            ) %>%
  filter(fit.day.sq > fit.day)

# Visualize
dat %>%
  filter(between(chick, 34, 39) & day %in% c(0, 10, 20))
  mutate(chick = factor(chick, ordered=F)) %>%
  ggplot(aes(day, weight, )) + geom_point() + geom_smooth(method = "lm", formula = y ~ I(x^2))
