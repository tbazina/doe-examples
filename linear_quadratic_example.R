######## Simple quaratic dataset model ##################
## Libraries
# Cleaning data
library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(broom)
# Visualization
library(ggplot2)
# DoE
library(DoE.wrapper)

################################################################################
# Clean input data (and simulated response)
dat <- as_tibble(ChickWeight)
dat <- dat %>% 
  mutate(chick = as.integer(Chick), diet = as.integer(Diet), day = Time) %>%
  group_by(Chick) %>%
  filter(n() >= 12) %>%
  ungroup() %>%
  filter(between(chick, 34, 39) & day %in% c(0, 10, 20)) %>%
  select(diet, day, weight)

################################################################################
# CRD - design
set.seed(42)
dat <- dat %>%
  sample_n(n()) %>%
  mutate(chick = 1:n()) %>%
  select(chick, day, weight)
exp.design <- dat %>% select(-weight)
exp.design %>%
  write.table(file = 'CRD_Chick_18_3_6.csv', sep = ";", dec = ",")

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
  filter(between(chick, 34, 39) & day %in% c(0, 10, 20)) %>%
  mutate(chick = factor(chick, ordered=F)) %>%
  ggplot(aes(day, weight, )) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x^2))
