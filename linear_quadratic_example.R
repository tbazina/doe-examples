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
dat %>%
  write.table(file = 'CRD_Chick_18_3_6_results.csv', sep = ';', dec = ',')
exp.design <- dat %>% select(-weight)
exp.design %>%
  write.table(file = 'CRD_Chick_18_3_6.csv', sep = ";", dec = ",")

################################################################################
# EDA
## Average, sd
dat %>% group_by(day) %>% summarise(
  mean = mean(weight),
  sd = sd(weight)
) %>% View()
## Histogram
dat %>%
  ggplot(aes(x=weight)) +
  geom_histogram(bins = 5,
                 color="black", fill="dodgerblue3",
                 ) +
  geom_vline(aes(xintercept=mean(weight)), linetype="dashed", size=1) +
  theme(
    axis.line = element_line(size=0.5, colour = "black")
  )
  
qqnorm()

## DoE Scatter plot
dat %>% mutate(mean = mean(weight)) %>%
  ggplot(aes(x = day, y = weight)) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 0.3) +
  geom_hline(aes(yintercept = mean)) +
  scale_y_continuous(
    name = 'Masa [g]',
    breaks = seq(0, 400, 50),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    breaks = seq(0, 20, 10)) +
  ggtitle("DoE dijagram rasipanja") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/doe_scatter_plot.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

## DoE mean plot
dat %>% 
  group_by(day) %>% summarise(weight.mean = mean(weight)) %>%
  ggplot(aes(x = day, y = weight.mean)) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 2, stroke = 1) +
  geom_line(
    color = 'black',
    linetype = '1434',
    size = 0.9) +
  scale_y_continuous(
    name = 'Prosjek mase [g]',
    breaks = seq(0, 400, 50),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    breaks = seq(0, 20, 10)) +
  ggtitle("DoE dijagram prosjeka") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/doe_mean_plot.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

## DOE Standard Deviation Plot
dat %>% 
  group_by(day) %>% summarise(weight.sd = sd(weight)) %>%
  ggplot(aes(x = day, y = weight.sd)) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 2, stroke = 1) +
  geom_line(
    color = 'black',
    linetype = '1434',
    size = 0.9) +
  scale_y_continuous(
    name = 'Standardna devijacija mase [g]',
    breaks = seq(0, 40, 5),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    breaks = seq(0, 20, 10)) +
  ggtitle("DoE dijagram st. dev.") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/doe_sd_plot.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

## Q-Q plot
dat %>%
  ggplot(aes(sample=weight)) +
  stat_qq(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 2, stroke = 0.3) +
  stat_qq_line(distribution = qgeom) + theme(
    axis.line = element_line(size=0.5, colour = "black")
  )

## Box-plot
dat %>% mutate(day = ordered(day)) %>%
  ggplot(aes(x = day, y = weight, fill=day)) +
  geom_boxplot(width = 0.3, alpha = 0.7) + 
  geom_dotplot(
    colour = "black", stroke = 2,
    binaxis='y', stackdir='center', dotsize=1, binwidth = 6) +
  scale_y_continuous(
    name = "Weight",
    breaks = seq(0, 400, 50),
    limits=c(0, NA)
    ) +
  scale_x_discrete(name = "Day") +
  ggtitle("Box-plot") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(face="bold"),
    axis.text.x = element_text(colour="black", size = 11),
    axis.text.y = element_text(colour="black", size = 9),
    axis.line = element_line(size=0.5, colour = "black")
  )
  

dat %>% 
  mutate(
    # diet = as.factor(diet),
    day.sq = day^2
    ) %>%
  # filter(between(chick, 34, 39) & day %in% c(0, 10, 20)) %>%
  # group_by(diet) %>%
  do(fitDay = lm(weight ~ day, data = .),
     fitDay.sq = lm(weight ~ day.sq, data = .)) %>% glance(fitDay.sq) 
  # summary(.$fitDay.sq[[1]])
  summarise(
    # diet = diet,
    # chick = chick,
    fit.day = summary(fitDay)$adj.r.squared,
    fit.day.sq = summary(fitDay.sq)$adj.r.squared
    )
  # filter(fit.day.sq > fit.day)

# Visualize
dat %>%
  # filter(between(chick, 34, 39) & day %in% c(0, 10, 20)) %>%
  mutate(chick = factor(chick, ordered=F)) %>%
  ggplot(aes(day, weight, )) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x))

# Analysis of variance
dat %$% lm(weight ~ day, data = .) %>% anova()
dat %$% aov(weight ~ day, data = .) %>% summary()
