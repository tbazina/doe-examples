######## Simple quaratic dataset model ##################
## Libraries
# Visualization
library(ggplot2)
# DoE
library(DoE.wrapper)
# Distribution check
library(fitdistrplus)
library(mixtools)
# Cleaning data
library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(broom)
# Use dplyr select
select <- dplyr::select

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
mu_sigma <- dat %>% group_by(day) %>% summarise(
  mu = mean(weight),
  sigma = sd(weight)
)

## Histogram
dat <- dat %>% group_by(day) %>% mutate(mu = mean(weight), sd = sd(weight)) %>%
  ungroup() 
  
hist_plot <- function(data) {
  data <- data %>% mutate(day = factor(day)) %>% rename(Dan = day)
  plt <- ggplot(aes(x=weight, fill = Dan), data = data) +
  facet_grid(. ~ Dan) +
  geom_histogram(aes(y = ..density.., fill = day),
                 binwidth = 19,
                 color="black", fill="dodgerblue3",
                 ) +
  geom_vline(aes(xintercept=mu), linetype="dashed", size=1) +
  geom_density(alpha = 0.7, outline.type = 'full') +
  scale_y_continuous(
    name = 'Gustoća razdiobe',
    # breaks = seq(0, 400, 50),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Masa [g]",
    # breaks = seq(0, 20, 10)
    ) +
  scale_fill_manual(
    values = c("yellow", "darksalmon")
  ) +
  ggtitle("Gustoća razdiobe po danima") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
  return(plt)
}

hist_plot(dat %>% filter(day != 0))
ggsave(filename = 'slike/density_distribution_days.png',
       width = 16, height = 11, units = 'cm', dpi = 320, pointsize = 10)

# Descriptive parameters of distribution
png('slike/distribution_descriptive.png',
    width = 14, height = 11, units = 'cm', res = 320, pointsize = 10)
descdist(data = dat$weight)
dev.off()

# Normalize data for beta distribution fit
dat <- dat %>% mutate(
  weight.norm = (weight - min(weight)) / (max(weight) - min(weight))
  )

# Fit distribution parameters
fit.beta <- fitdist(
  data = dat$weight.norm, distr = 'beta', method = 'qme', probs = c(1/3, 2/3)
  )
fit.beta
png('slike/distribution_hist_qq_cfd_pp.png',
    width = 16, height = 13, units = 'cm', res = 320, pointsize = 10)
plot(fit.beta, breaks = 8)
dev.off()

fit.multimodal <- normalmixEM(
  dat$weight, lambda = 0.1, mu = mu_sigma$mu, sigma = mu_sigma$sigma)
plot(fit.multimodal, density = T)

plotdist(dat$weight, breaks = 5, histo = T, demp = T)
  
## Q-Q plot
dat %>%
  ggplot(aes(sample=weight)) +
  stat_qq(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 2, stroke = 0.3,
    distribution = function(p) qbeta(p, shape1 = 0.6492259, shape2 = 1.2747630)) +
  stat_qq_line(distribution = function(p) qbeta(p, shape1 = 0.6492259, shape2 = 1.2747630)) + theme(
    axis.line = element_line(size=0.5, colour = "black")
  )

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


## Box-plot
dat %>% mutate(Dan = ordered(day)) %>%
  ggplot(aes(x = Dan, y = weight, fill=Dan)) +
  geom_boxplot(width = 0.3, alpha = 0.7) + 
  geom_dotplot(
    colour = "black", stroke = 2,
    binaxis='y', stackdir='center', dotsize=1, binwidth = 6) +
  scale_y_continuous(
    name = "Masa [g]",
    breaks = seq(0, 400, 50),
    limits=c(0, NA)
    ) +
  scale_x_discrete(name = "Dan vaganja [ / ]") +
  ggtitle("Box-plot") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    # axis.title = element_text(face="bold"),
    # axis.text.x = element_text(colour="black", size = 11),
    # axis.text.y = element_text(colour="black", size = 9),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/box-plot.png',
       width = 12, height = 10, units = 'cm', dpi = 320)
  

# Summary statistics
dat %>% group_by(day) %>% summarise(
  min = min(weight),
  "1st Qu." = quantile(weight, probs = 1/4),
  median = median(weight),
  mean = mean(weight),
  "3st Qu." = quantile(weight, probs = 3/4),
  max = max(weight)
) %>% View()

summary(dat %>% filter(day == 10))


# Analysis of variance and regression
dat <- dat %>% mutate(day.sq = day^2)
## Linear
dat %$% aov(weight ~ day, data = .) %>% summary()
fit.lin <- dat %$% lm(weight ~ day, data = .)
summary(fit.lin)
## Quadratic
dat %$% aov(weight ~ day.sq + day, data = .) %>% summary()
fit.sq <- dat %$% lm(weight ~ day.sq, data = .)
summary(fit.sq)

################################################################################
# Visulize fit
## Linear fit
dat %>%
  mutate(chick = factor(chick, ordered=F)) %>%
  ggplot(aes(x = day, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x)) +
  scale_y_continuous(
    name = 'Masa [g]',
    # breaks = seq(0, 400, 50),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    # breaks = seq(0, 20, 10)
    ) +
  ggtitle("Linearna regresija") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/linearna_regresija_plot.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

# Quadratic fit
dat %>%
  mutate(chick = factor(chick, ordered=F)) %>%
  ggplot(aes(x = day, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x^2)) +
  scale_y_continuous(
    name = 'Masa [g]',
    # breaks = seq(0, 400, 50),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    # breaks = seq(0, 20, 10)
    ) +
  ggtitle("Polinomna regresija 2. stupnja") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/kvadratna_regresija_plot.png',
       width = 8, height = 8, units = 'cm', dpi = 320)


################################################################################
# Model verification
dat <- dat %>%
  mutate(weight.lin = predict.lm(fit.lin),
         weight.sq = predict.lm(fit.sq),
         resid.lin = resid(fit.lin),
         resid.sq = resid(fit.sq)
         )
  
## Response vs predictions vizualization
dat %>%
  pivot_longer(cols = c(weight.lin, weight.sq), names_to = "weight.pred",
               values_to = "pred.val") %>%
  ggplot(aes(x = weight, y = pred.val, fill = weight.pred)) +
  facet_grid(. ~ weight.pred,
             labeller = labeller(
               .cols = c(weight.lin = "Linearna regresija",
                         weight.sq = "Polinomna regresija 2. stupnja"))
               ) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 1) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = 'black',
    linetype = '4343',
    size = 0.7) +
  scale_y_continuous(
    name = 'Predviđena masa [g]',
    breaks = seq(0, 300, 50),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Izmjerena masa [g]",
    breaks = seq(0, 300, 50)
    ) +
  ggtitle("Predviđanja - odziv") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/response_predictions.png',
       width = 16, height = 8, units = 'cm', dpi = 320)

## Residuals vs response vizualization
dat %>%
  pivot_longer(cols = c(resid.lin, resid.sq), names_to = "resid.pred",
               values_to = "resid.val") %>%
  ggplot(aes(x = weight, y = resid.val, fill = resid.pred)) +
  facet_grid(. ~ resid.pred,
             labeller = labeller(
               .cols = c(resid.lin = "Linearna regresija",
                         resid.sq = "Polinomna regresija 2. stupnja"))
               ) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 1) +
  geom_hline(
    yintercept = 0,
    color = 'black',
    linetype = '5353',
    size = 0.8) +
  scale_y_continuous(
    name = 'Rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Izmjerena masa [g]",
    breaks = seq(0, 300, 50)
    ) +
  ggtitle("Rezidualna odstupanja - odziv") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/residuals_response.png',
       width = 16, height = 8, units = 'cm', dpi = 320)

## Residuals vs factors vizualization
dat %>%
  pivot_longer(cols = c(resid.lin, resid.sq), names_to = "resid.pred",
               values_to = "resid.val") %>%
  ggplot(aes(x = day, y = resid.val, fill = resid.pred)) +
  facet_grid(. ~ resid.pred,
             labeller = labeller(
               .cols = c(resid.lin = "Linearna regresija",
                         resid.sq = "Polinomna regresija 2. stupnja"))
               ) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 1) +
  geom_hline(
    yintercept = 0,
    color = 'black',
    linetype = '5353',
    size = 0.8) +
  scale_y_continuous(
    name = 'Rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Dan vaganja [ / ]",
    breaks = seq(0, 20, 5)
    ) +
  ggtitle("Rezidualna odstupanja - nezavisna varijabla") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/residuals_factors.png',
       width = 16, height = 8, units = 'cm', dpi = 320)

## Residual lag plot - linear model
dat <- dat %>%
  mutate(resid.lin.lag = c(tail(resid.lin, -1), 0),
         resid.sq.lag = c(tail(resid.sq, -1), 0),
         )
dat %>% slice(-n()) %>%
  ggplot(aes(x = resid.lin, y = resid.lin.lag,
             )) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 1) +
  geom_hline(
    yintercept = 0,
    color = 'black',
    linetype = '5353',
    size = 0.8) +
  scale_y_continuous(
    name = 'i-ta rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
  ) +
  scale_x_continuous(
    name = '(i-1) rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
    ) +
  ggtitle("Linearni model") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/residuals_lin_lag.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

## Residual lag plot - quadratic model
dat %>% slice(-n()) %>%
  ggplot(aes(x = resid.sq, y = resid.sq.lag,
             )) +
  geom_point(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 1.5, stroke = 1) +
  geom_hline(
    yintercept = 0,
    color = 'black',
    linetype = '5353',
    size = 0.8) +
  scale_y_continuous(
    name = 'i-ta rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
  ) +
  scale_x_continuous(
    name = '(i-1) rezidualna odstupanja [g]',
    breaks = seq(-100, 100, 25),
    ) +
  ggtitle("Polinomni model") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/residuals_sq_lag.png',
       width = 8, height = 8, units = 'cm', dpi = 320)

## Residual histogram
dat %>% 
  pivot_longer(cols = c(resid.lin, resid.sq), names_to = "resid.pred",
               values_to = "resid.val") %>%
  group_by(resid.pred) %>% mutate(resid.mean = mean(resid.val)) %>%
  ungroup() %>%
  ggplot(aes(x=resid.val, fill = resid.pred)) +
    facet_grid(. ~ resid.pred,
               labeller = labeller(
                 .cols = c(resid.lin = "Linearna regresija",
                           resid.sq = "Polinomna regresija 2. stupnja"))
                 ) +
    geom_histogram(aes(y = ..density.., fill = resid.val),
                   binwidth = 15,
                   color="black", fill="dodgerblue3",
                   ) +
    geom_vline(aes(xintercept=resid.mean), linetype="dashed", size=1) +
    geom_density(alpha = 0.7, outline.type = 'full') +
    scale_y_continuous(
      name = 'Gustoća razdiobe',
      # breaks = seq(0, 400, 50),
      limits = c(0, NA)
    ) +
    scale_x_continuous(
      name = "Rezidualna odstupanja [g]",
      # breaks = seq(0, 20, 10)
      ) +
    scale_fill_manual(
      values = c("yellow", "darksalmon"),
      labels = c("Linearna", "Polinomna"),
      name = "Procjena gustoće\nrazdiobe"
    ) +
    ggtitle("Gustoća razdiobe rezidualnih odstupanja") +
    theme(
      axis.line = element_line(size=0.5, colour = "black"),
      plot.title = element_text(hjust = 0.5)
    )
ggsave(filename = 'slike/residuals_density.png',
       width = 16, height = 9, units = 'cm', dpi = 320)

## Q-Q residual plot
dat %>%
  pivot_longer(cols = c(resid.lin, resid.sq), names_to = "resid.pred",
               values_to = "resid.val") %>%
  ggplot(aes(sample=resid.val, fill = resid.pred)) +
    facet_grid(. ~ resid.pred,
               labeller = labeller(
                 .cols = c(resid.lin = "Linearna regresija",
                           resid.sq = "Polinomna regresija 2. stupnja"))
                 ) +
  stat_qq(
    shape = 21, colour = "black", fill = "dodgerblue4", size = 2, stroke = 0.3,
    ) +
  stat_qq_line() +
  scale_y_continuous(
    name = 'Empirijski kvantili',
    breaks = seq(-60, 60, 20),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Teorijski kvantili",
    # breaks = seq(0, 20, 10)
    ) +
  ggtitle("Q-Q dijagram rezidualnih odstupanja") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'slike/residuals_Q-Q.png',
       width = 16, height = 8, units = 'cm', dpi = 320)

################################################################################
# Predicting
dat.predict <- tibble(
  day = 0:20,
  day.sq = day^2
)
dat.predict <- dat.predict %>% mutate(
  weight.lin = predict(fit.lin, .),
  weight.sq = predict(fit.sq, .)
  )
dat.predict %>%
  write.table(file = 'CRD_Chick_18_3_6_predictions.csv', sep = ';', dec = ',')
