install.packages("gorillastats")

library(dplyr)
library(ggplot2)
library(cowplot)
library(gorillastats)

#########################
posterior_beta_dist(alpha = 3, beta = 3, x = 50, n = 100)

data_coin %>%
  filter(group == "Control") %>%
  select(date, x, n) %>%
  conf_interval_posterior(alpha = 1, beta = 1)


data_coin %>%
  plot_bayesian_trend(col_groups = c("Control" = "blue", "Variant 1" = "red"))

a <- data_coin %>%
  proportion_bayesian_test(col_groups = c("Control" = "blue", "Variant 1" = "red"))

names(a)


data_three_coins %>%
  plot_bayesian_trend(col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green"))


data_three_coins %>%
  proportion_bayesian_test(col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green"))



data_coin %>%
  bayesian_test_evaluation(
    alpha = 1, beta = 1,
    col_groups = c("Control" = "blue", "Variant 1" = "red"))


data_three_coins %>%
  bayesian_test_evaluation(
    alpha = 1, bet = 1,
    col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green"))


mu <- .5
var <- .05
beta_parameters <- beta_moments_to_parameters(mu, mu * (1 - mu) * 0.05)

data_three_coins %>%
  bayesian_test_evaluation(
    alpha = beta_parameters[1],
    bet = beta_parameters[2],
    col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green")
  )


