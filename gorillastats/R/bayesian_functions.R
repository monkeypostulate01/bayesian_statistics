# ###############################################
# Title: Bayesian Statistics functions
# Author: Abel Camacho Guardian
# Date: 31/12/2019
#
# ##############################################
# Todo list:
# Check confidence interval
# A/B test for non-negative continous and discrete distribution
# A/B test for normal distribution


#' Calculate the parameters of the Beta distribution when the mean and variance are known
#' @param mu,var  mean and variance  of the Beta distribution.
#' @return Function returns the non-negative parameters of the Betadistribution
#' @examples
#' # The uniform distribution(0,1) with mean 1/2 and variance 1/12
#' # is a Beta distribution with parameters alpha=1 and beta=1
#' beta_moments_to_parameters(1 / 2, 1 / 12)
beta_moments_to_parameters <- function(mu, var) {
  if (mu <= 0 | mu >= 1) {
    stop("mu must be between 0 and 1")
  }

  if (var > min(1, (1 - mu) * mu)) {
    stop("variance is too high")
  }

  alpha <- ((1 - mu) / var - 1 / mu) * mu**2
  beta <- alpha * (1 / mu - 1)
  output <- c("alpha" = alpha, "beta" = beta)

  return(output)
}


#' Calculate the posterior parameters and confidence interval of the Beta distribution.
#' @param alpha,beta priori parameters of the Beta distribution.
#' @param x,n number of success and number of observations.
#' @param conf_interval Confidence interval
#' @return returns the priori and posterior parameters, confidence interval of the Beta distribution.
#' @examples
#' # Posterior distrion when the prior is a Beta distribution with parameters alpha=1, beta=1,  and 30 success and 100 observations
#' posterior_beta_dist(1, 1, 30, 100)
posterior_beta_dist <- function(alpha = 1, beta = 1, x, n, conf_interval = 0.95) {
  if (alpha <= 0 | beta <= 0) {
    stop("Beta parameters must non-negative")
  }

  if (x < 0 | n < x) {
    stop("successes must be between 0 and number of observations")
  }

  p_low <- (1 - conf_interval) / 2
  p_high <- conf_interval + p_low

  results <- list()

  results$prior_alpha <- alpha
  results$prior_beta <- beta

  results$prior_conf_interval <- c(
    qbeta(p_low, alpha, beta),
    qbeta(p_high, alpha, beta)
  )

  results$posterior_alpha <- x + alpha
  results$posterior_beta <- n - x + beta

  results$conf_interval <- c(
    qbeta(p_low, results$posterior_alph, results$posterior_beta),
    qbeta(p_high, results$posterior_alph, results$posterior_beta)
  )

  return(results)
}



#' Plot the evolution of the bayesian confidence interval
#' @param data_input a data frame containing the variables in the model.
#' @param alpha,beta priori parameters of the Beta distribution.
#' @param  col_groups a color vector used to color the groups.
#' @return Returns a ggplot showing the evolution of the confidence intervaldd
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data(data_coins)
#' #
#' data_coin %>%
#'   plot_bayesian_trend(col_groups = c("Control" = "blue", "Variant 1" = "red"))
#'
#' data(data_three_coins)
#' data_three_coins %>%
#'   plot_bayesian_trend(col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green"))
plot_bayesian_trend <- function(data_input,
                                alpha = 1,
                                beta = 1,
                                col_groups = c("blue", "red")) {
  if (dim(data_input)[2] < 4) {
    stop("Number of columns must be at least four")
  }

  names(data_input)[1:4] <- c("date", "x_temp", "n_temp", "group")


  all_groups <- unique(data_input$group)
  m <- length(all_groups)
  if (length(col_groups) != m) {
    colfunc <- colorRampPalette(col_groups)
    col_groups <- colfunc(m)
  }

  trends_plot <- list()
  for (i in 1:m) {
    trends_plot[[i]] <- data_input %>%
      filter(group == all_groups[i]) %>%
      mutate(x = cumsum(x_temp), n = cumsum(n_temp)) %>%
      select(date, x, n) %>%
      conf_interval_posterior(alpha = alpha, beta = beta)
    n <- dim(trends_plot[[i]])[1]

    x_values <- c(
      trends_plot[[i]][1, "date"],
      trends_plot[[i]][, "date"],
      trends_plot[[i]][n, "date"],
      trends_plot[[i]][n:1, "date"]
    )

    y_values <- c(
      trends_plot[[i]][1, "conf_interval1"],
      trends_plot[[i]][, "conf_interval2"],
      trends_plot[[i]][n, "conf_interval1"],
      trends_plot[[i]][n:1, "conf_interval1"]
    )

    data_temp <- data.frame("x" = x_values, "y" = y_values, "group" = all_groups[i])

    if (i == 1) {
      data_output <- data_temp
    } else {
      data_output <- rbind(data_output, data_temp)
    }
  }

  plot_output <- data_output %>%
    ggplot() +
    geom_polygon(aes(
      x = x,
      y = 100 * y,
      fill = group,
    ),
    alpha = 0.3,
    col = "black"
    ) + xlab("") + ylab("") +
    scale_fill_manual(values = col_groups, name = "") +
    theme_bottom()

  return(plot_output)
}


#' Not Finished
#' A/B Bayesian test of two proportions
#' @inheritParams plot_bayesian_trend
#' @examples
#' data(data_coin)
#'
#' data(data_coin)
#' data_coin %>%
#'   proportion_bayesian_test()
#'
#' data(data_three_coins)
#'
#' data_three_coins %>%
#'   proportion_bayesian_test(col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green"))
proportion_bayesian_test <- function(data_input,
                                     alpha = 1,
                                     beta = 1,
                                     n_simulations = 100,
                                     col_groups = c("blue", "red")) {
  data_output <- list()

  if (dim(data_input)[2] >= 4) {
    names(data_input)[2:4] <- c("x", "n", "group")
  } else {
    names(data_input)[1:3] <- c("x", "n", "group")
  }

  groups <- unique(data_input$group)

  m <- length(groups)

  if (length(col_groups) != m) {
    colfunc <- colorRampPalette(col_groups)
    col_groups <- colfunc(m)
  }

  data_summary <- list()
  beta_dist <- list()
  simulations <- matrix(0, nrow = n_simulations, ncol = m)

  for (i in 1:m) {
    data_summary[[i]] <- data_input %>%
      filter(group == groups[i]) %>%
      summarise(x = sum(x), n = sum(n)) %>%
      select(x, n)

    beta_dist[[i]] <- posterior_beta_dist(
      alpha = alpha,
      beta = beta,
      x = data_summary[[i]]$x,
      n = data_summary[[i]]$n
    )

    simulations[, i] <- rbeta(
      n = n_simulations,
      shape1 = beta_dist[[i]]$posterior_alpha,
      shape2 = beta_dist[[i]]$posterior_beta
    )
  }

  data_output$simulations <- simulations

  prob_winner <- max_probabilities(simulations)

  test2_data <- data.frame(
    "x" = 100 * prob_winner,
    "group" = groups
  )


  simulations <- data.frame(simulations)
  names(simulations) <- groups

  data_simulations <- simulations %>% pivot_longer(
    cols = groups,
    names_to = "groups",
    values_to = "simulations"
  )


  data_output$plot_winner <- test2_data %>%
    ggplot() +
    geom_bar(aes(x = group, y = x, fill = group), stat = "identity", alpha = 0.3) +
    geom_text(aes(x = group, y = x + 3, label = paste0(round(x), "%")), col = "gray40", size = 4) +
    theme_bottom() +
    theme(legend.position = "none") +
    coord_flip() +
    ylab("Probability of being a winner") +
    xlab("") +
    ylim(0, 103) +
    scale_fill_manual(values = col_groups, name = "")

  data_output$plot <- data_simulations %>%
    ggplot() +
    geom_density(aes(x = 100 * simulations, fill = groups), alpha = 0.3) +
    xlab("Posterior probability") +
    ylab("") +
    scale_fill_manual(values = col_groups, name = "") +
    theme(axis.text.y = element_text(size = 0)) +
    theme_bottom()

  return(data_output)
}


#' Not Finished 2
#' A/B Bayesian test of two proportions
#' @inheritParams proportion_bayesian_test
#' @examples
#' data(data_coin)
#' data_coin %>%
#'   bayesian_test_evaluation(
#'     alpha = 1, beta = 1,
#'     col_groups = c("Control" = "blue", "Variant 1" = "red")
#'   )
#'
#' data(data_three_coins)
#' data_three_coins %>%
#'   bayesian_test_evaluation(
#'     alpha = 1, bet = 1,
#'     col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green")
#'   )
#'
#' mu <- .5
#' var <- .05
#' beta_parameters <- beta_moments_to_parameters(mu, mu * (1 - mu) * 0.05)
#'
#' data_three_coins %>%
#'   bayesian_test_evaluation(
#'     alpha = beta_parameters[1],
#'     bet = beta_parameters[2],
#'     col_groups = c("Control" = "blue", "Variant 1" = "red", "Variant 2" = "green")
#'   )
bayesian_test_evaluation <- function(data_input,
                                     alpha = 1,
                                     beta = 1,
                                     title_label = NULL,
                                     file_name = NULL,
                                     col_groups = c("blue", "red")) {
  names(data_input)[1:4] <- c("date", "x", "n", "group")

  plot_p1_temp <- data_input %>%
    group_by(group) %>%
    summarise(
      x = sum(x),
      n = sum(n)
    ) %>%
    select(x, n, group) %>%
    proportion_bayesian_test(
      n_simulations = 500,
      alpha = alpha,
      beta = beta,
      col_groups = col_groups
    )

  plot_p1 <- plot_p1_temp$plot +
    ggtitle(title_label) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 0, color = "white")
    )

  plot_winner <- plot_p1_temp$plot_winner

  plot_p2 <- data_input %>%
    plot_bayesian_trend(
      alpha = alpha,
      beta = beta,
      col_groups = col_groups
    ) + xlab("Time") +
    theme_bottom()

  final_temp <- plot_grid(plot_p1, plot_winner,
    ncol = 2
  )

  final_temp <- plot_grid(final_temp, plot_p2,
    ncol = 1
  )

  return(final_temp)
}



################################
# Functions must be hidden
conf_interval_posterior <- function(data_input, alpha, beta, conf_interval = 0.95) {
  n <- dim(data_input)[1]
  plot_data <- data.frame("date" = 0:n)
  plot_data$conf_interval1 <- rep(0, 1 + n)
  plot_data$conf_interval2 <- rep(0, 1 + n)

  posterior_beta <- posterior_beta_dist(alpha, beta,
    x = data_input$x[1],
    n = data_input$n[1], conf_interval = 0.95
  )

  plot_data[1, "conf_interval1"] <- posterior_beta$prior_conf_interval[1]
  plot_data[1, "conf_interval2"] <- posterior_beta$prior_conf_interval[2]

  plot_data[2, "conf_interval1"] <- posterior_beta$conf_interval[1]
  plot_data[2, "conf_interval2"] <- posterior_beta$conf_interval[2]

  for (i in 2:n) {
    posterior_beta <- posterior_beta_dist(alpha, beta,
      x = data_input$x[i],
      n = data_input$n[i], conf_interval = 0.95
    )

    plot_data[i + 1, "conf_interval1"] <- posterior_beta$conf_interval[1]
    plot_data[i + 1, "conf_interval2"] <- posterior_beta$conf_interval[2]
  }

  return(plot_data)
}
