





max_probabilities <- function(data_input) {
  n <- dim(data_input)[1]
  m <- dim(data_input)[2]

  probabilities <- rep(0, m)

  if (m > 2) {
    for (k in m:2) {
      data_temp <- apply(data_input[, -k], 1, max) <= data_input[, k]
      probabilities[k] <- sum(data_temp) / n
    }
  } else {
    data_temp <- data_input[, 1] <= data_input[, 2]
    probabilities[2] <- sum(data_temp) / n
  }

  probabilities[1] <- 1 - sum(probabilities)
  return(probabilities)
}
