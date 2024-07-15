# Generated from _main.Rmd: do not edit by hand

#' Calculates responsibilities for each point (or bin) at each time point, given parameter estimates for all clusters at all times 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param mu a T-by-K-by-d array of means
#' @param Sigma a T-K-by-d-by-d array of covariance matrices
#' @param pi a T-by-K vector of probabilities
calculate_responsibilities <- function(y, mu, Sigma, pi){
  resp <- list() # responsibilities gamma[[t]][i, k]
  log_resp <- list() # log of responsibilities
  d <- ncol(y[[1]])
  K <- ncol(mu)
  num_times <- length(y)
  if (d == 1) {
    for (tt in seq(num_times)) {
      log_phi <- matrix(NA, nrow(y[[tt]]), K)
      for (k in seq(K)) {
        log_phi[, k] <- stats::dnorm(y[[tt]],
                                     mean = mu[tt, k, 1],
                                     sd = sqrt(Sigma[tt, k, 1, 1]), log = TRUE)
      }
      log_temp = t(t(log_phi) + log(pi[tt, ]))
      log_resp[[tt]] = log_temp - matrixStats::rowLogSumExps(log_temp)
      resp[[tt]] = exp(log_resp[[tt]])
    }
  } else if (d > 1) {
    for (tt in seq(num_times)) {
      log_phi <- matrix(NA, nrow(y[[tt]]), K)
      for (k in seq(K)) {
        log_phi[, k] <- mvtnorm::dmvnorm(y[[tt]],
                                         mean = mu[tt, k, ],
                                         sigma = Sigma[tt, k, , ], log = TRUE)
      }
      log_temp = t(t(log_phi) + log(pi[tt, ]))
      log_resp[[tt]] = log_temp - matrixStats::rowLogSumExps(log_temp)
      resp[[tt]] = exp(log_resp[[tt]])
    }
  }
  return(resp) 
}
