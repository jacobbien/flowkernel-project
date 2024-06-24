# Generated from _main.Rmd: do not edit by hand

#' Initialize the Kernel EM-algorithm using constant parameters
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param K number of components
#' @param times_to_sample number of time points to sample
#' @param points_to_sample number of bins to sample from each sampled time point
#' @export
init_const <- function (y, K, times_to_sample = 50, points_to_sample = 50){
  num_times <- length(y)
  d <- ncol(y[[1]])
  mu <- array(NA, c(num_times, K, d))
  Sigma <- array(NA, c(num_times, K, d, d))
  pi <- matrix(NA, num_times, K)
  sample_data <- matrix(nrow = times_to_sample * points_to_sample, ncol = d)
  init_fit = NULL
  # Sample 50 different time points
  while (is.null(init_fit) == TRUE){
    time_points <- sample(1:num_times, times_to_sample, replace = TRUE)
    row_counter = 1
    # Loop over each sampled time point
    for (tt in time_points) {
      # Sample 50 rows from the matrix at the current time point
      rows_to_sample <- sample(1:nrow(y[[tt]]), points_to_sample, replace = TRUE)
      sampled_rows <- y[[tt]][rows_to_sample, ]
      # Append the sampled rows to the sample_init matrix
      sample_data[row_counter:(row_counter + points_to_sample - 1), ] <- sampled_rows
      row_counter <- row_counter + points_to_sample
    }
    if (d == 1){
      init_fit <- mclust::Mclust(sample_data, G = K, modelNames = "V")
      for (tt in seq(num_times)){
        mu[tt, , 1] <- init_fit$parameters$mean
        Sigma[tt, , 1, 1] <- init_fit$parameters$variance$sigmasq
        pi [tt, ] <- init_fit$parameters$pro
      }
    }else if (d > 1){
      init_fit <- mclust::Mclust(sample_data, G = K, modelNames = "VVV")
      for (tt in seq(num_times)){
      mu[tt, ,] <- t(init_fit$parameters$mean)
      pi[tt, ] <- init_fit$parameters$pro
      Sigma[tt, , , ] <- aperm(init_fit$parameters$variance$sigma, c(3,1,2))
      }
    }
  }
  
  #calculate responsibilities (can change this to log densities - numerical stability)
  resp <- list() # responsibilities gamma[[t]][i, k]
  if (d == 1) {
    for (tt in seq(num_times)) {
      phi <- matrix(NA, nrow(y[[tt]]), K)
      for (k in seq(K)) {
        phi[, k] <- stats::dnorm(y[[tt]],
                                 mean = mu[tt, k, 1],
                                 sd = sqrt(Sigma[tt, k, 1, 1]))
      }
      temp <- t(t(phi) * pi[tt, ])
      resp[[tt]] <- temp / rowSums(temp)
    }
  }else if (d > 1){
    for (tt in seq(num_times)) {
      phi <- matrix(NA, nrow(y[[tt]]), K)
      for (k in seq(K)) {
        phi[, k] <- mvtnorm::dmvnorm(y[[tt]],
                                     mean = mu[tt, k, ],
                                     sigma = Sigma[tt, k, , ])
      }
      temp <- t(t(phi) * pi[tt, ])
      resp[[tt]] <- temp / rowSums(temp)
    }
  }
  zest <- resp %>% purrr::map(~ max.col(.x))
  fit_init = list(mu = mu, Sigma = Sigma, pi = pi, resp = resp, zest = zest)
  return(fit_init)
}
