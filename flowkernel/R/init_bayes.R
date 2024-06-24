# Generated from _main.Rmd: do not edit by hand

#' Initialize the Kernel EM-algorithm using Bayesian methods
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param K number of components
#' @param biomass A list of length T, where each element `biomass[[t]]` is a numeric vector of length n_t containing the biomass (or count) of particles in each bin
#' @param num_iter number of iterations of EM to perform #need to think more about iterations for Bayes init
#' @param lap_smooth_const laplace smoothing constant #More explanation needed
#' @export
init_bayes <- function (y, K,  biomass = default_biomass(y), num_iter = 1, lap_smooth_const = 0){
  #browser()
  num_times <- length(y)
  d <- ncol(y[[1]])
  resp <- list() # responsibilities gamma[[t]][i, k]
  log_resp <- list()
  resp_weighted <- list() 
  resp_sum <- list() 
  resp_sum_pi <- list()
  y_sum <- list()
  mat_sum <- list()
  yy <- list()
  mu <- array(NA, c(num_times, K, d))
  Sigma <- array(NA, c(num_times, K, d, d))
  pi <- matrix(NA, num_times, K)
  
  if (d == 1){
    init_fit <- mclust::Mclust(y[[1]], G = K, modelNames = "V")
    ii = 1
    while (is.null(init_fit) == TRUE){
      init_fit <- mclust::Mclust(y[[1 + ii]], G = K, modelNames = "V")
      ii = ii + 1
    }
    mu[1, , 1] <- init_fit$parameters$mean
    Sigma[1, , 1, 1] <- init_fit$parameters$variance$sigmasq
    pi [1, ] <- init_fit$parameters$pro
  } else if (d > 1){
    init_fit <- mclust::Mclust(y[[1]], G = K, modelNames = "VVV")
    ii = 1
    while (is.null(init_fit) == TRUE){
      init_fit <- mclust::Mclust(y[[1 + ii]], G = K, modelNames = "V")
      ii = ii + 1
    }
    mu[1, ,] <- t(init_fit$parameters$mean)
    pi[1, ] <- init_fit$parameters$pro
    Sigma[1, , , ] <- aperm(init_fit$parameters$variance$sigma, c(3,1,2))
  }
  phi <- matrix(NA, nrow(y[[1]]), K)
  log_phi <- matrix(NA, nrow(y[[1]]), K)
  if (d == 1) {
    for (k in seq(K)) {
      log_phi[, k] <- stats::dnorm(y[[1]],
                                   mean = mu[1, k, 1],
                                   sd = sqrt(Sigma[1, k, 1, 1]), log = TRUE)
    }
  } else if (d > 1) {
    for (k in seq(K)) {
      log_phi[, k] <- mvtnorm::dmvnorm(y[[1]],
                                       mean = mu[1, k, ],
                                       sigma = Sigma[1, k, , ], log = TRUE)
    }
  }
  log_temp = t(t(log_phi) + log(pi[1, ]))
  log_resp = log_temp - matrixStats::rowLogSumExps(log_temp)
  resp[[1]] = exp(log_resp)
  resp_weighted[[1]] = diag(biomass[[1]]) %*% resp[[1]]
  resp_sum[[1]] <- colSums(resp_weighted[[1]]) %>%
    unlist() %>%
    matrix(ncol = K, byrow = TRUE)
  pi_prev <- pi
  mu_prev <- mu
  Sigma_prev <- Sigma
  for (tt in 2:num_times){
    for (l in seq(num_iter)) {
      # E-step
      phi <- matrix(NA, nrow(y[[tt]]), K)
      if (l == 1){
        if (d == 1) {
          for (k in seq(K)) {
            phi[, k] <- stats::dnorm(y[[tt]],
                                     mean = mu_prev[tt - 1, k, 1],
                                     sd = sqrt(Sigma_prev[tt - 1, k, 1, 1]))
          }
        } else if (d > 1) {
          for (k in seq(K)) {
            phi[, k] <- mvtnorm::dmvnorm(y[[tt]],
                                         mean = mu_prev[tt - 1, k, ],
                                         sigma = Sigma_prev[tt - 1, k, , ])
          }
        }
        temp <- t(t(phi) * pi_prev[tt - 1, ])
      }else if (l > 1) {
        if (d == 1) {
          for (k in seq(K)) {
            phi[, k] <- stats::dnorm(y[[tt]],
                                     mean = mu_prev[tt, k, 1],
                                     sd = sqrt(Sigma_prev[tt, k, 1, 1]))
          }
        } else if (d > 1) {
          for (k in seq(K)) {
            phi[, k] <- mvtnorm::dmvnorm(y[[tt]],
                                         mean = mu_prev[tt, k, ],
                                         sigma = Sigma_prev[tt, k, , ])
          }
        }
        temp <- t(t(phi) * pi_prev[tt, ])
      }
      temp_smooth = temp + lap_smooth_const * apply(temp, 1, min)
      
      
      resp[[tt]] <- temp_smooth / rowSums(temp_smooth)
      resp_weighted[[tt]] = diag(biomass[[tt]]) %*% resp[[tt]]
      zest <- resp %>% purrr::map(~ max.col(.x))
      #M-step
      #M-step pi
      resp_sum[[tt]] <- colSums(resp_weighted[[tt]]) %>%
        unlist() %>%
        matrix(ncol = K, byrow = TRUE)
      for (k in seq(K)){
        pi[tt, k] <- (resp_sum [[tt - 1]][, k] + resp_sum [[tt]][, k]) / (rowSums(resp_sum[[tt - 1]]) + rowSums(resp_sum[[tt]]))

      }
      #M-step mu
      y_sum[[tt]] <- crossprod(resp_weighted[[tt]], y[[tt]]) %>%
        unlist() %>% 
        array(c(K, d))
      for (k in seq(K)){
          mu[tt, k, ] <- ((resp_sum[[tt - 1]][, k] * mu[tt - 1, k , ]) +  y_sum[[tt]][k, ]) / (resp_sum[[tt - 1]] [, k] + resp_sum[[tt]] [, k])

      }

      
      #M-step Sigma
      mat_sum[[tt]] <- array(NA, c(K, d, d))
      yy[[tt]] <- matrix(NA, dim(y[[tt]])[1], d)
      for (k in seq(K)) {
        for(dd in seq(d)) {
          yy [[tt]][, dd] <- (y[[tt]][, dd]- mu[tt, k, dd])
        }
        mat_sum[[tt]][k, , ] <- crossprod(yy[[tt]], yy[[tt]] * resp_weighted[[tt]][, k]) # YY^T * D * YY
      }
      for (k in seq(K)){
        Sigma[tt, k, , ] <- ((resp_sum[[tt - 1]][, k] * Sigma[tt - 1, k, , ]) + mat_sum[[tt]][k, , ]) / (resp_sum[[tt - 1]] [, k] + resp_sum[[tt]] [, k])
      }
      pi_prev <- pi
      mu_prev <- mu
      Sigma_prev <- Sigma 
    }

  }
  dimnames(mu) <- list(NULL, paste0("cluster", 1:K), NULL)
  dimnames(Sigma) <- list(NULL, paste0("cluster", 1:K), NULL, NULL)
  dimnames(pi) <- list(NULL, paste0("cluster", 1:K))
  zest <- resp %>% purrr::map(~ max.col(.x))
  fit_init = list(mu = mu, Sigma = Sigma, pi = pi, resp = resp, zest = zest)
  return(fit_init)
}
