# Generated from _main.Rmd: do not edit by hand

#' A Kernel-smoothed EM algorithm for a mixture of Gaussians that change over time
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param K number of components
#' @param hmu bandwidth for mu parameter
#' @param hSigma bandwidth for Sigma parameter
#' @param hpi bandwidth for pi parameter
#' @param num_iter number of iterations of EM to perform
#' @param biomass A list of length T, where each element `biomass[[t]]` is a numeric vector of length n_t containing the biomass (or count) of particles in each bin
#' @param initial_fit initial fit from either of the initialization functions (defaults to constant initialization)
#' @param times_to_sample number of time points to sample for constant initialization. Used only if initial_fit not given already.
#' @param points_to_sample number of bins to sample from each sampled time point for constant initialization. Used only if initial_fit not given already.
#' @export
kernel_em <- function (y, K, hmu, hSigma, hpi, num_iter = 10, biomass = default_biomass(y), initial_fit = NULL, times_to_sample = 50, points_to_sample = 50){
  num_times <- length(y)
  d <- ncol(y[[1]])
  mu <- array(NA, c(num_times, K, d))
  Sigma <- array(NA, c(num_times, K, d, d))
  pi <- matrix(NA, num_times, K)
  resp <- list() # responsibilities gamma[[t]][i, k]
if (is.null(initial_fit)) {
    initial_fit = init_const(y, K, times_to_sample, points_to_sample)
  }
mu = initial_fit$mu
Sigma = initial_fit$Sigma
pi = initial_fit$pi
  for (l in seq(num_iter)) {
      # E-step: update responsibilities
        resp <- list() # responsibilities gamma[[t]][i, k]
        # E-step: update responsibilities
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
                                           mean = mu[tt,k,],
                                           sigma = Sigma[tt,k,,])
            }
            temp <- t(t(phi) * pi[tt, ])
            resp[[tt]] <- temp / rowSums(temp)
          }
        }
        resp_weighted <- purrr::map2(biomass, resp, ~ .y * .x)
      # M-step: update estimates of (mu, Sigma, pi)
      # do summations over i:
      #form T-by-K matrix summing resp_itk over i
    resp_sum <- purrr::map(resp_weighted, ~ colSums(.x)) %>%
          unlist() %>%
          matrix(ncol = K, byrow = TRUE)
    
    resp_sum_smooth <- apply(
      resp_sum, 2, function(x) 
        stats::ksmooth(1:length(x), x, bandwidth = hpi, x.points = 1:length(x))$y
    )
    pi <- resp_sum_smooth / rowSums(resp_sum_smooth)
      # form T-by-K-by-d array summing resp_itk * Y_ij over i
    y_sum <- purrr::map2(resp_weighted, y, ~ crossprod(.x, .y)) %>% 
          unlist() %>% 
          array(c(K, d, num_times)) %>% 
          aperm(c(3,1,2))
    
    y_sum_smoothed <- apply(
          y_sum, 2:3, function(x) 
          stats::ksmooth(1:length(x), x, bandwidth = hmu, x.points = 1:length(x))$y
    )
    resp_sum_smooth_mu <- apply(
          resp_sum, 2, function(x) 
          stats::ksmooth(1:length(x), x, bandwidth = hmu, x.points = 1:length(x))$y
    )
    
    for (j in seq(d)) {
      mu[, , j] <- y_sum_smoothed[, , j] / resp_sum_smooth_mu
    }
      # form a T-by-K-by-d-by-d array
      # summing (Y_it - mu_t)*diag(resp_itk)*(Y_it - mu_t)^T over i
    mat_sum <- array(NA, c(num_times, K, d, d))
        for (tt in seq(num_times)) {
          yy <- matrix(NA, dim(y[[tt]])[1], d)
          for (k in seq(K)) {
            for(dd in seq(d)) {
              #yy [,dd] <- (y[[tt]][, dd]- mu_sig[tt, k, dd])
              yy [,dd] <- (y[[tt]][, dd]- mu[tt, k, dd])
            }
              mat_sum[tt, k, , ] <- crossprod(yy, yy * resp_weighted[[tt]][, k]) # YY^T * D * YY
            }
          }
        mat_sum_smoothed <- apply(
          mat_sum, 2:4, function(x)
            stats::ksmooth(1:length(x), x, bandwidth = hSigma, x.points = 1:length(x))$y
        )
        resp_sum_smooth_Sigma <- apply(
          resp_sum, 2, function(x) 
            stats::ksmooth(1:length(x), x, bandwidth = hSigma, x.points = 1:length(x))$y
        )
        for (j in seq(d))
          for (l in seq(d))
            Sigma[, , j, l] <- mat_sum_smoothed[, , j, l] / resp_sum_smooth_Sigma
  }
  zest <- resp %>% purrr::map(~ max.col(.x))
  dimnames(mu) <- list(NULL, paste0("cluster", 1:K), NULL)
  dimnames(Sigma) <- list(NULL, paste0("cluster", 1:K), NULL, NULL)
  dimnames(pi) <- list(NULL, paste0("cluster", 1:K))
  fit <- list(mu = mu, Sigma = Sigma, pi = pi, resp = resp, zest = zest)
  return(fit)
}
