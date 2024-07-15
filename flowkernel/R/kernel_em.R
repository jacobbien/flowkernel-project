# Generated from _main.Rmd: do not edit by hand

#' A Kernel-smoothed EM algorithm for a mixture of Gaussians that changes over time. The larger any of the three bandwidths are, the smoother the corresponding parameter will vary. 
#' 
#' Element t in the list `y`, `y[[t]]`, is an n_t-by-d matrix with the coordinates of every point (or bin) at time t given in each row. `biomass[[t]]` is a numeric vector of length n_t, where the ith entry of the vector is the biomass for the bin whose coordinates are given in the ith row of `y[[t]]`. 
#' 
#' The initial_fit parameter is a list of initial parameter values that is generated from the initialization functions in the package. It contains initial values at all times for mu, Sigma, and pi, as well as estimates for the responsibilities and cluster membership. Currently, for our cluster membership estimates (z estimate - zest), each bin is assigned to the cluster that is most responsible for it.
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param K number of components
#' @param hmu bandwidth for mu parameter
#' @param hSigma bandwidth for Sigma parameter
#' @param hpi bandwidth for pi parameter
#' @param num_iter number of iterations of EM to perform
#' @param biomass list of length T, where each element `biomass[[t]]` is a 
#' numeric vector of length n_t containing the biomass (or count) of particles 
#' in each bin
#' @param initial_fit a list of starting values for the parameters, responsibilities, and estimated cluster assignments
#' @export
kernel_em <- function (y, K, hmu, hSigma, hpi, num_iter = 10, 
                       biomass = default_biomass(y),
                       initial_fit = init_const(y, K, 50, 50)) {
  num_times <- length(y)
  d <- ncol(y[[1]])
  mu <- initial_fit$mu
  Sigma <- initial_fit$Sigma
  pi <- initial_fit$pi
  for (l in seq(num_iter)) {
        # E-step: update responsibilities
        resp <- calculate_responsibilities(y, mu, Sigma, pi)
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
  list(mu = mu, Sigma = Sigma, pi = pi, resp = resp, zest = zest)
}
