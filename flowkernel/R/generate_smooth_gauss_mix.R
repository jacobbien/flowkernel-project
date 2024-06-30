# Generated from _main.Rmd: do not edit by hand

#' Generate data from smoothly-varying mixture of Gaussians model
#' 
#' The smoothly-varying mixture of Gaussians model is defined as follows:
#' 
#' At time t there are n_t points generated as follows:
#' 
#' Y_{it}|\{Z_{it}=k\} ~ N_d(mu_{kt},Sigma_{kt})
#' where
#' P(Z_{it}=k)=pi_{kt}
#' and the parameters (mu_{kt},Sigma_{kt}, pi_{kt}) are all slowly varying in time.
#' 
#' This function generates Y and Z.
#' 
#' @param mu_function a function that maps a vector of times to a T-by-K-by-d
#' array of means
#' @param Sigma_function a function that maps a vector of times to a
#' T-K-by-d-by-d array of covariance matrices
#' @param pi_function a function that maps a vector of times to a T-by-K vector
#' of probabilities
#' @param num_points a T vector of integers giving the number of points n_t to
#' generate at each time point t.
#' @export
generate_smooth_gauss_mix <- function(mu_function,
                                      Sigma_function,
                                      pi_function,
                                      num_points) {
  times <- seq_along(num_points)
  mu <- mu_function(times)
  Sigma <- Sigma_function(times)
  pi <- pi_function(times)
  K <- ncol(pi) # number of components
  d <- dim(mu)[3]
  dimnames(mu) <- list(NULL, paste0("cluster", 1:K), NULL)
  
  z <- list() # z[[t]][i] = class of point i at time t
  y <- list() # y[[t]][i,] = d-vector of point i at time t
  for (t in times) {
    z[[t]] <- apply(stats::rmultinom(num_points[t], 1, pi[t, ]) == 1, 2, which)
    y[[t]] <- matrix(NA, num_points[t], d)
    for (k in 1:K) {
      ii <- z[[t]] == k # index of points in component k at time t
      if (sum(ii) == 0) next
      if (d == 1)
        y[[t]][ii, ] <- stats::rnorm(n = sum(ii),
                                                   mean = mu[t, k, ],
                                                   sd = Sigma[t, k, , ])
      else
        y[[t]][ii, ] <- mvtnorm::rmvnorm(n = sum(ii),
                                                       mean = mu[t, k, ],
                                                       sigma = Sigma[t, k, , ])
    }
  }
  list(y = y, z = z, mu = mu, Sigma = Sigma, pi = pi)
}
