# Generated from _main.Rmd: do not edit by hand

#' Kernel-smoothed EM algorithm
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param K number of components
#' @param hmu bandwidth for mu parameter
#' @param hSigma bandwidth for Sigma parameter
#' @param hpi bandwidth for pi parameter
#' @param num_iter number of iterations of EM to perform
#' @export
kernel_em <- function(y, K, hmu, hSigma, hpi, num_iter = 100) {
  num_times <- length(y)
  d <- ncol(y[[1]])
  mu <- array(NA, c(num_times, K, d))
  Sigma <- array(NA, c(num_times, K, d, d))
  pi <- matrix(NA, num_times, K)
  resp <- list() # responsibilities gamma[[t]][i, k]
  
  # use mclust to get initial mu, Sigma, pi for each time t:
  if (d > 1) stop("not yet implemented.  Should call VVV rather than V")
  for (tt in seq(num_times)) {
    fit <- mclust::Mclust(y[[tt]], G = K, modelNames = "V")
    mu[tt, , 1] <- fit$parameters$mean
    Sigma[tt, , 1, 1] <- fit$parameters$variance$sigmasq
    pi[tt, ] <- fit$parameters$pro
  }
  for (l in seq(num_iter)) {
  # E-step: update responsibilities
  if (d > 1) stop("not yet implemented.  Should use mvtnorm::dmvtnorm()")
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
  # M-step: update estimates of (mu, Sigma, pi)
  # do summations over i:
  # form T-by-K matrix summing resp_itk over i
  resp_sum <- purrr::map(resp, ~ colSums(.x)) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE)
  resp_sum_smooth <- apply(
    resp_sum, 2, function(x) stats::ksmooth(1:length(x), x, bandwidth = hpi)$y
  )
  pi <- resp_sum_smooth / rowSums(resp_sum_smooth)
  # form T-by-K-by-d array summing resp_itk * Y_ij over i
  y_sum <- purrr::map2(resp, y, ~ crossprod(.x, .y)) %>% 
    unlist() %>% 
    array(c(K, d, num_times)) %>% 
    aperm(c(3,1,2))
  y_sum_smoothed <- apply(
    y_sum, 2:3, function(x) stats::ksmooth(1:length(x), x, bandwidth = hmu)$y
  )
  for (j in seq(d)) {
    mu[, , j] <- y_sum_smoothed[, , j] / resp_sum_smooth
  }
  # form a T-by-K-by-d-by-d array
  # summing (Y_it - mu_t)*diag(resp_itk)*(Y_it - mu_t)^T over i
  mat_sum <- array(NA, c(num_times, K, d, d))
  for (tt in seq(num_times)) {
    for (k in seq(K)) {
      yy <- y[[tt]] - mu[tt, k, ]
      mat_sum[tt, k, , ] <- crossprod(yy, yy * resp[[tt]][, k]) # YY^T * D * YY
    }
  }
  mat_sum_smoothed <- apply(
    mat_sum, 2:4, function(x) stats::ksmooth(1:length(x), x, bandwidth = hSigma)$y
  )
  for (j in seq(d))
    for (l in seq(d))
      Sigma[, , j, l] <- mat_sum_smoothed[, , j, l] / resp_sum_smooth
  }
  dimnames(mu) <- list(NULL, paste0("cluster", 1:K), NULL)
  dimnames(Sigma) <- list(NULL, paste0("cluster", 1:K), NULL, NULL)
  dimnames(pi) <- list(NULL, paste0("cluster", 1:K))
  list(mu = mu, Sigma = Sigma, pi = pi, resp = resp)
}
