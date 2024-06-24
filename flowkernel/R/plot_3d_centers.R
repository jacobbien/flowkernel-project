# Generated from _main.Rmd: do not edit by hand

#' Plot cluster centers in 3-d
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param z a length T list with `z[[t]]` being a n_t vector of cluster assignments
#' @param mu a T-by-K-by-d array of means
#' @export
plot_3d_centers <- function(y, z, mu){
  K <- ncol(mu)
  d <- ncol(y[[1]])
  z_dat <- unlist(z)
  ntimes = length(z)
  max_val <- list()
  max_val_time <- list()
  min_val = list()
  min_val_time = list()
  for (dd in seq(d)) {
    max_val[[dd]] <- sapply(y, function(mat) max(mat[, dd]))
    max_val_time[[dd]] <- max(max_val[[dd]])
    min_val[[dd]] <- sapply(y, function(mat) min(mat[, dd]))
    min_val_time[[dd]] <- min(min_val[[dd]])
  }
  y = unname(y)
  y <- y %>% 
    purrr::map_dfr(~ tibble::tibble(x = .x[, 1], y = .x[, 2], z = .x[, 3]), .id = "time") %>%
    dplyr::mutate(z1 = z_dat)
  y$time = as.integer(y$time)
  
  cluster_data_frames <- vector("list", length = K)
  for (kk in seq(K)) {
    cluster_mean <- mu[, kk, ]
    data <- data.frame(
      X1 = cluster_mean [, 1],
      X2 = cluster_mean [, 2],
      X3 = cluster_mean [, 3],
      time = 1:ntimes
    )
    cluster_data_frames[[kk]] = data
  }
  fig <- plotly::plot_ly()
  for (kk in seq(K)) {
    fig <- fig %>%
      plotly::add_markers(data = cluster_data_frames[[kk]], x = ~X1, y = ~X2, z = ~X3,
                  color = kk, size = 120, frame = ~time)
  }
  
  return(fig)
}
