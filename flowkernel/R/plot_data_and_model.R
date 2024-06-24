# Generated from _main.Rmd: do not edit by hand

#' Plot data colored by cluster assignment with cluster means
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param z a length T list with `z[[t]]` being a n_t vector of cluster assignments
#' @param mu a T-by-K-by-d array of means
#' @export
plot_data_and_model <- function(y, z, mu) {
  d <- ncol(y[[1]])
  K <- ncol(mu)
  ntimes = length(z)
  if (d == 1){
  dat_df <- purrr::map2_dfr(z, y, ~ tibble::tibble(z = as.factor(.x), y = .y),
                     .id = "time") %>%
    dplyr::mutate(time = as.numeric(.data$time))
  means_df <- tibble::as_tibble(mu[, , 1]) %>%
    dplyr::mutate(time = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.data$time, names_to = "cluster", values_to = "mean")
  fig <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dat_df,
      ggplot2::aes(x = .data$time, y = .data$y, color = .data$z), alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = means_df,
      ggplot2::aes(x = .data$time, y = .data$mean, group = .data$cluster)
    ) +
    ggplot2::labs(x = "Time", y = "Cell Diameter")  # Label the x-axis and y-axis
  }
  else if (d == 3) {
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
  fig <- y %>% plotly::plot_ly(
    x = ~x, y = ~y, z = ~z, color = ~z1,
    type = "scatter3d", frame = ~time, mode = "markers", size = 80,
    colors = colorRamp(c("blue", "orange", "red"))) %>%
    plotly::layout(scene = list(
      xaxis = list(title = "Diameter", range = c(1.1* min_val_time[[1]], 1.1 *max_val_time[[1]])),
      yaxis = list(title = "Chl_Small", range = c(1.1* min_val_time[[2]], 1.1 *max_val_time[[2]])),
      zaxis = list(title = "PE", range = c(1.1* min_val_time[[3]], 1.1 *max_val_time[[3]])),
      aspectmode = "manual",  # Set aspect ratio to manual
      aspectratio = list(x = 1, y = 1, z = 1)  # Specify the fixed aspect ratio
    ))
  updatemenus <- list(
    list(
      active = 0,
      type= 'buttons',
      buttons = list(
        list(
          label = "Data Points",
          method = "update",
          args = list(list(visible = c(TRUE, rep(c(TRUE, TRUE), K))))),
        list(
          label = "No Data Points",
          method = "update",
          args = list(list(visible = c(FALSE, rep(c(TRUE, TRUE), K))))))
    )
  )
  for (kk in seq(K)) {
    fig <- fig %>%
        plotly::add_markers(data = cluster_data_frames[[kk]], x = ~X1, y = ~X2, z = ~X3,
                    color = kk, size = 120, frame = ~time)%>%
      plotly::layout(updatemenus = updatemenus)
  }
  }
  return(fig)
}
