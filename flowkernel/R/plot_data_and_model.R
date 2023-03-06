# Generated from _main.Rmd: do not edit by hand

#' Plot data colored by cluster assignment with cluster means when `d=1`
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param z a length T list with `z[[t]]` being a n_t vector of cluster assignments
#' @param mu a T-by-K-by-d array of means
plot_data_and_model <- function(y, z, mu) {
  dat_df <- purrr::map2_dfr(z, y, ~ tibble::tibble(z = as.factor(.x), y = .y),
                     .id = "time") %>%
    dplyr::mutate(time = as.numeric(.data$time))
  means_df <- tibble::as_tibble(mu[, , 1]) %>%
    dplyr::mutate(time = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.data$time, names_to = "cluster", values_to = "mean")
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dat_df,
      ggplot2::aes(x = .data$time, y = .data$y, color = .data$z), alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = means_df,
      ggplot2::aes(x = .data$time, y = .data$mean, group = .data$cluster)
    )
}
