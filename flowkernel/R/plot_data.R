# Generated from _main.Rmd: do not edit by hand

#' Plot raw data when `d = 1`
#' 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' 
#' @export
plot_data <- function(y) {
  stopifnot(ncol(y[[1]]) == 1)
  purrr::map_dfr(y, ~ tibble::tibble(y = .x), .id = "time") %>% 
  dplyr::mutate(time = as.numeric(.data$time)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.2)
}
