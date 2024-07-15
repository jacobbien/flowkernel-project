# Generated from _main.Rmd: do not edit by hand

#' Plot cluster means as 1-d projection over time, with all three dimensions plotted together, in separate plots
#' @param mu a T-by-K-by-d array of means
#' @export
plot_1d_means_triple <- function(y, mu) {
  # Convert the 3D array into a long data frame for ggplot2
  long_df <- data.frame(
    time = rep(seq_along(mu[, 1, 1]), times = ncol(mu) * dim(mu)[3]),
    value = as.vector(mu),
    cluster = factor(rep(rep(1:ncol(mu), each = dim(mu)[1]), times = dim(mu)[3])),
    dimension = factor(rep(rep(1:dim(mu)[3], each = dim(mu)[1] * ncol(mu)), times = 1))
  )
  
  if (is.null(colnames(y[[1]]))) {
        label_1 <- "V1"
        label_2 <- "V2"
        label_3 <- "V3"
      } else {
        label_1 <- colnames(y[[1]])[1]
        label_2 <- colnames(y[[1]])[2]
        label_3 <- colnames(y[[1]])[3]
      }
  
  # Create the labels for the facets
  facet_labels <- c("1" = label_1, "2" = label_2, "3" = label_3)
  
    # Calculate the range for each dimension
  range_df <- long_df %>%
    dplyr::group_by(dimension) %>%
    dplyr::summarize(min_value = min(value), max_value = max(value)) %>%
    dplyr::mutate(padding = 0.1 * (max_value - min_value), 
           y_min = min_value - padding, 
           y_max = max_value + padding)
  
  # Merge the range information back to the long data frame
  long_df <- merge(long_df, range_df, by = "dimension")
  
  # Create the ggplot with facet_grid and custom y-limits for each plot
  plt <- ggplot2::ggplot(long_df, ggplot2::aes(x = time, y = value, color = cluster)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "Value") +
    ggplot2::facet_grid(dimension ~ ., scales = "free_y", labeller = ggplot2::labeller(dimension = facet_labels)) +
    ggplot2::ggtitle("Cluster Means Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::geom_blank(ggplot2::aes(y = y_min)) +  # Add blank points for custom y-limits
    ggplot2::geom_blank(ggplot2::aes(y = y_max))
  
  return(plt)
}
