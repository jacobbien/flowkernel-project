# Generated from _main.Rmd: do not edit by hand

#' Plot cluster means as 1-d projection over time, with line widths determined by pi
#' @param mu a T-by-K-by-d array of means
#' @param pi A T-by-K array, with each row consisting of probabilities that sum to one
#' @export
plot_1d_means_with_width <- function(mu, pi, dim = 1) {
  # Create an empty data frame
  df <- data.frame(time = seq_along(pi[, 1]))
  
  # Use a for loop to append each column to the data frame
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df[[col_name]] <- mu[, k, dim]
  }
  
  y_label <- switch(dim,
                    "1" = "Diameter",
                    "2" = "chl_small",
                    "3" = "pe",
                    paste("Dimension", dim))
  
  # Create the ggplot with multiple line plots
  pi_plt <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k), linewidth = pi[, k]), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::ggtitle("Cluster Means Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::guides(linewidth = "none")  # To remove the linewidth legend
  
  return(pi_plt)
}
