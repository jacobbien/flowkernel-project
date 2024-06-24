# Generated from _main.Rmd: do not edit by hand

#' Plot cluster means as 1-d projection over time, with all three dimensions plotted together, in separate plots
#' @param mu a T-by-K-by-d array of means
#' @export
plot_1d_means_triple <- function(mu) {
  # Create an empty data frame for the first plot
  df1 <- data.frame(time = seq_along(mu[, 1, 1]))
  
  # Use a for loop to append each column to the data frame
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df1[[col_name]] <- mu[, k, 1]
  }
  
  # Create the ggplot with multiple line plots for the first plot
  pi_plt1 <- ggplot2::ggplot(df1, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df1[, k + 1], color = paste("Cluster", k)), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = "Diameter") +
    ggplot2::ggtitle("Means of Diameter Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::guides(size = "none")  # To remove the size legend
  
  # Create an empty data frame for the second plot
  df2 <- data.frame(time = seq_along(mu[, 1, 1]))
  
  # Use a for loop to append each column to the data frame for the second plot
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df2[[col_name]] <- mu[, k, 2]
  }
  
  # Create the ggplot with multiple line plots for the second plot
  pi_plt2 <- ggplot2::ggplot(df2, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df2[, k + 1], color = paste("Cluster", k)), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = "chl_small") +
    ggplot2::ggtitle("Means of chl_small Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::guides(size = "none")  # To remove the size legend
  
  # Create an empty data frame for the third plot
  df3 <- data.frame(time = seq_along(mu[, 1, 1]))
  # Use a for loop to append each column to the data frame for the third plot
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df3[[col_name]] <- mu[, k, 3]
  }
  
  # Create the ggplot with multiple line plots for the third plot
  pi_plt3 <- ggplot2::ggplot(df3, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df3[, k + 1], color = paste("Cluster", k)), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = "PE") +
    ggplot2::ggtitle("Means of PE Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::guides(size = "none")  # To remove the size legend
  
  # Arrange the three plots vertically
  combined_plot <- gridExtra::grid.arrange(pi_plt1, pi_plt2, pi_plt3, ncol = 1)
  
  return(combined_plot)
}
