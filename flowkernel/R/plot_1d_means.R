# Generated from _main.Rmd: do not edit by hand

#' Plot cluster centers as 1-d projection over time
#' @param mu a T-by-K-by-d array of means
#' @param dim specify which dimension to be plotted: 1 (diam), 2 (chl_small), or 3 (pe)
#' @export
plot_1d_means <- function(mu, dim = 1) {
  # Create an empty data frame
  df <- data.frame(time = seq_along(mu[, 1, 1]))
  
  # Use a for loop to append each column to the data frame
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df[[col_name]] <- mu[, k, dim]
  }
  # Determine the y-axis label based on the dimension
  y_label <- switch(dim,
                    "1" = "Diameter",
                    "2" = "chl_small",
                    "3" = "pe",
                    paste("Dimension", dim))
  
  # Create the ggplot with multiple line plots
  pi_plt <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k)), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::ggtitle("Cluster Means Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu)))
  
  # Convert ggplot to plotly for interactivity
  pi_plotly <- plotly::ggplotly(pi_plt, dynamicTicks = TRUE)
  
  return(pi_plotly)
}
