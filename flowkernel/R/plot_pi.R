# Generated from _main.Rmd: do not edit by hand

#' Plot cluster populations (pi) over time
#' @param pi A T-by-K array, with each row consisting of probabilities that sum to one
#' @export
plot_pi <- function(pi) {
  # Create an empty data frame
  df <- data.frame(time = seq_along(pi[, 1]))
  
  # Use a for loop to append each column to the data frame
  for (k in 1:ncol(pi)) {
    col_name <- paste("Cluster", k)
    df[[col_name]] <- pi[, k]
  }
  
  # Create the ggplot with multiple line plots
  pi_plt <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    lapply(1:ncol(pi), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k)), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = "Pi") +
    ggplot2::ggtitle("Pi Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(pi)))
  
  # Convert ggplot to plotly for interactivity
  fig <- plotly::ggplotly(pi_plt, dynamicTicks = TRUE)
  
  return(fig)
}
