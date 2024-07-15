# Generated from _main.Rmd: do not edit by hand

#' Plot cluster means as 1-d projection over time, with line widths determined by pi
#' @param mu a T-by-K-by-d array of means
#' @param pi A T-by-K array, with each row consisting of probabilities that sum to one
#' @export
plot_1d_means_with_width <- function(y, mu, pi, dim = 1) {
  
  y <- purrr::map(y, ~ .x[, dim, drop = FALSE])
  y_label <- ifelse(is.null(colnames(y[[1]])), paste0("V", dim), colnames(y[[1]])[dim])
  
  # Create an empty data frame
  df <- data.frame(time = seq_along(pi[, 1]))
  
  # Use a for loop to append each column to the data frame
  for (k in 1:ncol(mu)) {
    col_name <- paste("Cluster", k)
    df[[col_name]] <- mu[, k, dim]
  }

  # Create the ggplot with multiple line plots
  fig <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    lapply(1:ncol(mu), function(k) {
      ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k), linewidth = pi[, k]), linetype = "solid")
    }) +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::ggtitle("Cluster Means Over Time") +
    ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu))) +
    ggplot2::guides(linewidth = "none")  # To remove the linewidth legend
  
  return(fig)
}
