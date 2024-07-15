# Generated from _main.Rmd: do not edit by hand

#' Plot biomass over time for each cluster
#' @param biomass A list of length T, where each element `biomass[[t]]` is a numeric vector of length n_t containing the biomass (or count) of particles in each bin
#' @param resp length T list with `y[[t]]` being a n_t-by-K matrix
#' @export
plot_biomass <- function(biomass, resp) {
  K <- ncol(resp[[1]])
  ntimes <- length(resp)
  
  # Initialize a list to hold data frames for each cluster
  data_list <- vector("list", K)
  
  for (k in 1:K) {
    cluster_biomass <- sapply(1:ntimes, function(tt) sum(resp[[tt]][, k] * biomass[[tt]]))
    data_list[[k]] <- data.frame(time = seq_along(cluster_biomass), Cluster = paste("Cluster", k), Biomass = cluster_biomass)
  }
  
  # Combine all the data frames into one
  df <- do.call(rbind, data_list)
  
  # Create the ggplot with multiple line plots
  plt <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = Biomass, color = Cluster)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = "Cluster Biomass") +
    ggplot2::ggtitle("Cluster Biomass Over Time")
  
  # Convert ggplot to plotly for interactivity
  pi_plotly <- plotly::ggplotly(plt, dynamicTicks = TRUE)
  
  return(pi_plotly)
}

