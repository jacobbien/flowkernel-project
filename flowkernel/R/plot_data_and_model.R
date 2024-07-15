# Generated from _main.Rmd: do not edit by hand

#' Plot data colored by cluster assignment with cluster means
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
#' @param z a length T list with `z[[t]]` being a n_t vector of cluster assignments
#' @param mu a T-by-K-by-d array of means
#' @param dim an integer which specifies which dimension of the data to plot. Defaults to a vector - `c(1:ncol(y[[1]]))` - that will plot all dimensions together
#' @param show_data a Boolean variable which determines whether data points are plotted along with the cluster centers or not. Defaults to `TRUE`
#' @export
plot_data_and_model <- function(y, z, mu, dim = c(1:ncol(y[[1]])), show_data = TRUE) {
  d <- ncol(y[[1]])
  K <- ncol(mu)
  ntimes <- length(z)
  
  if (d == 1) {
    y_label <- ifelse(is.null(colnames(y[[1]])), paste0("V", dim), colnames(y[[1]])[dim])
    
    df <- data.frame(time = seq_along(mu[, 1, 1]))
    for (k in 1:ncol(mu)) {
      col_name <- paste("Cluster", k)
      df[[col_name]] <- mu[, k, 1]
    }
    
    plt <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
      lapply(1:ncol(mu), function(k) {
        ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k)), linetype = "solid")
      }) +
      ggplot2::labs(x = "Time", y = y_label) +
      ggplot2::ggtitle("Model Means") +
      ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu)))
    
    fig <- plotly::ggplotly(plt, dynamicTicks = TRUE)
    
    if (show_data == TRUE) {
      dat_df <- purrr::map2_dfr(z, y, ~ tibble::tibble(z = as.factor(.x), y = .y), .id = "time") %>%
      dplyr::mutate(time = as.numeric(.data$time))
    
      means_df <- tibble::as_tibble(mu[, , 1]) %>%
        dplyr::mutate(time = dplyr::row_number()) %>%
        tidyr::pivot_longer(-.data$time, names_to = "cluster", values_to = "Mean") %>%
        dplyr::mutate(cluster = as.factor(cluster))
      
      fig <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = means_df,
          ggplot2::aes(x = .data$time, y = .data$Mean, group = .data$cluster)
        ) +
        ggplot2::geom_point(
          data = dat_df,
          ggplot2::aes(x = .data$time, y = .data$y, color = .data$z), alpha = 0.2
        ) +
        ggplot2::labs(x = "Time", y = y_label, title = "Data and Model")
    }
  } else if (d == 3) {
    z_dat <- unlist(z)
    
    if (isTRUE(all.equal(dim, c(1, 2, 3)))) {
      if (is.null(colnames(y[[1]]))) {
        x_label <- "V1"
        y_label <- "V2"
        z_label <- "V3"
      } else {
        x_label <- colnames(y[[1]])[1]
        y_label <- colnames(y[[1]])[2]
        z_label <- colnames(y[[1]])[3]
      }
    
      max_val <- list()
      max_val_time <- list()
      min_val <- list()
      min_val_time <- list()
      for (dd in seq(d)) {
        max_val[[dd]] <- sapply(y, function(mat) max(mat[, dd]))
        max_val_time[[dd]] <- max(max_val[[dd]])
        min_val[[dd]] <- sapply(y, function(mat) min(mat[, dd]))
        min_val_time[[dd]] <- min(min_val[[dd]])
      }
      
      y <- y %>%
        purrr::map_dfr(~ tibble::tibble(x = .x[, 1], y = .x[, 2], z = .x[, 3]), .id = "time") %>%
        dplyr::mutate(z1 = z_dat) %>%
        dplyr::mutate(time = as.integer(time))
      
      cluster_data_frames <- vector("list", length = K)
      for (kk in seq(K)) {
        cluster_mean <- mu[, kk, ]
        data <- data.frame(
          X1 = cluster_mean[, 1],
          X2 = cluster_mean[, 2],
          X3 = cluster_mean[, 3],
          time = 1:ntimes
        )
        cluster_data_frames[[kk]] <- data
      }
      
      if (show_data == FALSE) {
        fig <- plotly::plot_ly(colors = colorRamp(c("blue", "orange", "red"))) %>% 
          plotly::layout(title = 'Model Means')
      } else {
        fig <- y %>% plotly::plot_ly(
          x = ~x, y = ~y, z = ~z, color = ~z1,
          type = "scatter3d", frame = ~time, mode = "markers", size = 80,
          colors = colorRamp(c("blue", "orange", "red"))
        ) %>%
          plotly::layout(title = 'Data and Model')
        
        updatemenus <- list(
          list(
            active = 0,
            type = 'buttons',
            buttons = list(
              list(
                label = "Data Points",
                method = "update",
                args = list(list(visible = c(TRUE, rep(c(TRUE, TRUE), K))))
              ),
              list(
                label = "No Data Points",
                method = "update",
                args = list(list(visible = c(FALSE, rep(c(TRUE, TRUE), K))))
              )
            )
          )
        )
      }
      
      for (kk in seq(K)) {
        fig <- fig %>%
          plotly::add_markers(data = cluster_data_frames[[kk]], x = ~X1, y = ~X2, z = ~X3,
                              color = as.factor(kk), size = 120, frame = ~time)
        if (show_data == TRUE) {
          fig <- fig %>%
            plotly::layout(updatemenus = updatemenus)
        }
      }
      
      fig <- fig %>% 
        plotly::layout(scene = list(
          xaxis = list(title = x_label, range = c(1.1 * min_val_time[[1]], 1.1 * max_val_time[[1]])),
          yaxis = list(title = y_label, range = c(1.1 * min_val_time[[2]], 1.1 * max_val_time[[2]])),
          zaxis = list(title = z_label, range = c(1.1 * min_val_time[[3]], 1.1 * max_val_time[[3]])),
          aspectmode = "manual",  # Set aspect ratio to manual
          aspectratio = list(x = 1, y = 1, z = 1)  # Specify the fixed aspect ratio
        ))
    } else {
      y <- purrr::map(y, ~ .x[, dim, drop = FALSE])
      y_label <- ifelse(is.null(colnames(y[[1]])), paste0("V", dim), colnames(y[[1]])[dim])
      
      df <- data.frame(time = seq_along(mu[, 1, 1]))
      for (k in 1:ncol(mu)) {
        col_name <- paste("Cluster", k)
        df[[col_name]] <- mu[, k, dim]
      }
      
      
      plt <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
        lapply(1:ncol(mu), function(k) {
          ggplot2::geom_line(ggplot2::aes(y = df[, k + 1], color = paste("Cluster", k)), linetype = "solid")
        }) +
        ggplot2::labs(x = "Time", y = y_label) +
        ggplot2::ggtitle("Cluster Means Over Time") +
        ggplot2::scale_color_manual(name = "Cluster", values = rainbow(ncol(mu)))

      fig <- plotly::ggplotly(plt, dynamicTicks = TRUE)
      
      if (show_data == TRUE) {
        dat_df <- purrr::map2_dfr(z, y, ~ tibble::tibble(z = as.factor(.x), y = .y), .id = "time") %>%
        dplyr::mutate(time = as.numeric(.data$time))
      
        means_df <- tibble::as_tibble(mu[, , dim]) %>%
          dplyr::mutate(time = dplyr::row_number()) %>%
          tidyr::pivot_longer(-.data$time, names_to = "cluster", values_to = "Mean") %>%
          dplyr::mutate(cluster = as.factor(cluster))
      
        fig <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = means_df,
            ggplot2::aes(x = .data$time, y = .data$Mean, group = .data$cluster)
          ) +
          ggplot2::geom_point(
            data = dat_df,
            ggplot2::aes(x = .data$time, y = .data$y, color = .data$z), alpha = 0.2
          ) +
          ggplot2::labs(x = "Time", y = y_label, title = "Data and Model")
      }
    }
  }
  return(fig)
}
