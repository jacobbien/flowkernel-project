# Generated from _main.Rmd: do not edit by hand

#' Creates a biomass list of length T, where each element `biomass[[t]]` is a numeric vector of length n_t containing just 1's. 
#' @param y length T list with `y[[t]]` being a n_t-by-d matrix
default_biomass <- function(y) { 
  biomasslist <- vector("list", length(y))
  for (i in 1:length(y)) {
    biomasslist[[i]] <- as.numeric(rep(1, dim(y[[i]])[1]))
  }
  return(biomasslist)
}
