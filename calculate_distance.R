#' This function was created by the UC Davis Datalab to measure the distance between
#' observations and the closest point of the coast
#' 
#' Calculate the distance from each point to a shape
#' @param points points of class `sf`, which can take the form of a `data.frame` as long as it is also an `sf` points object
#' @param shape shape to measure from, which should be a sf object, where the CRS matches tht of `points`
#' @param coords variable names for the location coordinates, used to figure out the unique locations within `points`
#' @return vector of distances from each of `points` to the nearest part of `shape`
#'
calculate_distance <- function( points, shape, coords=c("X", "Y") ) {
  # find the unique locations
  unique_locs <- unique( points[, coords] )
  unique_locs[[ 'index' ]] <- 1:nrow(unique_locs)
  loc_indx <- left_join( as_tibble(points), as_tibble(unique_locs), by=coords )[[ 'index' ]]
  
  
  # calculate the offshore distance and convert meters to km
  distance_offshore <- as.vector( st_distance(unique_locs, shape) / 1000)

  # return the distances
  distance_offshore[ loc_indx ]
}