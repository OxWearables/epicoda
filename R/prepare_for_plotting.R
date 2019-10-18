#' Varies component of interest.
#'
#' Produces variable going between percentiles of the component of interest in the data.
#'
#' @param component_of_interest The variable of interest.
#' @param lower_quantile The lower quantile to vary from.
#' @param upper_quantile The upper quantile to vary to.
#' @return Vector of values going from \code{lower_quantile} to \code{upper_quantile} of the distribution of the varaible of interest.
vary_component_of_interest <- function(component_of_interest,
                                  lower_quantile = 0.05,
                                  upper_quantile = 0.95) {
  component_values <- seq(
    from = quantile(component_of_interest, 0.05, na.rm = TRUE),
    to = quantile(component_of_interest, 0.95, na.rm = TRUE),
    length.out = 10000
  )
  return(component_values)
}


# Generate a new dataset

#' make_new_data: Generates a new dataset varying in the dimension of interest.
#'
#' Generates a new dataset to feed into the plotting functions.
#'
#' @param from_component
#' @param to_component
#' @param comp_sum Numeric value indicating the value compositional columns should sum to (e.g. 1, 24, 168, 10080).
#' @param fixed_values Should be a dataframe with a column for each component and containing a single value for each of those.
#' @param dataset Dataset to take distribution of \code{from_component} from.
#' @param comp_labels Labels of compositional columns.
#' @param lower_quantile See \code{vary_time_of_interest}
#' @param upper_quantile See \code{vary_time_of_interest}
#' @param granularity Doesn't usually need setting. Parameter indicating how many predictions to make. If too low, plotted curve has gaps. If too high, calculation is slow.
#' @return Dataframe varying in time of interest.
#' @examples
#'
#' @export
make_new_data <- function(from_component,
                          to_component,
                          fixed_values,
                          dataset,
                          comp_sum = sum(fixed_values[1,comp_labels]),
                          comp_labels,
                          lower_quantile = 0.05,
                          upper_quantile = 0.95,
                          granularity = 10000) {
  new_data <- data.frame()[1:10000,]
  for (label in comp_labels) {
    if (label == to_component) {
      this_col <- data.frame(vary_time_of_interest(dataset[, label],
                                                   lower_quantile,
                                                   upper_quantile))
      new_data[column] <- this_col
    }
    else{
      this_col <- data.frame(rep(fixed_values[1, column], by = 10000))
      new_data[column] <- this_col
    }
  }
  tf <- rep(comp_sum, by = 10000)
  for (label in comp_labels){
    tf <- tf - new_data[, label]
  }
  tf <- tf + new_data[,to_component]
  new_data[, from_component]<- tf
  return(new_data)
}
