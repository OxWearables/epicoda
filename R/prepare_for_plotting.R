#' Generates list of fixed_values based on median/modal values in dataset
#'
#' If fixed values are not set, this will set them to modal/ median values.
#'
#' @param data Data used for model development.
#' @param comp_labels Labels of the compositional componenets.Should be column names of columns in \code{data}.
#' @return dataframe with a single row of fixed_values.
generate_fixed_values <- function(data, comp_labels){
  fixed_values <- data.frame(matrix(ncol = 0, nrow = 1))
  others <- colnames(data)[!(colnames(data) %in% comp_labels)]
  for (colname in others){
    if (is.factor(data[, colname])== TRUE){
      fixed_values[colname] <- (Mode(data[,colname]))
    }
    else {
      fixed_values[colname] <- median(data[, colname], na.rm = TRUE)
    }
  }
  cm <- comp_mean(data, comp_labels)
  fixed_values <- cbind(cm, fixed_values)
  return(fixed_values)
}




#' Varies component of interest.
#'
#' Produces variable going between percentiles of the component of interest in the data.
#'
#' @param component_of_interest The variable of interest.
#' @param lower_quantile The lower quantile to vary from.
#' @param upper_quantile The upper quantile to vary to.
#' #' @param granularity Doesn't usually need setting. Parameter indicating how many predictions to make. If too low, plotted curve has gaps. If too high, calculation is slow.
#' @return Vector of values going from \code{lower_quantile} to \code{upper_quantile} of the distribution of the varaible of interest.
vary_component_of_interest <- function(component_of_interest,
                                  lower_quantile = 0.05,
                                  upper_quantile = 0.95,
                                  granularity = 10000) {
  component_values <- seq(
    from = quantile(component_of_interest, 0.05, na.rm = TRUE),
    to = quantile(component_of_interest, 0.95, na.rm = TRUE),
    length.out = granularity
  )
  return(component_values)
}


# Generate a new dataset

#' make_new_data: Generates a new dataset varying in the dimension of interest.
#'
#' Generates a new dataset to feed into the plotting functions.
#'
#' @param from_component Component to plot transfer out of. Of the two being considered, should be component with generally higher values (e.g. higher median)
#' @param to_component Component to plot transfer into. Of the two being considered, should be component with generally lower values (e.g. lower median.)
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
                          units = NULL,
                          comp_labels,
                          lower_quantile = 0.05,
                          upper_quantile = 0.95,
                          granularity = 10000) {
  new_data <- data.frame()[1:10000,]
  if (units == "hr/wk"){
    comp_sum <- 24*7
  }
  if (units == "unitless"){
    comp_sum <- 1
  }
  if (units == "hr/day"){
    comp_sum <- 24
  }
  if (units == "min/day"){
    comp_sum <- 60*24
  }
  if (units == "min/wk"){
    comp_sum <- 60*24*7
  }
  if (!(units %in% c("hr/wk", "hr/day", "min/wk", "min/day", "unitless"))){
    stop("Unrecognised value for units. units should be \"hr/wk\", \"hr/day\", \"min/wk\", \"min/day\" or \"unitless\"")
  }
  for (label in comp_labels) {
    if (label == to_component) {
      this_col <- data.frame(vary_component_of_interest(dataset[, label],
                                                   lower_quantile,
                                                   upper_quantile))
      new_data[label] <- this_col
    }
    else{
      this_col <- data.frame(rep(fixed_values[1, label], by = 10000))
      new_data[label] <- this_col
    }
  }
  for (label in colnames(dataset)[!(colnames(dataset) %in% comp_labels)]){
    this_col <- data.frame(rep(fixed_values[1, label], by = 10000))
    new_data[label] <- this_col
  }
  tf <- rep(comp_sum, by = 10000)
  for (label in comp_labels[comp_labels != from_component]){
    tf <- tf - new_data[, label]
  }
  new_data[, from_component] <- tf

  return(new_data)
}
