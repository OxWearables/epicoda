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
#' @param dataset
#' @param fixed_values
#' @param PA_labels
#' @param lower_quantile See \code{vary_time_of_interest}
#' @param upper_quantile See \code{vary_time_of_interest}
#' @return Dataframe varying in time of interest.
#' @examples
#'
#' @export
make_new_data <- function(time_from,
                          time_to,
                          dataset,
                          fixed_values,
                          PA_labels,
                          lower_quantile = 0.05,
                          upper_quantile = 0.95) {
  new_data <- data.frame()[1:10000,]
  for (column in colnames(dataset)) {
    if (column == time_to) {
      this_col <- data.frame(vary_time_of_interest(dataset[, column],
                                                   lower_quantile,
                                                   upper_quantile))
      new_data[column] <- this_col
    }
    else{
      this_col <- data.frame(rep(fixed_values[1, column], by = 10000))
      new_data[column] <- this_col
    }
  }
  if (PA_labels == "int_class") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$vigorous - new_data$moderate -
      new_data$non_active - new_data$light + new_data[, time_from]
  }
  if (PA_labels == "ml_class_5") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$sedentary - new_data$moderate -
      new_data$sleep - new_data$walking - new_data$tasks.light + new_data[, time_from]
  }
  if (PA_labels == "ml_class_4") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$sedentary - new_data$modwalk -
      new_data$sleep - new_data$tasks.light + new_data[, time_from]
  }
  if (PA_labels == "ml_with_vig") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$moderate - new_data$vigorous -
      new_data$walking - new_data$tasks.light - new_data$sleep - new_data$sedentary + new_data[, time_from]

  }
  if (PA_labels == "int_with_sleep") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$moderate - new_data$vigorous -
      new_data$light - new_data$sleep - new_data$waking_non_active + new_data[, time_from]
  }
  if (PA_labels == "ml_with_vig_mw") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$modwalk - new_data$vigorous -
      new_data$sedentary - new_data$sleep - new_data$tasks.light + new_data[, time_from]
  }
  if (PA_labels == "nonsleep_behav") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$moderate - new_data$vigorous -
      new_data$walking - new_data$tasks.light - new_data$sleep - new_data$sedentary +
      new_data[, time_from]

  }
  if (PA_labels == "compromise") {
    new_data[, time_from] <-
      rep(24  * 7, by = 10000) - new_data$moderate - new_data$vigorous -
      new_data$light - new_data$sleep - new_data$sedentary +
      new_data[, time_from]

  }
  return(new_data)
