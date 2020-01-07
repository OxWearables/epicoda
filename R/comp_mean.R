#' Compositional mean
#'
#'  Calculates the compositional mean of a dataset.
#'
#' @param data Dataset to calculate compositional mean of.
#' @param comp_labels The labels of the compositional parts.
#' @inheritParams process_zeroes
#' @inheritParams process_units
#' @return Vector which is the compositional mean.
#' @examples
#' comp_mean(data = simdata,
#'           comp_labels = c("partA", "partB", "partC", "partD", "partE"),
#'           units = "hr/day")
#' @export
comp_mean <- function(data, comp_labels, rounded_zeroes = FALSE, det_limit = NULL, units = "unitless", specified_units = NULL){

  compos_mean <- data.frame(matrix(nrow = 1, ncol = 0))

  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]

  dCompOnly <- data[, comp_labels]
  dCompOnly <- process_zeroes(dCompOnly, comp_labels, rounded_zeroes, det_limit)

  for (activity_type in comp_labels){
    compos_mean[,activity_type] <- gm(dCompOnly[,activity_type])
  }
  tot_time <- apply(compos_mean[1, ], 1, sum)

  comp_mean_normalised <- compos_mean/tot_time
  names(comp_mean_normalised) <- comp_labels
  cm <- comp_mean_normalised*comp_sum
  return(cm)
}
