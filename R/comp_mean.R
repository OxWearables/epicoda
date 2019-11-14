#' Compositional mean
#'
#'  Calculates the compositional mean of a dataset.
#'
#' @param data Dataset to calculate compositional mean of.
#' @param comp_labels The labels of the compositional component.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data.
#' @param units
#' @param specified_units If units are being specified via the composition sum, this is where it is done. It should be a vector where the first argument is a string describing the units, and the second argument is the expected sum of a composition e.g. \code{c("hr/day", 24)}
#' @return Vector which is the compositional mean.
#' @examples #TBA
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
  print(cm)
  return(cm)
}
