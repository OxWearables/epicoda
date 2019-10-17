#' Compositional mean
#'
#'  Calculates the compositional mean of a dataset.
#'
#' @param data Dataset to calculate compositional mean of.
#' @param comp_labels The labels of the compositional component.
#' @return Vector which is the compositional mean.
#' @examples #TBA
#' @export
comp_mean <- function(data, comp_labels){
  compos_mean <- c()
  for (activity_type in comp_labels){
    compos_mean[activity_type] <- gm(data[,activity_type])
  }
  tot_time <- sum(compos_mean)
  comp_mean_normalised <- compos_mean/tot_time
  names(comp_mean_normalised) <- comp_labels
  return(comp_mean_normalised)
}
