#' Compositional mean
#'
#'  Calculates the compositional mean of a dataset.
#'
#' @param data Dataset to calculate compositional mean of.
#' @param comp_labels The labels of the compositional component.
#' @return Vector which is the compositional mean.
#' @examples #TBA
#' @export
comp_mean <- function(data, comp_labels, rounded_zeroes = FALSE, det_limit = NULL){
  compos_mean <- c()
  if (rounded_zeroes & is.null(det_limit)){
    stop("det_limit must be set for zeroes to be imputed. It should be the minimum measurable value in the compositional
         columns of data.")
  }
  dCompOnly <- data[, comp_labels]
  dDropped <- data
  if (any(dCompOnly ==0) & rounded_zeroes){
    print(paste("imputing zeroes with detection limit", det_limit))
    dCompOnly <- zCompositions::lrEM(dCompOnly, label = 0, dl = matrix(data = rep(det_limit,length(dCompOnly[,1])*ncol(dCompOnly)),
                                                                       nrow = length(dCompOnly[,1]),
                                                                       byrow = T), max.iter = 50)
  }
  else{
    for (activity in comp_labels){
      dDropped <- dDropped[dCompOnly[,activity] != 0, ]
      dCompOnly <- dCompOnly[dCompOnly[,activity] != 0, ]
    }
  }
  for (activity_type in comp_labels){
    compos_mean[activity_type] <- gm(dCompOnly[,activity_type])
  }
  tot_time <- sum(compos_mean)
  comp_mean_normalised <- compos_mean/tot_time
  names(comp_mean_normalised) <- comp_labels
  return(comp_mean_normalised)
}
