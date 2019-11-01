#' Compositional mean
#'
#'  Calculates the compositional mean of a dataset.
#'
#' @param data Dataset to calculate compositional mean of.
#' @param comp_labels The labels of the compositional component.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data.
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
  if (any(dCompOnly ==0) & rounded_zeroes){
    print(paste("Please note that zero values were imputed with detection limit", det_limit, "using zCompositions::lrEM"))
    dCompOnly <- zCompositions::lrEM(dCompOnly, label = 0, dl = matrix(data = rep(det_limit,length(dCompOnly[,1])*ncol(dCompOnly)),
                                                                       nrow = length(dCompOnly[,1]),
                                                                       byrow = T), max.iter = 50)
  }
  else{
    print("Please note that zero values were dropped.")
    for (activity in comp_labels){
      dCompOnly <- dCompOnly[dCompOnly[,activity] != 0, ]
    }
  }
  for (activity_type in comp_labels){
    compos_mean[activity_type] <- gm(dCompOnly[,activity_type])
  }
  tot_time <- sum(compos_mean)
  comp_mean_normalised <- compos_mean/tot_time
  names(comp_mean_normalised) <- comp_labels
  cm <- comp_mean_normalised*sum(data[1, comp_labels])
  return(comp_mean_normalised)
}
