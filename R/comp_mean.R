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

  compos_mean <- c()
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
  if (units == "specified"){
    if (is.null(specified_units)){
      stop("composition_sum must be given if units = \"specified\" ")
    }
    if (!is.character(specified_units[1])){
      stop("The first argument of specified_units must be a string describing the units.")
    }
    if (!is.numeric(specified_units[2])){
      stop("The first argument of specified_units must be a number specifying the sum of a composition.")
    }
    else {
      comp_sum <- composition_sum
    }
  }
  if (!(units %in% c("hr/wk", "hr/day", "min/wk", "min/day", "unitless", "specified"))){
    stop("Unrecognised value for units. units should be \"hr/wk\", \"hr/day\", \"min/wk\", \"min/day\" or \"unitless\"")
  }
  if (rounded_zeroes & is.null(det_limit)){
    stop("det_limit must be set for zeroes to be imputed. It should be the minimum measurable value in the compositional
         columns of data.")
  }
  dCompOnly <- data[, comp_labels]
  if (any(dCompOnly ==0) & rounded_zeroes){
    message(paste("Note that before calculating the compositional mean zero values were imputed with detection limit", det_limit, "using zCompositions::lrEM"))
    dCompOnly <- suppressMessages(zCompositions::lrEM(dCompOnly, label = 0, dl = matrix(data = rep(det_limit,length(dCompOnly[,1])*ncol(dCompOnly)),
                                                                       nrow = length(dCompOnly[,1]),
                                                                       byrow = T), max.iter = 50))
  }
  else{
    message("Note that before calculating the compositional mean zero values were dropped.")
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
  cm <- comp_mean_normalised*comp_sum
  return(cm)
}
