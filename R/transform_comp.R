#' Transform compositional variables to ilr pivot coordinates
#'
#' Takes a dataset with a set of labelled compositional columns and returns a dataset with
#' transformed variables added.
#'
#' @param data Dataset to have the transformation applied to.
#' @param comp_labels The labels of the compositional columns.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data.
#' @param component_1 First component in ilr-pivot coordinate transformation. Passed to \code{alter_order_comp_labels} unless \code{NULL}.
#' @return \code{data} with ilr-transformed activity columns.
#' @examples TBA
#' @export
transform_comp <- function(data, comp_labels, rounded_zeroes = TRUE, det_limit = NULL, component_1 = NULL){
  if (rounded_zeroes = TRUE & is.null(det_limit)){
    stop("det_limit must be set for zeroes to be imputed. It should be the minimum measurable value in the compositional
         columns of data.")
  }
  if (!is.null(component_1)){
    comp_labels <- alter_order_comp_labels(comp_labels, component_1)
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
      dDropped <- dDropped[dCompOnly[activity] != 0, ]
      dCompOnly <- dCompOnly[dCompOnly[activity] != 0, ]
    }
  }

  dTransformed <- ilr_trans(dCompOnly)
  transf_labels <- transf_labels(comp_labels)
  colnames(dTransformed) <- transf_labels
  dOut <- data.frame(dDropped[, !colnames(dDropped) %in% colnames(dCompOnly)], dCompOnly, dTransformed)
  return(dOut)
}
