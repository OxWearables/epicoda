#' @title
#' Alr transformation
#'
#' @description
#' This peforms additive log-ratio transformation.
#'
#' @param data Compositional columns of dataframe.
#' @param comp_labels The labels of the compositional columns.
#' @param comparison_part Name of part that all other parts will be compared to.
#' 
#' @return
#' `data` with alr-transformed compositional columns.
#'
#' @examples
#' alr_trans(
#'   data = simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")],
#'   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
#'   comparison_part = "sleep"
#' )

#' @noRd
alr_trans <- function(data, comp_labels, comparison_part = NULL) {
  if (is.null(comparison_part)){
    stop('`comparison_part` must be specified for alr transformation. It is the part which all other parts will be expressed as ratios relative to.')
  }
  if (!(comparison_part %in% comp_labels)){
    stop('`comparison_part` must appear in `comp_labels`.')
  }
  comp_labels_without_cc <- comp_labels[comp_labels != comparison_part]
  y <- data[, comp_labels_without_cc]
  for (label in comp_labels_without_cc){
    y[, label]<- log(y[, label]/data[, comparison_part])
  }
  colnames(y) <- paste0("alr_",colnames(y), comparison_part)
  return(y)
}
