#' Alr transformation
#'
#' @param data Compositional columns of dataframe
#' @param comp_labels The labels of the compositional columns.
#' @param comparison_component Name of component that all other components will be compared to.
#' @export
alr_trans <- function(data, comparison_component = NULL, comp_labels) {
  if (is.null(comparison_component)){
    stop("comparison_component must be specified for alr transformation. It is the component which all other components will be expressed as ratios relative to.")
  }
  if (!(comparison_component %in% comp_labels)){
    stop("comparison_component must appear in comp_labels")
  }
  comp_labels_without_cc <- comp_labels[comp_labels != comparison_component]
  y <- data[, comp_labels_without_cc]
  for (label in comp_labels_without_cc){
    y[, label]<- log(y[, label]/data[, comparison_component])
  }
  colnames(y) <- paste0("alr_",colnames(y), comparison_component)
  return(y)
}
