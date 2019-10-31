#' Produce a forest plot indicating model prediction at given compositions
#'
#'
#' @param composition_list List of compositions
#' @param model Should be a Cox model.
#' @param comp_labels The labels of the compositional columns.
#' @param transformation_type
#' @param comparison_component Only needed for alr transformation. Should be an element of \code{comp_labels}. Name of component that all other components will be compared to.
#' @param component_1 Only used if First component in ilr-pivot coordinate transformation. Passed to \code{alter_order_comp_labels} unless \code{NULL}.

#' @export
forest_plot_examples <- function(data, model, comp_labels, transformation_type = "ilr", comparison_component = NULL, component_1 = NULL){
  return(y)
}
