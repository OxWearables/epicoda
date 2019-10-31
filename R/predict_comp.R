#' Give model predictions for a particular compositions
#'
#'
#' @param composition_list List of compositions
#' @param model
#' @param comp_labels The labels of the compositional columns.
#' @param transformation_type
#' @param comparison_component Only needed for alr transformation. Should be an element of \code{comp_labels}. Name of component that all other components will be compared to.
#' @param component_1 Only used if First component in ilr-pivot coordinate transformation. Passed to \code{alter_order_comp_labels} unless \code{NULL}.

#' @export
predict_comp <- function(data, model, comp_labels, transformation_type = "ilr", comparison_component = NULL, component_1 = NULL){
  if (class(model) != "coxph"){
    stop("forest_plot_comp is only implemented for Cox models.")
  }
return(y)
}
