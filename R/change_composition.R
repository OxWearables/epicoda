#' Change composition
#'
#' This makes a change to a composition in the specified way and automatically rebalances.
#'
#' @param composition This should be the starting composition as a data frame.
#' @param main_component Component the main change is happening to.
#' @param main_change Magnitude and direction of change e.g. +1, -0.5.
#' @param comp_labels Compositional column labels.
#'
#' @return Composition after rebalancing in the described way.
#' @examples
#' @export
change_composition <-
  function(composition,
           main_component,
           main_change,
           comp_labels){
    new_composition <- data.frame(matrix(nrow = 1, ncol = 0))
    new_composition[, main_component] <-
      composition[, main_component] + main_change
    list_we_need <-
      comp_labels[comp_labels != main_component]
    for (component in list_we_need) {
      new_composition[, component] <-
        (rowSums(composition) - new_composition[, main_component])*(composition[, component] / (rowSums(composition) -
                                                                                 composition[, main_component]))
    }
    return(new_composition)
  }
