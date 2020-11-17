#' Change composition
#'
#' This makes a change to a composition in the specified way and automatically reallocates to/from other parts of the composition proportionally. It primarily exists as a utility function to make reporting easier.
#'
#' @param composition This should be the starting composition as a data frame.
#' @param main_part part the main change is happening to.
#' @param at_expense_of Parts the main change is at the expense of/ reallocated to.
#' @param main_change Magnitude and direction of change e.g. +1, -0.5.
#' @param comp_labels Compositional column labels.
#'
#' @return Composition after reallocating as described.
#' @examples
#' new_comp <-
#' change_composition(
#'  composition = simdata[1,],
#'  main_part = "moderate",
#'  main_change = +0.5,
#'  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
#'  )
#' @export
change_composition <-
  function(composition,
           main_part,
           at_expense_of = NULL,
           main_change,
           comp_labels) {
    new_composition <- data.frame(matrix(nrow = 1, ncol = 0))
    new_composition[, main_part] <-
      composition[, main_part] + main_change
    if (is.null(at_expense_of)) {
      at_expense_of <- comp_labels[comp_labels != main_part]
    }
    if (length(at_expense_of) > 1) {
      for (part in at_expense_of) {
        new_composition[, part] <-
          (rowSums(composition[, c(at_expense_of, main_part)]) - new_composition[, main_part]) *
          (composition[, part] / (rowSums(composition[, at_expense_of])))
      }

    }
    else {
      new_composition[, at_expense_of] <-
        rowSums(composition[, c(at_expense_of, main_part)]) - new_composition[, main_part]

    }
    for (part in comp_labels) {
      if (part != main_part) {
        if (!(part %in% at_expense_of)) {
          new_composition[, part] <- composition[, part]
        }
      }

    }

    for (part in comp_labels){
      if (new_composition[, part] <0 ){
        stop("Change resulted in parts of composition less than zero. Is the change specified on the correct scale?")
      }
    }
    new_composition <- new_composition[, comp_labels]
    return(new_composition)
  }
