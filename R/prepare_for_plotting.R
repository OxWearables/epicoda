#' Generates list of fixed_values based on mean/modal values in dataset
#'
#' If fixed values (for non-compositional variables) are not set, this will set them to modal/ mean values.
#'
#' @param data Data used for model development.
#' @inheritParams plot_transfers
#' @return dataframe with a single row of fixed_values.
generate_fixed_values <- function(data, comp_labels){
  fixed_values <- data.frame(matrix(ncol = 0, nrow = 1))
  others <- colnames(data)[!(colnames(data) %in% comp_labels)]
  for (colname in others){
    if (!(is.numeric(data[, colname]))){
      fixed_values[colname] <- (Mode(data[,colname]))[1]
    }
    if ((is.numeric(data[, colname]))) {
      fixed_values[colname] <- mean(data[, colname], na.rm = TRUE)
    }
  }
  return(fixed_values)
}




#' Varies part of interest.
#'
#' Produces variable going between percentiles of the part of interest in the data.
#'
#' @param part_of_interest The variable of interest.
#' @inheritParams plot_transfers
#' @return Vector of values going from \code{lower_quantile} to \code{upper_quantile} of the distribution of the varaible of interest.
vary_part_of_interest <- function(part_of_interest,
                                  lower_quantile = 0.05,
                                  upper_quantile = 0.95,
                                  granularity = 10000) {
  part_values <- seq(
    from = stats::quantile(part_of_interest, lower_quantile, na.rm = TRUE),
    to = stats::quantile(part_of_interest, upper_quantile, na.rm = TRUE),
    length.out = granularity
  )
  return(part_values)
}
#' make_new_data: Generates a new dataset varying in the dimension of interest.
#'
#' Generates a new dataset to feed into the plotting functions (\code{plot_transfers})
#'
#' @param dataset Dataset to use to inform range of data used (should be dataset model was developed on)
#' @inheritParams plot_transfers
make_new_data <- function(from_part,
                          to_part,
                          fixed_values,
                          dataset,
                          units = NULL,
                          specified_units = NULL,
                          comp_labels,
                          lower_quantile = 0.05,
                          upper_quantile = 0.95,
                          granularity = 10000) {

  # PREP ====================================================
  dataset <- normalise_comp(dataset, comp_labels)
  fixed_values <- normalise_comp(fixed_values, comp_labels)
  new_data <- data.frame()[1:granularity,]

  # Process units
  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]

  # PROCESS NON COMPOSITIONAL VARIABLES  =====================
  for (label in colnames(dataset)[!(colnames(dataset) %in% comp_labels)]){
    new_data[, label] <- fixed_values[1, label]
  }

  # PROCESS COMPOSITIONAL VARIABLES ==========================
  # Vary the to part and the from part from the upper quantile to lower quantile
  vpi_tp <- vary_part_of_interest(dataset[, to_part],
                                  lower_quantile,
                                  upper_quantile, granularity = granularity)

  vpi_fp <- vary_part_of_interest(dataset[, from_part],
                                  lower_quantile,
                                  upper_quantile, granularity = granularity)

  # Find the ranges of these two
  min_fp <- min(vpi_fp)
  max_fp <- max(vpi_fp)

  min_tp <- min(vpi_tp)
  max_tp <- max(vpi_tp)

  # Use the values from the narrower range
  if ((max_tp - min_tp) >= (max_fp - min_fp)){ # if from_part is narrower assign this first
    tt <- 1
    for (label in comp_labels[comp_labels != to_part]){
      if (label == from_part) {
        new_data[, label] <- vpi_fp
      }
      if (label != from_part){
        new_data[, label] <- fixed_values[1, label]
      }
      tt <- tt - new_data[, label]
    }
    new_data[, to_part] <- tt
  }
  else {# if to_part is narrower assign this first
    tf <- 1
    for (label in comp_labels[comp_labels != from_part]){
      if (label == to_part) {
        new_data[, label] <- vpi_tp
      }
      if (label != to_part){
        new_data[, label] <- fixed_values[1, label]
      }
      tf <- tf - new_data[, label]
    }
    new_data[, from_part] <- tf
  }

  # REORDER AND PREPARE FOR OUTPUT======================
  new_data <- new_data[, c(comp_labels, colnames(dataset)[(colnames(dataset) %in% colnames(fixed_values)) & !(colnames(dataset) %in% comp_labels)])] # ensure same order as previously so backwards compatible

  # Rescale composition to be on the relevant scale
  new_data <- rescale_comp(new_data, comp_sum = comp_sum, comp_labels = comp_labels)
  print("Compositional variables not varied in the visualisation were fixed at:")
  for (variable in comp_labels[(comp_labels != from_part) & (comp_labels != to_part)]) {
    print(paste(variable, ":", signif(comp_sum*fixed_values[1, variable], 2), units))
  }

  return(new_data)
}
