#' Mode
#'
#' @param x Vector to take the mode of.
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


#' Geometric mean
#'
#' @param vector Vector to take the geometric mean of.
gm <- function(vector) {
  geometric_mean <- exp(mean(log(vector)))
  return(geometric_mean)
}

#' Create transformation matrix from clr to ilr pivot coordinates
#'
#' @param nrow Number of rows of transformation matrix to be created (= number of components in composition)
create_transformation_matrix <- function(nrow) {
  tm <- matrix(nrow = nrow, ncol = 0)
  for (i in 1:(nrow - 1)){
    newcol <- c()
    if (i >1){
      for (j in 1:(i - 1)){
        newcol <- c(newcol, 0)
      }
    }

    newcol <- c(newcol, sqrt((nrow - i) / (nrow - i + 1)))

    for (j in (i + 1):nrow) {
      newcol <- c(newcol,-1/sqrt((nrow - i) *(nrow - i + 1)))
    }
    tm <- cbind(tm, newcol)
  }
  tm <- as.matrix(tm)
  return(tm)
}


#' Alter order of compositional column labels.
#'
#' @param comp_labels List of compositional column labels.
#' @param component_1 Component which should be moved to front.
#' @return \code{comp_labels} in new order.
alter_order_comp_labels <- function(comp_labels, component_1){
  if (component_1 %in% comp_labels){
    comp_red <- comp_labels[comp_labels != component_1]
    comp_labels <- c(component_1, comp_red)
    return(comp_labels)
  }
  else{
    stop("Specified component_1 does not appear in comp_labels.")
  }
}


#' Process units argument
#'
#' @param units List of compositional column labels.
#' @param specified_units Component which should be moved to front.
process_units <- function(units, specified_units){
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
    units <- specified_units[1]
    comp_sum <- specified_units[2]
  }
  if (!(units %in% c("hr/wk", "hr/day", "min/wk", "min/day", "unitless", "specified"))){
    stop("Unrecognised value for units. units should be \"hr/wk\", \"hr/day\", \"min/wk\", \"min/day\", \"unitless\" or \"specified\" with the specified_units argument also given.")
  }
}


#' Process zeroes argument
#'
#' @param data Dataset to have zeroes imputed for.
#' @param comp_labels The labels of the compositional columns.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data.
process_zeroes <- function(data, comp_labels, rounded_zeroes, det_limit){
  if (rounded_zeroes & is.null(det_limit)){
    stop("det_limit must be set for zeroes to be imputed. It should be the minimum measurable value in the compositional
         columns of data.")
  }
  data$row_labels <- 1:nrow(data)
  comp_data <- data[, c(comp_labels, "row_labels")]
  if (any(comp_data ==0) & rounded_zeroes){
    message(paste("Note that zeroes were imputed with detection limit \n", det_limit, "using zCompositions::lrEM"))
    comp_data[, comp_labels] <- suppressMessages(zCompositions::lrEM(comp_data[, comp_labels], label = 0, dl = matrix(data = rep(det_limit,length(comp_data[,1])*ncol(comp_data[, comp_labels])),
                                                                                      nrow = length(comp_data[,1]),
                                                                                      byrow = T), max.iter = 50))
  }
  else{
   message("Note that before calculating the compositional mean zero values were dropped.")
    for (activity in comp_labels){
      comp_data <- comp_data[comp_data[,activity] != 0, ]
    }
  }
  non_comp_cols <- colnames(data)[!(colnames(data) %in% comp_labels)]
  data <- merge(data[,non_comp_cols], comp_data, by = "row_labels")
  data <- data[, colnames(data)[colnames(data) != "row_labels"]]
  return(data)
}


