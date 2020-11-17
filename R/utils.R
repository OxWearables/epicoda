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
#' @param nrow Number of rows of transformation matrix to be created (= number of parts in composition)
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
#' @param part_1 part which should be moved to front.
#' @return \code{comp_labels} in new order.
alter_order_comp_labels <- function(comp_labels, part_1){
  if (part_1 %in% comp_labels){
    comp_red <- comp_labels[comp_labels != part_1]
    comp_labels <- c(part_1, comp_red)
    return(comp_labels)
  }
  else{
    stop("Specified part_1 does not appear in comp_labels.")
  }
}


#' Process units argument
#'
#' @param units What should the units of the compositional variables be in any output? Currently available are "unitless" (where working in terms of proportions), "hr/day", "hr/wk", "min/day", "min/wk" and "specified", in which case the \code{specified_units} argument should be set. Note that this doesn't specify the input units, as this is not relevant for any function.
#' @param specified_units If units are being specified via the composition sum, this is where it is done. It should be a vector where the first argument is a string describing the units, and the second argument is the expected sum of a composition e.g. \code{c("hr/day", 24)}
process_units <- function(units, specified_units){
  if (!(units %in% c("hr/wk", "hr/day", "min/wk", "min/day", "unitless", "specified"))){
    stop("Unrecognised value for units. units should be \"hr/wk\", \"hr/day\", \"min/wk\", \"min/day\", \"unitless\" or \"specified\" with the specified_units argument also given.")
  }
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
    if (!(length(specified_units) ==2)){
      stop("specified_units are not in the correct form.")
    }
    if (!is.character(specified_units[1])){
      stop("The first argument of specified_units must be a string describing the units.")
    }
    units <- specified_units[1]
    comp_sum <- as.numeric(specified_units[2])

    if (!is.numeric(comp_sum)){
      stop("The second argument of specified_units must be a number specifying the sum of a composition.")
    }
  }

  return(c(units, comp_sum))
}


#' Process zeroes argument
#'
#' @param data Dataset to have zeroes imputed for.
#' @param comp_labels The labels of the compositional columns.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This is needed when \code{rounded_zeroes} is \code{TRUE}. It should be the
#' minimum measurable value in the compositional columns of data, and should be on the same scale as the (input) compositional columns. If it is
#' not set, the minimum measured value greater than zero is used, which is appropriate for many practical purposes.
process_zeroes <- function(data, comp_labels, rounded_zeroes, det_limit = NULL){
  data$row_labels <- 1:nrow(data)
  comp_data <- data[, c(comp_labels, "row_labels")]

  if (any(comp_data ==0, na.rm = TRUE) & rounded_zeroes){
    if (is.null(det_limit)){
    message("det_limit was imputed as the minimum value observed in the compositional
         columns of data; \n if this is an unrealistic det_limit, the results may be unreliable.")
    det_limit <- min(comp_data[!apply(comp_data[, comp_labels], 1, function(r) any(r == 0)) , comp_labels], na.rm = TRUE)
  }
    message(paste("Note that zeroes were imputed with detection limit \n", signif(det_limit, 3), " (on the unitless scale) using zCompositions::lrEM"))
    comp_data_nonans <- comp_data[stats::complete.cases(comp_data[, comp_labels]), ]
    comp_data_nans <- comp_data[!stats::complete.cases(comp_data[, comp_labels]), ]

    utils::capture.output(comp_data_nonans[, comp_labels] <- zCompositions::lrEM(comp_data_nonans[, comp_labels], label = 0, dl = matrix(data = rep(det_limit,length(comp_data_nonans[,1])*ncol(comp_data_nonans[, comp_labels])),
                                                                                      nrow = length(comp_data_nonans[,1]),
                                                                                      byrow = T), max.iter = 50))
    comp_data <- rbind(comp_data_nonans, comp_data_nans)
  }

  if (!rounded_zeroes){
   message("Note that any zero values were dropped.")
    for (part in comp_labels){
      comp_data <- comp_data[comp_data[,part] != 0, ]
    }
  }

  non_comp_cols <- colnames(data)[!(colnames(data) %in% comp_labels)]

  if (length(non_comp_cols) > 1.5){
    data <- merge(data[,non_comp_cols], comp_data, by = "row_labels")
  }

  if (length(non_comp_cols) <= 1.5) {
    data <- comp_data
  }

  data <- data[, colnames(data)[colnames(data) != "row_labels"]]
  return(data)
}



#' Process model argument
#'
#' @param model This is the model which needs type extracting.
process_model_type <- function(model){
  # We assign some internal parameters
  type <- "unassigned"
  if (class(model)[1] == "lm") {
    type <- "linear"
  }
  if ((class(model)[1] == "glm") &&
      (stats::family(model)[[1]] == "binomial")) {
    type <- "logistic"
  }
  if ((class(model)[1]  == "coxph")) {
    type <- "cox"
  }
  if (type == "unassigned") {
    stop("model is not a recognised type of model.")
  }

  return(type)
}




#' Process axis labels
#'
#' @param label This is the axis label that already exists. To suppress the label entirely, set \code{label == "suppressed"}.
#' @inheritParams predict_fit_and_ci
#' @param type Output from \code{process_model_type}
process_axis_label <- function(label, terms, type){
  if ((is.null(label)) & (terms) & (type == "linear")) {
    label <- "Model-predicted difference in outcome"
  }
  if ((is.null(label)) & !(terms) & (type == "logistic")) {
    label <- "Model-predicted probability"
  }
  if ((is.null(label)) & (terms) & (type == "logistic")) {
    label <- "Model-predicted OR"
  }
  if ((is.null(label)) &  (type == "cox")) {
    label <- "Model-predicted HR"
  }
  if ((is.null(label)) & (terms == FALSE)) {
    label <- "Model-predicted outcome"
  }
  if (label == "suppressed" | label == "Suppressed"){
    label <- NULL
  }
  return(label)
}


#' Normalise input data
#'
#' @param data Input data.
#' @inheritParams process_zeroes
normalise_comp <- function(data, comp_labels){
  output <- data
  output[, comp_labels] <- output[, comp_labels]/apply(output[, comp_labels], 1, sum)
  return(output)
}

#' Return output data on correct scale
#'
#' @param data Data including a normalised set of compositional columns.
#' @param comp_sum The sum the compositional columns should have.
#' @inheritParams process_zeroes
rescale_comp <- function(data, comp_labels, comp_sum){
  output <- data
  if (isFALSE(all.equal(apply(output[, comp_labels], 1, sum), rep(1, by = nrow(output))))){
    warning("Rescaling was applied even though not all rows summed to 1. This may be because this function is being applied at the wrong point. It may also occur if there are missing values in the compositional columns. Repeat after removing any missing or non-numeric compositional values.")
  }
  output[, comp_labels] <- output[, comp_labels]*comp_sum
  return(output)
}


#' Determine if range of vector is effectively zero.
#'
#' @param x Data for which to check
#' @param tol Tolerance within which considered equal (on median-scaled scale).
zero_range <- function(x, tol = 0.01) {
  if (length(x) == 1) return(TRUE)
  y <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))/ stats::median(x, na.rm = TRUE)
  isTRUE(y<tol)
}


#' Rescale det_limit
#'
#' Rescale a det_limit given on the same scale as the compositional columns.
#' @param data Input data.
#' @inheritParams process_zeroes
rescale_det_limit <- function(data, comp_labels, det_limit){
  if (!(is.null(det_limit))){
    vec_of_sums <- apply(data[, comp_labels], 1, sum)
    if (!zero_range(vec_of_sums)){
      message(paste("The range of sums of columns is ", max(vec_of_sums, na.rm = TRUE) - min(vec_of_sums, na.rm = TRUE), ".\n The median sum will be used to rescale the det_limit. Does this match the scale on which you specified the det_limit?"))
    }
    rescale_fac <- stats::median(vec_of_sums, na.rm = TRUE)
    det_limit_new <- det_limit/rescale_fac
  }
  if(is.null(det_limit)) {
    det_limit_new <- NULL
  }
  return(det_limit_new)
}


