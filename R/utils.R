#' Mode
#'
#' @param x Vector to take the mode of.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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


#' Create columns headings for after the transformation.
#'
#' @param comp_labels List of compositional column labels.
transf_labels <- function(comp_labels){
  l <- (length(comp_labels)-1)
  transf <- comp_labels[1:l]
  first_labels_transf <- paste0(1:l, "-", transf)
  transf_with_labels <- paste("ilr_", first_labels_transf, "_vs_remaining", sep = "")
  return(transf_with_labels)
}
