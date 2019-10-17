#' Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Geometric mean
gm <- function(vector) {
  geometric_mean <- exp(mean(log(vector)))
  return(geometric_mean)
}
