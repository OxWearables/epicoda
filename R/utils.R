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

#' Create transformation matrix from clr to ilr pivot coordinates
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
