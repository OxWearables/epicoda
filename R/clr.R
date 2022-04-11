#' @title
#' Clr transformation
#'
#' @description
#' This performs centred log-ratio transformation on the compositional columns
#' of a data frame.
#'
#' @param data Compositional columns of dataframe.
#' 
#' @return
#' `data` with clr-transformed compositional columns.
#' 
#' @examples
#' clr_trans(data = simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")])

#' @noRd
clr_trans <- function(data) {
  gms <- apply(data,1,gm)
  y <- log(data / gms)
  colnames(y) <- paste0("clr_",colnames(y))
  return(y)
}

#' @title
#' Clr inversion
#'
#' @description
#' This inverts a clr transformation. Currently it only does so onto the
#' unitless scale. If needed, it can be extended to take a units output
#' argument.
#'
#' @param data Clr-transformed columns.
#' 
#' @return
#' `data` with clr transformation inverted.

#' @noRd
clr_trans_inv <- function(data){
  unwrapped <- exp(data)
  total <- rowSums(unwrapped)
  y <- unwrapped/total
  colnames(y) <- sub("clr_", "", colnames(y))
  return(y)
}
