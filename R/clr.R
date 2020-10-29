#' Clr transformation
#'
#' This performs centred log-ratio transformation on the compositional columns of a data frame.
#'
#' @param data Compositional columns of dataframe.
#' @return \code{data} with clr-transformed compositional columns.
#' @examples clr_trans(data = simdata[, c("vigorous", "moderate", "light", "sedentary", "sleep")],
#' comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
#' comparison_part = "sleep")
#'
#' @export
clr_trans <- function(data) {
  gms <- apply(data,1,gm)
  y <- log(data / gms)
  colnames(y) <- paste0("clr_",colnames(y))
  return(y)
}

#' Clr inversion
#'
#' This inverts a clr transformation. Currently it only does so onto the unitless scale. If needed, it can be extended to take a units output argument.
#'
#' @param data Clr-transformed columns.
#' @return \code{data} with clr transformation inverted.
#' @export
clr_trans_inv <- function(data){
  unwrapped <- exp(data)
  total <- rowSums(unwrapped)
  y <- unwrapped/total
  colnames(y) <- sub("clr_", "", colnames(y))
  return(y)
}
