#' Clr transformation
#' @export
clr_trans <- function(data) {
  gms <- apply(data,1,gm)
  y <- log(data / gms)
  colnames(y) <- paste0("clr_",colnames(y))
  return(y)
}

#' Clr inversion
#'
#' This inverts a clr transformation.
#' @export
clr_trans_inv <- function(data){
  unwrapped <- exp(data)
  total <- rowSums(unwrapped)
  y <- unwrapped/total
  colnames(y) <- sub("clr_", "", colnames(y))
  return(y)
}
