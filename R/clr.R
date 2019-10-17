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
  print(head(total))
  y <- 168*unwrapped/total
  return(y)
}
