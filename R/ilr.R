#' ilr_trans: Ilr transformations
#'
#' Takes activity columns and returns them after ilr transformation.
#'
#' @param data Dataset to have the transformation applied to.
#' @return ilr-transformed columns.
#' @examples TBC
#' @export
ilr_trans <- function(data){
  dClr <- clr_trans(data)
  mClr <- as.matrix(dClr)
  transformation_matrix <- create_transformation_matrix(ncol(data))
  dTransformed <- data.frame(mClr %*% transformation_matrix)
  colnames(dTransformed) <- paste0("ilr_", 1:(ncol(data)-1))
  return(dTransformed)
}
