#' ilr_trans: Performs ilr transformations (pivot coordinates)
#'
#' Takes activity columns and returns them after ilr transformation.
#'
#' This doesn't use compositions::ilr to insulate it from changes in the choice of
#' basis used. As pivot coordinates are currently standard in physical activity epidemiology this
#' is specified.
#'
#' @param data Dataset to have the transformation applied to.
#' @return ilr-transformed columns.
#' @examples # TBA
#' @export
ilr_trans <- function(data){
  dClr <- clr_trans(data)
  mClr <- as.matrix(dClr)
  transformation_matrix <- create_transformation_matrix(ncol(data))
  dTransformed <- data.frame(mClr %*% transformation_matrix)
  colnames(dTransformed) <- paste0("ilr_", 1:(ncol(data)-1))
  return(dTransformed)
}
