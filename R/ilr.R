#' @title
#' ilr_trans: Performs ilr transformations using pivot coordinates.
#'
#' @description
#' Takes compositional columns and returns them after ilr transformation using
#' pivot coordinates.
#'
#' @details
#' This doesn't use `ilr` to insulate it from changes in the choice of basis
#' used. As pivot coordinates are currently standard in physical behaviour
#' epidemiology this is specified.
#'
#' @param data Compositional columns to transform.

#' @return
#' ilr-transformed columns.

#' @noRd
ilr_trans <- function(data){
  dClr <- clr_trans(data)
  mClr <- as.matrix(dClr)
  transformation_matrix <- create_transformation_matrix(ncol(data))
  dTransformed <- data.frame(mClr %*% transformation_matrix)
  colnames(dTransformed) <- paste0("ilr_", seq_len(ncol(data) - 1))
  return(dTransformed)
}


#' @title
#' ilr_trans_inv: Inverts ilr transformation (pivot coordinates)
#'
#' @description
#' Takes ilr-transformed columns and inverts the ilr transformation.
#'
#' @param data Dataset to have the transformation reversed.
#' 
#' @return
#' Compositional columns.

#' @noRd
ilr_trans_inv <- function(data){
  transformation_matrix <- create_transformation_matrix(ncol(data) + 1)
  transformation_matrix_t <- t(transformation_matrix)

  matrixA <- rbind(transformation_matrix_t, rep(1, by = ncol(transformation_matrix_t)))
  aug_data <- cbind(data, rep(0, by = nrow(data)))

  Ainv <- solve(matrixA)

  data_ready <- t(as.matrix(aug_data))
  dUntransformed <- data.frame(t(Ainv %*% data_ready))
  colnames(dUntransformed) <- paste0("clr_comp_", seq_len(ncol(dUntransformed) - 1))
  dReversed <- clr_trans_inv(dUntransformed)

  return(dReversed)
}
