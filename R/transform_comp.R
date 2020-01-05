#' Transform compositional variables to ilr (pivot), alr or clr coordinates
#'
#' Takes a dataset with a set of labelled compositional columns and returns a dataset with
#' transformed variables added.
#'
#' @param data Dataset to have the transformation applied to.
#' @param comp_labels The labels of the compositional columns.
#' @param transformation_type The type of transformation desired. Should be \code{"ilr", "alr"} or \code{"clr"}.
#' @inheritParams process_zeroes
#' @param comparison_part Only needed for alr transformation. Should be an element of \code{comp_labels}. Name of part that all other parts will be compared to.
#' @param part_1 Only used if First part in ilr-pivot coordinate transformation. Passed to \code{alter_order_comp_labels} unless \code{NULL}.
#' @return \code{data} with transformed activity columns.
#' @examples # TBA
#' @export
transform_comp <- function(data, comp_labels, transformation_type = "ilr", rounded_zeroes = TRUE, det_limit = NULL, comparison_part = NULL, part_1 = NULL){
  if (transformation_type == "alr"){
    message("Alr transformed variables shouldn't be used for any applications which are sensitive to distance, such as PCA (applications which are not affine-equivariant).")
  }
  if (transformation_type == "alr" & is.null(comparison_part)){
    stop("comparison_part must be set for alr transformation.")
  }
  if (transformation_type == "clr"){
    message("Clr transformed variables are singular, so shouldn't be used for regression modelling.")
  }
  if (rounded_zeroes == TRUE & is.null(det_limit)){
    stop("If zeroes will be imputed in transform_comp, an argument must be passed to det_limit.")
  }
  dTransformationReady <- data
  dTransformationReady$row_labels_master <- 1:nrow(dTransformationReady)
  dTransformationReady <- process_zeroes(dTransformationReady, comp_labels, rounded_zeroes, det_limit)
  if (transformation_type == "ilr"){
    if (!is.null(part_1)){
      comp_labels <- alter_order_comp_labels(comp_labels, part_1)
    }
    transf_labels <- transf_labels(comp_labels, "ilr", part_1 = part_1)
    dTransformationReady <- dTransformationReady[, colnames(dTransformationReady)[!(colnames(dTransformationReady) %in% transf_labels )]]
    dTransformed <- ilr_trans(dTransformationReady[,comp_labels])
    colnames(dTransformed) <- transf_labels
    dTransformed$row_labels_master <- dTransformationReady$row_labels_master
  }

  if (transformation_type == "alr"){
    transf_labels <- transf_labels(comp_labels, "alr", comparison_part)
    dTransformationReady <- dTransformationReady[, colnames(dTransformationReady)[!(colnames(dTransformationReady) %in% transf_labels )]]
    dTransformed <- alr_trans(dTransformationReady[, comp_labels], comp_labels, comparison_part)
    colnames(dTransformed) <- transf_label
    dTransformed$row_labels_master <- dTransformationReady$row_labels_master
  }

  if (transformation_type == "clr"){
    transf_labels <- transf_labels(comp_labels, "clr")
    dTransformationReady <- dTransformationReady[, colnames(dTransformationReady)[!(colnames(dTransformationReady) %in% transf_labels )]]
    dTransformed <- clr_trans(dTransformationReady[, comp_labels])
    colnames(dTransformed) <- transf_labels
    dTransformed$row_labels_master <- dTransformationReady
  }
  dOut <- merge(dTransformationReady, dTransformed, by = "row_labels_master")
  dOut <- dOut[, colnames(dOut)[!(colnames(dOut) %in% c("row_labels_master", "row_labels"))]]

  return(dOut)
}



