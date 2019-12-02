#' Transform compositional variables to ilr (pivot), alr or clr coordinates
#'
#' Takes a dataset with a set of labelled compositional columns and returns a dataset with
#' transformed variables added.
#'
#' @param data Dataset to have the transformation applied to.
#' @param comp_labels The labels of the compositional columns.
#' @param transformation_type The type of transformation desired. Should be \code{"ilr", "alr"} or \code{"clr"}.
#' @param rounded_zeroes Are zeroes rounded zeroes?
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data.
#' @param comparison_component Only needed for alr transformation. Should be an element of \code{comp_labels}. Name of component that all other components will be compared to.
#' @param component_1 Only used if First component in ilr-pivot coordinate transformation. Passed to \code{alter_order_comp_labels} unless \code{NULL}.
#' @return \code{data} with transformed activity columns.
#' @examples # TBA
#' @export
transform_comp <- function(data, comp_labels, transformation_type = "ilr", rounded_zeroes = TRUE, det_limit = NULL, comparison_component = NULL, component_1 = NULL){
  if (transformation_type == "alr"){
    message("Alr transformed variables shouldn't be used for any applications which are sensitive to distance, such as PCA (applications which are not affine-equivariant).")
  }
  if (transformation_type == "alr" & is.null(comparison_component)){
    stop("comparison_component must be set for alr transformation.")
  }
  if (transformation_type == "clr"){
    message("Clr transformed variables are singular, so shouldn't be used for regression modelling.")
  }

  dTransformationReady <- data
  dTransformationReady$row_labels_master <- 1:nrow(dTransformationReady)
  dTransformationReady <- process_zeroes(dTransformationReady, comp_labels, rounded_zeroes, det_limit)

  if (transformation_type == "ilr"){
    if (!is.null(component_1)){
      comp_labels <- alter_order_comp_labels(comp_labels, component_1)
    }
    dTransformed <- ilr_trans(dTransformationReady)
    transf_labels <- transf_labels(comp_labels, "ilr", component_1 = component_1)
    colnames(dTransformed) <- transf_labels
  }

  if (transformation_type == "alr"){
    dTransformed <- alr_trans(dTransformationReady, comp_labels, comparison_component)
    colnames(dTransformed) <- transf_labels(comp_labels, "alr", comparison_component)
  }

  if (transformation_type == "clr"){
    dTransformed <- clr_trans(dTransformationReady)
    colnames(dTransformed) <- transf_labels(comp_labels, "clr")
  }
  dOut <- merge(dTransformationReady, dTransformed, by = "row_labels_master")
  dOut <- dOut[, colnames(dOut)[!(colnames(dOut) %in% c("row_labels_master", "row_labels"))]]

  return(dOut)
}



