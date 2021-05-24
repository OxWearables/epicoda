#' Extract dataset from model object
#'
#' Used by predict_fit_and_ci and plot_transfers.
#'
#' @inheritParams predict_fit_and_ci
#' @return Dataset used to create model with compositional columns on original scale.
#' @export
#' @examples
get_dataset_from_model <- function(model, comp_labels, transf_labels, type){
  ## We get dataset from model frame
  dataset <- stats::model.frame(model)

  ## We verify that the correct column names are present
  if (!(all(transf_labels  %in% colnames(dataset)[grepl("ilr", colnames(dataset))]))){
    stop("Specified comp_labels do not match those used to develop the model (e.g. different order?)")
  }
  if (!(all(colnames(dataset)[grepl("ilr", colnames(dataset))] %in% transf_labels))){
    stop("Specified comp_labels do not match those used to develop the model (e.g. missing labels?)")
  }

  ## We add the compositional columns on the untransformed scale
  comp_cols <- ilr_trans_inv(dataset[, transf_labels])
  colnames(comp_cols) <- comp_labels
  dataset <- cbind(dataset, comp_cols)

  ## We remove the "strata" prefix from any Cox strata variables
  if (type == "cox"){
    strata_list <- colnames(dataset)[grepl("strata\\(",colnames(dataset) )]
    for (name in strata_list){
      plain <- gsub("strata\\(", "", name)
      plain <- gsub("\\)", "", plain)
      dataset[, plain] <- dataset[, name]
  }
  }

  ## We return the dataset without the compositional columns
  dataset_ready <- dataset[,!(colnames(dataset) %in% c(transf_labels, "survival_object"))]

  return(dataset_ready)
}


#' Extract compositional mean from model object
#'
#' Used by predict_fit_and_ci and plot_transfers.
#'
#' @inheritParams predict_fit_and_ci
#' @return Dataset used to create model with compositional columns on original scale.
#' @export
#' @examples
get_cm_from_model <- function(model, comp_labels, transf_labels){
  mm <- stats::model.frame(model)[, transf_labels]
  cm_transf_df <- apply(mm, 2, mean)
  cm_transf_df <- as.data.frame(t(cm_transf_df))
  cm <- ilr_trans_inv(cm_transf_df)
  colnames(cm) <- comp_labels
return(list("cm"= cm, "cm_transf_df" = cm_transf_df))
}
