#' @title
#' Create columns headings for after the transformation.
#'
#' @inheritParams transform_comp

#' @noRd
transf_labels <- function(
  comp_labels,
  transformation_type,
  comparison_part = NULL,
  part_1 = NULL
) {
  
  transformation_type <- match.arg(transformation_type, c("ilr", "alr", "clr"))
  if (length(comp_labels) < 2) {
    stop('`comp_labels` should label at least two compositional parts of the data.')
  }
  
  if (transformation_type == "ilr") {
    
    # ILR
    if (!is.null(part_1)) {
      comp_labels <- alter_order_comp_labels(comp_labels, part_1)
    }
    l <- length(comp_labels) - 1
    transf <- comp_labels[seq_len(l)]
    transf_with_labels <- c()
    for (i in seq_len(l)) {
      if (i < l) {
        new_entry <- paste0("ilr_", i, "_", transf[i], "_vs_parts_", i + 1, "_to_", l + 1)
        transf_with_labels <- c(transf_with_labels, new_entry)
      }
      if (i == l) {
        new_entry <- paste0("ilr_", i, "_", transf[i], "_vs_part_", l + 1)
        transf_with_labels <- c(transf_with_labels, new_entry )
      }
    }

  } else if (transformation_type == "alr") {
    
    # ALR
    if (is.null(comparison_part)) {
      stop("comparison_part must be specified for alr transformation. See alr_trans.")
    }
    comp_labels_without_cc <- comp_labels[comp_labels != comparison_part]
    transf_with_labels <- paste0("alr_",comp_labels_without_cc, "_", comparison_part)
    
  } else {
    
    # CLR
    transf_with_labels <- paste0("clr_", comp_labels)
    
  }
  
  return(transf_with_labels)
  
}


#' @title
#' Create sum from a vector
#'
#' @param vector Vector to be turned into a sum.
#' 
#' @return
#' Sum of elements of vector.

#' @noRd
vector_to_sum <- function(vector){
  sum_to_return <- paste(vector, collapse = "+")
  return(sum_to_return)
}
