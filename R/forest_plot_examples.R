#' Produce a forest plot indicating model prediction at given compositions
#'
#' This function takes a named list of compositions, and plots a model prediction at each composition.
#'
#' @param composition_list Named list of compositions. Note each composition should be stored as a data frame. For example, use the output of \code{change_composition}.
#' @param x_label Label for x axis in plot.
#' @param xllimit Minimum value for x axis.
#' @param xulimit Maximum value for x axis.
#' @param text_settings An optional argument which should be an \code{fpTxtGp} object as specified in the \code{forestplot} package.
#' @param plot_log If this is \code{TRUE}, the x-axis will be log-transformed.
#' @inheritParams predict_fit_and_ci
#' @inheritDotParams forestplot::forestplot
#' @return Forest plot illustrating prediction of the model at given compositions.
#' @export
forest_plot_comp <-
  function(composition_list,
           model,
           dataset,
           fixed_values = NULL,
           transformation_type = NULL,
           comparison_part = NULL,
           part_1 = NULL,
           comp_labels,
           units = "unitless",
           specified_units = NULL,
           rounded_zeroes = TRUE,
           det_limit = NULL,
           terms = TRUE,
           x_label = NULL,
           xllimit = NULL,
           xulimit = NULL,
           plot_log = FALSE,
           text_settings = NULL,
           ...) {

    if (!is.list(composition_list)) {
      stop('`composition_list` should be a list.')
    }
    # We normalise comp



    if (is.null(text_settings)){
      text_settings <- forestplot::fpTxtGp(
        label = grid::gpar(
          fontfamily = "sans",
          cex = 1,
          fontface = 2
        ),
        xlab = grid::gpar(
          fontfamily = "sans",
          cex = 1,
          fontface = 2
        ),
        ticks = grid::gpar(cex = 0.75, fontface = 2)
      )
    }

    type <- process_model_type(model)
    x_label <- process_axis_label(label = x_label, type = type, terms = terms)

    if ( terms ){
      if (type == "cox" | type == "logistic") {
        vline_loc <- 1
      }
     if (type == "linear"){
        vline_loc <- 0
     }
    }
    if (!terms){
      vline_loc <- NA
    }


    col_of_names <- names(composition_list)
    df <- data.table::rbindlist(composition_list, use.names = TRUE)

    dNew <- predict_fit_and_ci(
      model = model,
      dataset = dataset,
      new_data = df,
      fixed_values = fixed_values,
      transformation_type = transformation_type,
      comparison_part = comparison_part,
      part_1 = part_1,
      comp_labels = comp_labels,
      units = units,
      specified_units = specified_units,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit,
      terms = terms
    )


    if (is.null(xllimit)){
      xllimit <- min(dNew$lower_CI)
    }

    if (is.null(xulimit)){
      xulimit <- max(dNew$upper_CI)
      xllimit <- round(xllimit - ((xulimit- xllimit)/5), digits = 2)
      xulimit <- round(xulimit+ ((xulimit- xllimit)/5), digits = 2)
    }


    data_frame_for_forest_plot <- dNew[, c("fit", "lower_CI", "upper_CI")]
    colnames(data_frame_for_forest_plot) <- c("coef", "low", "high")

    data_frame_for_forest_plot <- rbind(data.frame("coef" = c(NA, vline_loc), "low" = c(NA, vline_loc), "high" = c(NA, vline_loc)), data_frame_for_forest_plot)




    CI <- paste(round(data_frame_for_forest_plot$low, digits = 2), "-", round(data_frame_for_forest_plot$high, digits = 2))
     tabletext <- cbind(c(NA, "REFERENCE: AT compositional mean", col_of_names), c("Model prediction", vline_loc, round(data_frame_for_forest_plot$coef[3:nrow(data_frame_for_forest_plot)], digits = 2)), c("95% CI", "NA", CI[3:nrow(data_frame_for_forest_plot)]))


     fp <- forestplot::forestplot(
      tabletext,
      graph.pos = 2,
      data_frame_for_forest_plot,
      xlog = plot_log,
      clr.line = "black",
      clip = c(xllimit, xulimit),
      xticks = seq(xllimit, xulimit, by = round(((xulimit- xllimit)/5), digits = 2)),
      xlab = x_label,
      zero = vline_loc,
      clr.line = "black",
      boxsize = 0.05,
      txt_gp = text_settings,
      ...
    )
    return(fp)
  }






