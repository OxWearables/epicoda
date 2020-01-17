#' Produce a forest plot indicating model prediction at given compositions
#'
#' This function takes a named list of compositions, and plots a model prediction at each composition.
#'
#' @param composition_list Named list of compositions. Note each composition should be stored as a data frame. For example, use the output of \code{change_composition}.
#' @param models_list If \code{model} is \code{NULL} (or not set), a named list of models for which to plot model predictions in the forest plot. Note all models should have the same type or the results will be meaningless.
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
           model = NULL ,
           models_list = NULL,
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
           pred_name = NULL,
           ...) {

    if (!is.list(composition_list)) {
      stop('`composition_list` should be a list.')
    }

    if (is.null(pred_name)){
      pred_name <- "Model prediction (95% CI)"
    }

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
    if (!is.null(model)){
      type <- process_model_type(model)
    }
    if (is.null(model)){
      type <- process_model_type(models_list[[1]])
    }

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

    if (!is.null(model)){
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
      }

      req_seq <- seq(round((xllimit- 0.05)/0.05, digits = 1)*0.05, round((xulimit + 0.05)/0.05, digits = 1)*0.05, by = 0.05)
      req_seq_labs <- formatC(req_seq, format = "f", digits= 2)
      attr(req_seq, "labels") <- req_seq_labs


      data_frame_for_forest_plot <- dNew[, c("fit", "lower_CI", "upper_CI")]
      colnames(data_frame_for_forest_plot) <- c("coef", "low", "high")

      data_frame_for_forest_plot <- rbind(data.frame("coef" = c(NA, vline_loc), "low" = c(NA, vline_loc), "high" = c(NA, vline_loc)), data_frame_for_forest_plot)




      text_col <- paste(format(round(data_frame_for_forest_plot$coef , digits = 2), nnsmall = 2), " (", format(round(data_frame_for_forest_plot$low, digits = 2), nsmall = 2), ", ", format(round(data_frame_for_forest_plot$high, digits = 2), nsmall = 2), ")", sep = "")
      tabletext <- cbind(c(NA, "REFERENCE: At compositional mean", col_of_names), c(pred_name, vline_loc, text_col[3:nrow(data_frame_for_forest_plot)]))


      fp <- forestplot::forestplot(
        tabletext,
        graph.pos = 2,
        data_frame_for_forest_plot,
        xlog = plot_log,
        clr.line = "black",
        clip = c(xllimit, xulimit),
        xticks = req_seq,
        xlab = x_label,
        zero = vline_loc,
        clr.line = "black",
        txt_gp = text_settings,
        ...
      )
    }

    if (is.null(model)){
      if (is.null(models_list)){
        return( " Either model or models_list must be set.")
      }
       data_frame_for_forest_plot <- data.frame("coef" = c(), "low" = c(), "high" = c())
    for (i in 1:nrow(df)){
      coef <- c()
      low <- c()
      high <- c()
      for (j in 1:length(models_list)){
        dPred <- predict_fit_and_ci(
        model = models_list[[j]],
        dataset = dataset,
        new_data = df[i,],
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
      coef <- cbind(coef, dPred$fit)
      low <- cbind(low, dPred$lower_CI)
      high <- cbind(high, dPred$upper_CI)

      }
      data_frame_for_forest_plot <- rbind(data_frame_for_forest_plot, data.frame("coef" = coef, "low" = low, "high" = high))

      }



    if (is.null(xllimit)){
      xllimit <- min(data_frame_for_forest_plot)
    }

    if (is.null(xulimit)){
      xulimit <- max(data_frame_for_forest_plot)
    }
    req_seq <- seq(round((xllimit- 0.05)/0.05, digits = 1)*0.05, round((xulimit + 0.05)/0.05, digits = 1)*0.05, by = 0.05)
    req_seq_labs <- formatC(req_seq, format = "f", digits= 2)
    attr(req_seq, "labels") <- req_seq_labs

    data_frame_for_forest_plot <- rbind( rep(vline_loc, by = ncol(data_frame_for_forest_plot)), data_frame_for_forest_plot)



 #   text_col <- paste(format(round(data_frame_for_forest_plot$coef , digits = 2), nnsmall = 2), " (", format(round(data_frame_for_forest_plot$low, digits = 2), nsmall = 2), ", ", format(round(data_frame_for_forest_plot$high, digits = 2), nsmall = 2), ")", sep = "")
  tabletext <- cbind(c( "REFERENCE: At compositional mean", col_of_names))#, c(pred_name, vline_loc, text_col[3:nrow(data_frame_for_forest_plot)]))
    col_vec <- hcl.colors(n = length(models_list), palette = "dark2")
     fp <- forestplot::forestplot(
    tabletext,
     graph.pos = 2,
      mean = data_frame_for_forest_plot[, colnames(data_frame_for_forest_plot)[grepl("coef", colnames(data_frame_for_forest_plot))]],
      lower = data_frame_for_forest_plot[, colnames(data_frame_for_forest_plot)[grepl("low", colnames(data_frame_for_forest_plot))]],
      upper = data_frame_for_forest_plot[, colnames(data_frame_for_forest_plot)[grepl("high", colnames(data_frame_for_forest_plot))]],
      xlog = plot_log,
      clip = c(xllimit -0.05, xulimit + 0.05),
      xticks = req_seq,
      xlab = x_label,
      zero = vline_loc,
      txt_gp = text_settings,
      legend = names(models_list),
      col = forestplot::fpColors(box = col_vec, line = col_vec),
      ...
    )
    }

    return(fp)
  }






