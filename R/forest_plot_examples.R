#' Produce a forest plot of model predictions
#'
#' This function takes a named list of compositions, and plots model predictions for each composition. Please note that with the default \code{terms = TRUE} predictions are relative to at the (compositional) mean composition in the data used to create the model. Please note that if there is missing data in the original data (e.g. in covariates or outcomes) this may not be the same as the compositional mean in the original data.
#'
#' @param composition_list Named list of compositions. Note each composition should be stored as a data frame. For example, use the output of \code{change_composition}.
#' @param models_list If \code{model} is \code{NULL} (or not set), a named list of models for which to plot model predictions in the forest plot. All models should have the same type.
#' @param x_label x-axis label.
#' @param xllimit Minimum value for x-axis.
#' @param xulimit Maximum value for x-axis.
#' @param text_settings An optional argument to specify text formatting. It should be an \code{fpTxtGp} object (\code{forestplot} package).
#' @param plot_log If this is \code{TRUE}, the x-axis will be log-transformed.
#' @param boxsize Sets the size of boxes for estimates on the forest plot.
#' @param pred_name Name for column of predictions in plot.
#' @inheritParams predict_fit_and_ci
#' @inheritDotParams forestplot::forestplot
#' @return Forest plot showing model predictions.
#' @examples
#' # Example using a list of models
#'   # First we set up composition list
#' df <- as.data.frame(comp_mean(data = simdata,
#'                              comp_labels = c("vigorous", "moderate",
#'                              "light", "sedentary", "sleep"),
#'                              units = "hr/day"))
#' new_comp <- change_composition(composition = df, main_part = "moderate",
#'                              main_change = +0.5,
#'                              comp_labels = c("vigorous", "moderate",
#'                              "light", "sedentary", "sleep"))
#' new_comp2 <- change_composition(composition = df, main_part = "sedentary",
#'                              main_change = -3.5,
#'                              comp_labels = c("vigorous", "moderate",
#'                              "light", "sedentary", "sleep"))
#' list_for_plot <- list("Extra 0.5 hr/day moderate" = new_comp,
#'                       "3.5 hr/day less sedentary" = new_comp2)
#'
#'  # Then calculate models
#' lm_BMI_unadjusted <- comp_model(type = "linear", outcome = "BMI",
#'                                 data = simdata,
#'                                 comp_labels = c("vigorous", "moderate",
#'                                  "light", "sedentary", "sleep"))
#' lm_BMI_age_group_only <- comp_model(type = "linear", outcome = "BMI",
#'                                     covariates = c("agegroup"), data = simdata,
#'                                     comp_labels = c("vigorous", "moderate",
#'                                      "light", "sedentary", "sleep"))
#'
#'   # Finally, plot
#' forest_plot_comp(composition_list = list_for_plot,
#'                  models_list = list("Unadjusted" = lm_BMI_unadjusted,
#'                  "Age-adjusted" = lm_BMI_age_group_only),
#'                  comp_labels = c("vigorous", "moderate",
#'                  "light", "sedentary", "sleep"))
#' @export
forest_plot_comp <-
  function(composition_list,
           model = NULL ,
           models_list = NULL,
           comp_labels,
           x_label = NULL,
           xllimit = NULL,
           xulimit = NULL,
           plot_log = FALSE,
           text_settings = NULL,
           pred_name = NULL,
           boxsize = 0.05,
           terms = TRUE,
           units = "unitless",
           specified_units = NULL,
           fixed_values = NULL,
           part_1 = NULL,
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
      for (i in 2:length(models_list)){
        if (process_model_type(models_list[[i]]) != type){
          stop("Not all models are of the same type.")
        }
      }
    }

    x_label <- process_axis_label(label = x_label, type = type, terms = terms)


    if (terms){
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
        new_data = df,
        fixed_values = fixed_values,
        part_1 = part_1,
        comp_labels = comp_labels,
        units = units,
        specified_units = specified_units,
        terms = terms
      )



      if (is.null(xllimit)){
        xllimit <- min(dNew$lower_CI)
      }
      if (is.null(xulimit)){
        xulimit <- max(dNew$upper_CI)
      }

      if (terms){
        xllimit <- min(xllimit, vline_loc)
        xulimit <- max(xulimit, vline_loc)
      }

      if (((xulimit - xllimit)/0.05) <= 10) {
        req_seq <- seq(floor((xllimit- 0.05)/0.05)*0.05, ceiling((xulimit + 0.05)/0.05)*0.05, by = 0.05)
        req_seq_labs <- formatC(req_seq, format = "f", digits= 2)
      }
      if ((((xulimit - xllimit)/0.05) > 10) & (((xulimit - xllimit)/0.05) <= 20)){
        req_seq <- seq(floor((xllimit- 0.1)/0.1)*0.1, ceiling((xulimit + 0.1)/0.1)*0.1, by = 0.1)
        req_seq_labs <- formatC(req_seq, format = "f", digits= 1)
      }
      if ((((xulimit - xllimit)/0.05) > 20)){
        req_seq <- seq(floor((xllimit- 0.5)/0.5)*0.5, ceiling((xulimit + 0.5)/0.5)*0.5, by = 0.5)
        req_seq_labs <- formatC(req_seq, format = "f", digits= 1)
      }

      attr(req_seq, "labels") <- req_seq_labs


      data_frame_for_forest_plot <- dNew[, c("fit", "lower_CI", "upper_CI")]
      colnames(data_frame_for_forest_plot) <- c("coef", "low", "high")

      data_frame_for_forest_plot <- rbind(data.frame("coef" = c(NA, vline_loc), "low" = c(NA, vline_loc), "high" = c(NA, vline_loc)), data_frame_for_forest_plot)




      text_col <- paste(format(round(data_frame_for_forest_plot$coef , digits = 2), nnsmall = 2), " (", format(round(data_frame_for_forest_plot$low, digits = 2), nsmall = 2), ", ", format(round(data_frame_for_forest_plot$high, digits = 2), nsmall = 2), ")", sep = "")
      if (terms){
        tabletext <- cbind(c(NA, "REFERENCE: At compositional mean", col_of_names), c(pred_name, vline_loc, text_col[3:nrow(data_frame_for_forest_plot)]))
      }
      if (!(terms)){
        tabletext <- cbind(c(NA, NA, col_of_names), c(pred_name, vline_loc, text_col[3:nrow(data_frame_for_forest_plot)]))
      }

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
        txt_gp = text_settings,
        boxsize = boxsize,
        col = forestplot::fpColors(zero = "black"),
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
        new_data = df[i,],
        fixed_values = fixed_values,
        part_1 = part_1,
        comp_labels = comp_labels,
        units = units,
        specified_units = specified_units,
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


    if (terms){
       xllimit <- min(xllimit, vline_loc)
       xulimit <- max(xulimit, vline_loc)
    }

    if (((xulimit - xllimit)/0.05) <= 10) {
      req_seq <- seq(floor((xllimit- 0.05)/0.05)*0.05, ceiling((xulimit + 0.05)/0.05)*0.05, by = 0.05)
      req_seq_labs <- formatC(req_seq, format = "f", digits= 2)
       }
    if ((((xulimit - xllimit)/0.05) > 10) & (((xulimit - xllimit)/0.05) <= 20)){
      req_seq <- seq(floor((xllimit- 0.1)/0.1)*0.1, ceiling((xulimit + 0.1)/0.1)*0.1, by = 0.1)
      req_seq_labs <- formatC(req_seq, format = "f", digits= 1)
     }
    if ((((xulimit - xllimit)/0.05) > 20)){
      req_seq <- seq(floor((xllimit- 0.5)/0.5)*0.5, ceiling((xulimit + 0.5)/0.5)*0.5, by = 0.5)
      req_seq_labs <- formatC(req_seq, format = "f", digits= 1)
       }

    attr(req_seq, "labels") <- req_seq_labs

    data_frame_for_forest_plot <- rbind( rep(vline_loc, by = ncol(data_frame_for_forest_plot)), data_frame_for_forest_plot)

    if (terms){
       tabletext <- cbind(c( "REFERENCE: At compositional mean", col_of_names))
    }
    if (!(terms)){
      tabletext <- cbind(c(NA, col_of_names))
    }


    col_vec <- grDevices::hcl.colors(n = length(models_list), palette = "dark2")
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
      col = forestplot::fpColors(box = col_vec, line = col_vec, zero = "black"),
    boxsize = boxsize,
      ...
    )
    }

    return(fp)
  }






