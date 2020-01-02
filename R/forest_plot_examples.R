#' Produce a forest plot indicating model prediction at given compositions
#'
#' @param composition_list Named list of compositions. Note each composition should be stored as a data frame. For example, use the output of \code{change_composition}.
#' @param x_label
#' @param xllimit
#' @param xulimit
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
           rounded_zeroes = FALSE,
           det_limit = NULL,
           terms = TRUE,
           x_label,
           xllimit,
           xulimit,
           ...) {
    if (class(model) != "coxph") {
      stop("forest_plot_comp is only implemented for Cox models.")
    }
    if (!is.list(composition_list)) {
      stop('`composition_list` should be a list.')
    }


    col_of_names <- names(composition_list)
    df <- rbindlist(composition_list, use.names = TRUE)
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


    fp <- forestplot(
      tabletext,
      graph.pos = 2,
      data_frame_for_forest_plot,
      xlog = TRUE,
      xticks = c(0.8, 0.9, 1.0, 1.1, 1.2),
      boxsize = .15,
      lwd.ci = 3,
      lwd.zero = 3,
      lwd.xaxis = 3,
      lwd.xticks = 3,
      clr.line = "black",
      clip = c(xllimit, xulimit),
      xlab = x_label
      ,
      ...
    )
    return(fp)
  }





#
#
# cm <- comp_mean(dThrough, PA_labels)
# cmdf <- data.frame(matrix(nrow = 1, ncol = 0))
# for (name in names(cm)) {
#   cmdf[, name] <- cm[[name]]
# }
# acmv <- change_composition(cmdf, "vigorous", 0.5, PA_labels)
# acmm <- change_composition(cmdf, "moderate", 2.5, PA_labels)
# acms <- change_composition(cmdf, "sedentary",-7, PA_labels)
#
#
# vector_of_fixed_compositions <- list(
#   "ACM" = list("REFERENCE: At\ncompositional mean",
#                cmdf),
#   "ACMplusVIG" = list("Extra 30 mins/wk \n vigorous activity",
#                       acmv),
#   "ACMplusMOD" = list("Extra 150 mins/wk \n moderate activity",
#                       acmm),
#   "ACMplusSED" = list("7 hr/wk less \n sedentary time",
#                       acms)
# )
#
# vector_of_fixed_compositions[["ACM"]][[2]]
# for (element in names(vector_of_fixed_compositions)) {
#   this_pred <-
#     predict_HR_and_CI(cox_coda_adj,fixed_values_plain,
#                       vector_of_fixed_compositions[[element]][[2]],
#
#                       PA_labels)
#   print(this_pred)
#   vector_of_fixed_compositions[[element]] <-
#     list(vector_of_fixed_compositions[[element]][[1]],
#          vector_of_fixed_compositions[[element]][[2]],
#          this_pred)
# }
#
# coef <- c()
# low <- c()
# high <- c()
# name <- c()
# vig <- c("Vigorous")
# mod <- c("Moderate")
# light <- c("Light")
# sedentary <- c("Sedentary")
# sleep <- c("Sleep")
#
# vector_of_fixed_compositions[["ACM"]][[3]][2]
# vector_of_fixed_compositions[["ACMplusVIG"]][[3]]
# for (element in names(vector_of_fixed_compositions)) {
#   name <- c(name, vector_of_fixed_compositions[[element]][[1]])
#   coef <- c(coef, vector_of_fixed_compositions[[element]][[3]][[1]])
#   low <- c(low, vector_of_fixed_compositions[[element]][[3]][[2]])
#   high <- c(high, vector_of_fixed_compositions[[element]][[3]][[3]])
#   vig <-
#     c(vig,
#       signif(vector_of_fixed_compositions[[element]][[2]]$vigorous, digits = 3))
#   mod <-
#     c(mod,
#       signif(vector_of_fixed_compositions[[element]][[2]]$moderate, digits = 3))
#   light <-
#     c(light,
#       signif(vector_of_fixed_compositions[[element]][[2]]$light, digits = 3))
#   sedentary <-
#     c(sedentary,
#       signif(vector_of_fixed_compositions[[element]][[2]]$sedentary, digits = 3))
#   sleep <-
#     c(sleep,
#       signif(vector_of_fixed_compositions[[element]][[2]]$sleep, digits = 3))
# }
# CI <- paste(signif(low, digits = 2), "-", signif(high, digits = 2))
# CI[1] <- NA
# CI[4] <- "0.91 - 1.01"
# data_frame_for_forest_plot <-
#   data.frame(coef = c(NA, coef),
#              low = c(NA, low),
#              high = c(NA, high))
#
# tabletext <- cbind(c(NA, name), c("HR", signif(coef, digits = 3)), c("95% CI", CI))
#
# fpTxtGp(label = gpar(fontfamily = "HersheySerif"))
# fp <- forestplot(
#   tabletext,
#   graph.pos = 2,
#   data_frame_for_forest_plot,
#   xlog = TRUE,
#   xticks = c(0.8, 0.9, 1.0, 1.1, 1.2),
#   boxsize = .15,
#   lwd.ci = 3,
#   lwd.zero = 3,
#   lwd.xaxis = 3,
#   lwd.xticks = 3,
#   clr.line = "black",
#   clip = c(0.8, 1.2),
#   xlab = "Hazard Ratio",
#   txt_gp = fpTxtGp(
#     label = gpar(
#       fontfamily = "sans",
#       cex = 2,
#       fontface = 2
#     ),
#     xlab = gpar(
#       fontfamily = "sans",
#       cex = 2,
#       fontface = 2
#     ),
#     ticks = gpar(cex = 2, fontface = 2)
#   )
# )
