#' plot_transfers: Plots model predictions.
#'
#' Plots model predictions for transfers between the given parts.
#'
#' @param from_part Should be an element of \code{comp_labels}.
#' @param to_part Should be an element of \code{comp_labels}. Should have compositional mean less than \code{from_part}.
#' @inheritParams predict_fit_and_ci
#' @param yllimit Upper limit to show on y-axis on plot.
#' @param yulimit Lower limit to show on y-axis on plot.
#' @param y_label Label for y-axis on plot.
#' @param plot_log If this is \code{TRUE}, the y-axis will be log-transformed.
#' @param lower_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param upper_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param granularity Doesn't usually need setting. Parameter indicating how many predictions to make. If too low, plotted curve has gaps. If too high, calculation is slow.
#' @param theme Optional \code{theme} argument which can be set as a \code{ggplot2::theme} object and will control how the plot appears.
#' @return Plot with balance of two parts plotted as exposure/ independent variable.
#' @export
#' @examples
plot_transfers <- function(from_part,
                           to_part,
                           model,
                           dataset,
                           fixed_values = NULL,
                           transformation_type = NULL,
                           comparison_part = NULL,
                           part_1 = NULL,
                           comp_labels,
                           yllimit = NULL,
                           yulimit = NULL,
                           y_label = NULL,
                           plot_log = FALSE,
                           lower_quantile = 0.05,
                           upper_quantile = 0.95,
                           units = "unitless",
                           specified_units = NULL,
                           rounded_zeroes = TRUE,
                           det_limit = NULL,
                           terms = TRUE,
                           granularity = 10000,
                           point_specification = ggplot2::geom_point(size = 2),
                           error_bar_colour = "grey",
                           theme = NULL) {
  if (is.null(transformation_type)) {
    stop(
      "transformation_type must be specified and must match the transformation used in transform_comp earlier (which defaults to \"ilr\")"
    )
  }

  # We normalise
  dataset <- normalise_comp(data = dataset, comp_labels = comp_labels)


  # Set theme for plotting
  if (is.null(theme)){
    theme_for_plots <-
      ggplot2::theme(
        line = ggplot2::element_line(size = 1),
        axis.ticks = ggplot2::element_line(size= 2),
        text = ggplot2::element_text(size = 15, face = "bold"),
        axis.text.y = ggplot2::element_text(
          size = 15,
          face = "bold",
          colour = "black"
        ),
        axis.text.x = ggplot2::element_text(
          size = 15,
          face = "bold",
          colour = "black"
        )
      )
  }
  else{
    theme_for_plots <- theme
  }



  # We set units
  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]


  # We label what the transformed cols will be
  if (transformation_type == "ilr") {
    if (!is.null(part_1)) {
      comp_labels <- alter_order_comp_labels(comp_labels, part_1)
    }
  }
  transf_labels <-
    transf_labels(comp_labels,
                  transformation_type,
                  comparison_part = comparison_part,
                  part_1 = part_1)

  dataset_ready <-
    dataset[,!(colnames(dataset) %in% transf_labels)]


  # We assign some internal parameters
  type <- process_model_type(model)


  # We make sure there will be a y_label, unless this is specified as "suppressed"
  y_label <- process_axis_label(label = y_label, type = type, terms = terms)


# We calculate the compositional mean so we can use it in future calculations
  cm <- comp_mean(
      dataset,
      comp_labels,
      rounded_zeroes = rounded_zeroes, det_limit = det_limit,
      units = "unitless"
    )
  cm_transf_df <- transform_comp(cm, comp_labels,
                                 transformation_type = transformation_type,
                                 part_1 = part_1,
                                 comparison_part = comparison_part,
                                 rounded_zeroes = rounded_zeroes, det_limit = det_limit)

  cm_on_scale <- rescale_comp(cm, comp_labels = comp_labels, comp_sum = comp_sum)


  # We assign some fixed_values to use in setting up new_data
  if (!(is.null(fixed_values))) {
    if (length(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels]) > 0) {
      warning(
        "fixed_values will be updated to have compositional parts fixed at the compositional mean. For technical and pragmatic reasons, use of a different reference for the compositional parts is not currently possible."
      )
    }
    fixed_values <- cbind(fixed_values, cm)
  }
  if (is.null(fixed_values)) {
    fixed_values <-
      generate_fixed_values(
        dataset,
        comp_labels,
        rounded_zeroes = rounded_zeroes,
        det_limit = det_limit
      )
  }

  # We make some new data for predictions
  new_data <-
    make_new_data(
      from_part,
      to_part,
      fixed_values = fixed_values,
      dataset  = dataset_ready,
      units = "hr/day",
      comp_labels = comp_labels,
      lower_quantile = 0.05,
      upper_quantile = 0.95,
      granularity = granularity
    )


  # We normalise this to work with it
  new_data <- normalise_comp(data = new_data, comp_labels = comp_labels)

  new_data <-
    transform_comp(
      new_data,
      comp_labels,
      transformation_type = transformation_type,
      part_1 = part_1,
      comparison_part = comparison_part,
      rounded_zeroes = FALSE
    )


  dNew <- predict_fit_and_ci(model = model,
                             dataset = dataset,
                             new_data = new_data,
                             fixed_values = fixed_values,
                             transformation_type = transformation_type,
                             comparison_part = comparison_part,
                             part_1 = part_1,
                             comp_labels = comp_labels,
                             units = units,
                             specified_units = specified_units,
                             rounded_zeroes = rounded_zeroes,
                             det_limit = det_limit,
                             terms = terms)
  # We normalise again
  dNew <- normalise_comp(data = dNew, comp_labels = comp_labels)


  # We pull out the required values on the needed scale
  dToScale <- rescale_comp(data = dNew, comp_labels = comp_labels, comp_sum = comp_sum)
  dNew$axis_vals <-
    dToScale[, to_part] - rep(cm_on_scale[1, to_part], by = nrow(dNew))


  if (is.null(yllimit)) {
    yllimit <- min(dNew$lower_CI)
  }
  if (is.null(yulimit)) {
    yulimit <- max(dNew$upper_CI)
  }

  dNew$lower_CI <-
    pmax(rep(yllimit, by = length(dNew$lower_CI)), dNew$lower_CI)
  dNew$upper_CI <-
    pmin(rep(yulimit, by = length(dNew$lower_CI)), dNew$upper_CI)





  # We begin the plotting
  if (type == "logistic") {
    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
         breaks = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.2),
        labels = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.2)
        ) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::geom_hline(yintercept = 1) +
        theme_for_plots
    }

    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::geom_hline(yintercept = 1) +
        theme_for_plots
    }
  }








  if (type == "cox") {
    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
          breaks = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.1),
          labels = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.1),
          limits = c(yllimit, yulimit)
        )+
        theme_for_plots
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0) +
        theme_for_plots
    }
  }






  if (type == "linear") {
  if (plot_log == TRUE) {
      if (terms){
        warning('Taking the log transformation doesn\'t make sense for values near 0 and the graph is likely to look very strange')
      }
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
         breaks = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.2),
        labels = seq(signif(yllimit, digits = 1), signif(yulimit, digits = 1), by = 0.2)
        ) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::geom_hline(yintercept = 0) +
        theme_for_plots
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes_(x = dNew$axis_vals, y = dNew$fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes_(
          x = dNew$axis_vals,
          ymin = dNew$lower_CI,
          ymax = dNew$upper_CI
        ),
        color = error_bar_colour) +
        point_specification +
        ggplot2::labs(x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
                      y = y_label) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::geom_hline(yintercept = 0) +
        theme_for_plots
    }
  }


    print("Please note that plotting may take some time.")
    return(plot_of_this)
  }
