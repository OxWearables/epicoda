#' @title
#' plot_transfers: Plots model predictions.
#'
#' @description 
#' Plots model predictions for transfers/substitutions between the named parts.
#'
#' @param from_part Should be an element of `comp_labels`.
#' @param to_part Should be an element of `comp_labels`.
#' @inheritParams predict_fit_and_ci
#' @param yllimit Lower limit of y-axis shown on plot.
#' @param yulimit Upper limit of y-axis shown on plot.
#' @param xllimit Lower limit of x-axis shown on plot. Should be in same scale
#' as `units`.
#' @param xulimit Upper limit of x-axis shown on plot. Should be in same scale
#' as `units`.
#' @param y_label Label for y-axis. `"suppressed"` is a special value which will
#' result in no label.
#' @param plot_log If this is `TRUE`, the y-axis will be log-transformed.
#' @param lower_quantile If set, this gives the lower limit of plotting (as a
#' quantile for both variables of interest). In practice, the current behaviour
#' is to calculate the range of both variables between the upper and lower
#' quantile, and use the narrower one.
#' @param upper_quantile  If set, this gives the upper limit of plotting (as a
#' quantile for both variables of interest).
#' @param granularity Does not usually require setting. If set, gives the number
#' of points plotted on the graph. If it is too low, the plot will contain gaps.
#' If it is too high, plotting will be slow.
#' @param point_specification Should be a `ggplot2::geom_point` object
#' specifying how the points on the graph will be plotted.
#' @param error_bar_colour Should be an R-recognised colour for error bars,
#' specified by name in quotation marks.
#' @param theme Optional \code{theme} argument which can be set as a
#' `ggplot2::theme` object and will control how the plot appears.
#'
#' @return
#' Plot with balance of two parts plotted as exposure/ independent variable.
#' 
#' @importFrom ggplot2 aes element_line element_text geom_errorbar geom_hline geom_point geom_vline ggplot labs scale_y_continuous theme xlim ylim
#' @importFrom scales log_trans
#' 
#' @examples
#' lm_outcome <- comp_model(
#'   type = "linear",
#'   outcome = "BMI",
#'   covariates = c("agegroup", "sex"),
#'   data = simdata,
#'   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
#'   rounded_zeroes = FALSE
#' )
#'
#' plot_transfers(
#'   from_part = "sedentary",
#'   to_part = "moderate",
#'   model = lm_outcome ,
#'   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
#'   y_label = "Model-predicted difference in BMI",
#'   units = "hr/day",
#'   terms = TRUE
#' )

#' @export
plot_transfers <- function(
  from_part,
  to_part,
  model,
  comp_labels,
  terms = TRUE,
  part_1 = NULL,
  yllimit = NULL,
  yulimit = NULL,
  xllimit = NULL,
  xulimit = NULL,
  y_label = NULL,
  plot_log = FALSE,
  lower_quantile = 0.05,
  upper_quantile = 0.95,
  units = "unitless",
  specified_units = NULL,
  fixed_values = NULL,
  granularity = 10000,
  point_specification = geom_point(size = 2),
  error_bar_colour = "grey",
  theme = NULL
) {

  # Set theme for plotting
  theme_for_plots <- if (is.null(theme)) {
    theme(
      line = element_line(size = 1),
      axis.ticks = element_line(size = 2),
      text = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(
        size = 15,
        face = "bold",
        colour = "black"
      ),
      axis.text.x = element_text(
        size = 15,
        face = "bold",
        colour = "black"
      )
    )
  } else {
    theme
  }

  # We set units
  comp_sum <- as.double(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]

  # We assign some internal parameters
  type <- process_model_type(model)

  # We label what the transformed columns will be
  if (!is.null(part_1)) {
    comp_labels <- alter_order_comp_labels(comp_labels, part_1)
  }

  transf_labels <- transf_labels(
    comp_labels = comp_labels,
    transformation_type = "ilr",
    part_1 = part_1
  )

  # We back calculate the dataset used to derive the model
  dataset_ready <- get_dataset_from_model(
    model = model,
    comp_labels = comp_labels,
    transf_labels = transf_labels,
    type = type
  )

  # We find the reference values
  cm <- get_cm_from_model(
    model = model,
    comp_labels = comp_labels,
    transf_labels = transf_labels
  )$cm
  cm_on_scale <- rescale_comp(
    data = cm,
    comp_labels = comp_labels,
    comp_sum = comp_sum
  )

  # We make sure there will be a y_label, unless this is specified as "suppressed"
  y_label <- process_axis_label(
    label = y_label,
    type = type,
    terms = terms
  )

  # We assign some fixed_values to use in setting up new_data
  fixed_values <- if (!(is.null(fixed_values))) {
    if (length(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels]) > 0) {
      message(
        '`fixed_values` will be updated to have compositional parts fixed at the compositional mean. For technical and pragmatic reasons, use of a different reference for the compositional parts is not currently possible.'
      )
    }
    cbind(fixed_values, cm)
  } else {
    cbind(generate_fixed_values(dataset_ready, comp_labels), cm)
  }

  # We make some new data for predictions
  if ((!(from_part %in% comp_labels)) || !(to_part %in% comp_labels)) {
    stop('`from_part` or `to_part` not in `comp_labels`.')
  }
  new_data <- make_new_data(
    from_part,
    to_part,
    fixed_values = fixed_values,
    dataset  = dataset_ready,
    units = "hr/day",
    comp_labels = comp_labels,
    lower_quantile = lower_quantile,
    upper_quantile = upper_quantile,
    granularity = granularity
  )

  # We normalise this to work with it
  new_data <- normalise_comp(data = new_data, comp_labels = comp_labels)

  new_data <- suppressMessages(
    transform_comp(
      new_data,
      comp_labels,
      transformation_type = "ilr",
      part_1 = part_1,
      rounded_zeroes = FALSE
    )
  )

  dNew <- predict_fit_and_ci(
    model = model,
    new_data = new_data,
    fixed_values = fixed_values,
    part_1 = part_1,
    comp_labels = comp_labels,
    units = units,
    specified_units = specified_units,
    terms = terms
  )

  # We pull out the required values on the needed scale
  dNew$axis_vals <- dNew[, to_part] - rep(cm_on_scale[1, to_part], by = nrow(dNew))
  dNew$axis_vals2 <- -dNew[, from_part] + rep(cm_on_scale[1, from_part], by = nrow(dNew))

  # Check no pathology in axis value assignment
  if (!(isTRUE(all.equal(dNew$axis_vals, dNew$axis_vals2)))) {
    stop('Axis vals differ')
  }

  # Assign limit values
  if (is.null(yllimit)) {
    yllimit <- min(dNew$lower_CI)
  }
  if (is.null(yulimit)) {
    yulimit <- max(dNew$upper_CI)
  }
  if (is.null(xllimit)) {
    xllimit <- min(dNew$axis_vals)
  }
  if (is.null(xulimit)) {
    xulimit <- max(dNew$axis_vals)
  }

  dNew$lower_CI <- pmax(rep(yllimit, by = length(dNew$lower_CI)), dNew$lower_CI)
  dNew$upper_CI <- pmin(rep(yulimit, by = length(dNew$lower_CI)), dNew$upper_CI)

  ## We begin the plotting
  if (type == "logistic") {
    
    # Logistic
    if (plot_log) {
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        scale_y_continuous(
          trans = log_trans(),
          breaks = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.2),
          labels = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.2),
          minor_breaks = NULL,
          limits = c(yllimit, yulimit)
        ) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 1) +
        theme_for_plots
    } else {
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        ylim(yllimit, yulimit) + xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 1) +
        theme_for_plots
    }
    
  } else if (type == "cox") {
    
    # Cox-PH
    if (plot_log) {
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 0) +
        scale_y_continuous(
          trans = log_trans(),
          breaks = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.1),
          labels = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.1),
          minor_breaks = NULL,
          limits = c(yllimit, yulimit)
        ) +
        theme_for_plots
    } else {
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        ylim(yllimit, yulimit) + xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 0) +
        theme_for_plots
    }
    
  } else if (type == "linear") {
    
    # Linear
    if (plot_log) {
      if (terms) {
        warning(
          'Taking the log transformation doesn\'t make sense for values near 0 and the graph is likely to look very strange'
        )
      }
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        scale_y_continuous(
          trans = log_trans(),
          breaks = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.2),
          labels = seq(round(yllimit, digits = 1), round(yulimit, digits = 1), by = 0.2),
          minor_breaks = NULL,
          limits = c(yllimit, yulimit)
        ) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        theme_for_plots
    } else {
      plot_of_this <- ggplot(data = dNew, mapping = aes(x = axis_vals, y = fit)) +
        ylim(yllimit, yulimit) + xlim(xllimit, xulimit) +
        geom_errorbar(
          aes(
            x = axis_vals,
            ymin = lower_CI,
            ymax = upper_CI
          ),
          color = error_bar_colour
        ) +
        point_specification +
        labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        theme_for_plots
    }
    
  }

  # Message about plotting
  message('Please note that plotting may take some time.')

  # Return compositional mean for reference
  attr(plot_of_this, "cm") <- cm_on_scale

  return(plot_of_this)
}
