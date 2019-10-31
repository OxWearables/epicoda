#' plot_transfers: Plots model predictions.
#'
#' Plots model predictions for the transfer of time given.
#'
#' @param time_from Should be an element of \code{comp_labels}.
#' @param time_to Should be an element of \code{comp_labels}. Should have compositional mean less than \code{time_from}.
#' @param model
#' @param dataset Should be dataset used to develop \code{model}. Used to set reasonable values to display predictions for based on range of the data.
#' @param fixed_values
#' @param transformation_type Should match transformation used in \code{activity_trans} when developing models.
#' @param comp_labels
#' @param yllimit
#' @param yulimit
#' @param y_label
#' @param plot_log
#' @param lower_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param upper_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param units_label What are the units of the compositional variables? E.g. for activity data "hrs/day". NB all the calculations are unitless,
#' @return Plot with balance of two components plotted as exposure/ independent variable.
#' @examples

plot_transfers <- function(time_from,
                                time_to,
                                model,
                                dataset,
                                fixed_values = NULL,
                                comp_labels,
                                yllimit = NULL,
                                yulimit = NULL,
                                y_label,
                                plot_log = FALSE,
                                lower_quantile = 0.05,
                                upper_quantile = 0.95,
                                units_label) {
  type <- "unassigned"
  if (class(model)=="lm"){
    type <- "linear"
  }
  if ((class(model)[1] == "glm") & (family(model)[[1]] == "binomial")){
    type <- "logistic"
  }
  if ((class(model) == "coxph")){
    type <- "cox"
  }
  if (type == "unassigned"){
    stop("model is not a recognised type of model.")
  }
  if (is.null(yllimit) & type == "cox") {
    yllimit <- 0.5
  }
  if (is.null(yulimit) & type == "cox") {
    yulimit <- 1.75
  }
  if (is.null(yllimit) & type == "logistic") {
    yllimit <- 0
  }
  if (is.null(yulimit) & type == "logistic") {
    yulimit <- 1
  }

  if (is.null(fixed_values)){
    fixed_values <- generate_fixed_values(dataset, comp_labels)
  }
  cm <- comp_mean(dataset, comp_labels)
  if (!(is.null(fixed_values))){
    if (!is.null(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels])){
      warning("fixed_values will be updated to have compositional components fixed at the compositional mean. For technical and pragmatic reasons, use of a different reference for the compositional components is not currently possible.")
    }
    for (label in comp_labels){
      fixed_values[, label] <- cm[[label]]
    }
  }
  new_data <-
    make_new_data(
      time_from,
      time_to,
      dataset,
      fixed_values,
      comp_labels,
      lower_quantile,
      upper_quantile
    )
  new_data <-
    activity_trans(new_data,
                   comp_labels,
                   transformation_type,
                   rounded_zeroes = FALSE)

  if (type == "logistic") {
    predictions <- predict(model,
                           newdata = new_data,
                           type = "link",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, time_to] - comp_mean(dataset, comp_labels)[[time_to]]
    dNew$normalised_predictions <- model$family$linkinv(dNew$fit)

    dNew$lower_CI <-
      model$family$linkinv(dNew$fit - 1.96 * dNew$se.fit)
    dNew$upper_CI <-
      model$family$linkinv(dNew$fit + 1.96 * dNew$se.fit)
    dNew$lower_CI <-
      pmax(rep(yllimit, by = length(dNew$lower_CI)), dNew$lower_CI)
    dNew$upper_CI <-
      pmin(rep(yulimit, by = length(dNew$lower_CI)), dNew$upper_CI)

    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = normalised_predictions)) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(x = paste(time_from, "to", time_to, "\n (hr/week)"),
             y = y_label) +
        scale_y_continuous(
          trans = log_trans(),
          limits = c(yllimit, yulimit)
        ) +
        geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = normalised_predictions)) +
        ylim(yllimit, yulimit) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(x = paste(time_from, "to", time_to, "\n (hr/week)"),
             y = y_label) +
        geom_vline(xintercept = 0)
    }
  }




  if (type == "cox") {
    transf_vec_for_here <- transf_vector(PA_label_dictionary[[comp_labels]])
    print(transf_vec_for_here)
    predictions <- predict(model,
                           newdata = new_data,
                           type = "terms",
                           se.fit = TRUE,
                           terms = transf_vec_for_here)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, time_to] - comp_mean(dataset, comp_labels)[[time_to]]

    vector_for_args <-   paste("dNew$fit.", transf_vec_for_here, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")

    vector_for_se <- paste("dNew$se.fit.", transf_vec_for_here, sep = "")
    sum_for_se <- paste0(vector_for_se, collapse = "+")
    dNew$predictions <- exp(eval(parse(text = sum_for_args)))

    dNew$lower_CI <-
      dNew$predictions * exp(-1.96 * eval(parse(text = sum_for_se)))
    dNew$upper_CI <-
      dNew$predictions * exp(1.96 * eval(parse(text = sum_for_se)))
    dNew$lower_CI <-
      pmax(rep(yllimit, by = length(dNew$lower_CI)), dNew$lower_CI)
    dNew$upper_CI <-
      pmin(rep(yulimit, by = length(dNew$lower_CI)), dNew$upper_CI)

    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = predictions)) +
        ylim(yllimit, yulimit) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(
          x = paste(time_from, "to", time_to, "\n ", units_label),
          y = y_label) +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 0) +
        scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.2),
          labels = seq(yllimit, yulimit, by = 0.2),
          limits = c(yllimit, yulimit)
        )
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = predictions)) +
        ylim(yllimit, yulimit) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(
          x = paste(time_from, "to", time_to, "\n ", units_label),
          y = y_label) +
        geom_hline(yintercept = 1) +
        geom_vline(xintercept = 0)
    }
  }
  if (type == "linear") {
    predictions <- predict(model, newdata = new_data,
                           se.fit = TRUE)
    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, time_to] - comp_mean(dataset, comp_labels)[[time_to]]
    dNew$normalised_predictions <- dNew$fit

    dNew$lower_CI <- dNew$fit - 1.96 * dNew$se.fit
    dNew$upper_CI <- dNew$fit + 1.96 * dNew$se.fit
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

    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = normalised_predictions)) +
        ylim(yllimit, yulimit) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(x = paste(time_from, "to", time_to, "\n ", units_label),
             y = y_label) +
        scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.1),
          labels = seq(yllimit, yulimit, by = 0.1)
        ) +
        geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = aes(x = axis_vals, y = normalised_predictions)) +
        ylim(yllimit, yulimit) +
        geom_errorbar(aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        geom_point(size = 0.5) +
        labs(x = paste(time_from, "to", time_to, "\n (hr/week)"),
             y = y_label) +
        geom_vline(xintercept = 0)
    }
  }
  return(plot_of_this)

}


