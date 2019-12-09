#' plot_transfers: Plots model predictions.
#'
#' Plots model predictions for the transfer of time given.
#'
#' @param from_component Should be an element of \code{comp_labels}.
#' @param to_component Should be an element of \code{comp_labels}. Should have compositional mean less than \code{from_component}.
#' @param model
#' @param dataset Should be dataset used to develop \code{model}. Used to set reasonable values to display predictions for based on range of the data.
#' @param fixed_values If desired, fixed_values for variables in \code{dataset} which aren't in \code{comp_labels}. These will be used when making predictions if \code{terms = FALSE}.
#' @param transformation_type Should match transformation used in \code{transform_comp} when developing models.
#' @param comparison_component If used, should match transformation used in \code{transform_comp} when developing models.
#' @param component_1 If used, should match transformation used in \code{transform_comp} when developing models.
#' @param comp_labels
#' @param yllimit Upper limit to show on y-axis on plot.
#' @param yulimit Lower limit to show on y-axis on plot.
#' @param y_label Label for y-axis on plot.
#' @param plot_log If this is \code{TRUE}, the y-axis will be log-transformed.
#' @param lower_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param upper_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param units What are the units of the compositional variables? E.g. for activity data "hr/day". Currently all non-activity exposure variables should be specified as unitless until support for alternatives units is added.
#' @param terms Are predictions for terms,or are they absolute?
#' @return Plot with balance of two components plotted as exposure/ independent variable.
#' @examples
plot_transfers <- function(from_component,
                           to_component,
                                model,
                                dataset,
                                fixed_values = NULL,
                                transformation_type = NULL,
                                comparison_component = NULL,
                                component_1 = NULL,
                                comp_labels,
                                yllimit = NULL,
                                yulimit = NULL,
                                y_label = NULL,
                                plot_log = FALSE,
                                lower_quantile = 0.05,
                                upper_quantile = 0.95,
                                units = "unitless",
                                specified_units = NULL,
                                rounded_zeroes = FALSE,
                                det_limit = NULL,
                                terms = TRUE,
                                granularity = 10000){

  if (is.null(transformation_type)){
    stop("transformation_type must be specified and must match the transformation used in transform_comp earlier (which defaults to \"ilr\")")
  }

  # We make sure there will be a y_label
  if ((is.null(y_label)) & terms){
    y_label <- "Model-predicted change in outcome"
  }
  if ((is.null(y_label)) & (terms == FALSE)){
    y_label <- "Model-predicted outcome"
  }

  # We set units
  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]


  # We label what the transformed cols will be
  if (transformation_type == "ilr"){
    if (!is.null(component_1)){
      comp_labels <- alter_order_comp_labels(comp_labels, component_1)
    }
  }
  transf_labels <- transf_labels(comp_labels, transformation_type, comparison_component = comparison_component, component_1 = component_1)

  dataset_ready <- dataset[, !(colnames(dataset) %in% transf_labels)]
  # We assign some internal parameters
  type <- "unassigned"
  if (class(model)=="lm"){
    type <- "linear"
  }
  if ((class(model)[1] == "glm") && (family(model)[[1]] == "binomial")){
    type <- "logistic"
  }
  if ((class(model) == "coxph")){
    type <- "cox"
  }
  if (type == "unassigned"){
    stop("model is not a recognised type of model.")
  }
  if (is.null(yllimit) & (type == "cox")) {
    yllimit <- 0.5
  }
  if (is.null(yulimit) & (type == "cox")) {
    yulimit <- 1.75
  }
  if (is.null(yllimit) & (type == "logistic") && (terms == FALSE)) {
    yllimit <- 0
  }
  if (is.null(yllimit) & (type == "logistic") && (terms == TRUE)) {
    yllimit <- -1
  }
  if (is.null(yulimit) & (type == "logistic")) {
    yulimit <- 1
  }



  # We assign some fixed_values to use in plotting

  cm <- comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units, specified_units = specified_units)
  if (!(is.null(fixed_values))){
    if (!is.null(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels])){
      warning("fixed_values will be updated to have compositional components fixed at the compositional mean. For technical and pragmatic reasons, use of a different reference for the compositional components is not currently possible.")
    }
    fixed_values <- cbind(fixed_values, cm)
  }
  if (is.null(fixed_values)){
    fixed_values <- generate_fixed_values(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units, specified_units = specified_units)
  }
  # We make some new data for predictions
  new_data <-
    make_new_data(from_component,
               to_component,
               fixed_values,
               dataset_ready,
               units = units,
               comp_labels = comp_labels,
               lower_quantile = 0.05,
               upper_quantile = 0.95,
               granularity = granularity)

  new_data <-
    transform_comp(new_data,
                   comp_labels,
                   transformation_type = transformation_type,
                   component_1 = component_1,
                   comparison_component = comparison_component,
                   rounded_zeroes = FALSE)



  # We begin the plotting
  if (type == "logistic" && (terms == FALSE)) {
    message("Note that the confidence intervals on this plot include uncertainty driven by other, non-compositional variables. To look at compositional variables only, use terms = TRUE")
    predictions <- predict(model,
                           newdata = new_data,
                           type = "link",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]
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
                        mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n", units_name),
             y = y_label) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          limits = c(yllimit, yulimit)
        ) +
        ggplot2::geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n " , units),
             y = y_label) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }






  if (type == "logistic" && (terms)) {



    predictions <- predict(model,
                           newdata = new_data,
                           type = "terms", terms = transf_labels,
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]
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
                        mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n " , units),
             y = y_label) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          limits = c(yllimit, yulimit)
        ) +
        ggplot2::geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n " , units),
             y = y_label) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }















  if (type == "cox" && (terms)) {
    predictions <- predict(model,
                           newdata = new_data,
                           type = "terms",
                           se.fit = TRUE,
                           terms = transf_labels)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]

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
                        mapping = ggplot2::aes(x = axis_vals, y = predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          x = paste(from_component, "to", to_component, "\n ", units),
          y = y_label) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.2),
          labels = seq(yllimit, yulimit, by = 0.2),
          limits = c(yllimit, yulimit)
        )
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          x = paste(from_component, "to", to_component, "\n ", units),
          y = y_label) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }





  if (type == "cox" && !(terms)) {
    predictions <- predict(model,
                           newdata = new_data, type = "risk",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]

    dNew$predictions <- dNew$fit

    dNew$lower_CI <-
      dNew$predictions * exp(-1.96 *dNew$se.fit)
    dNew$upper_CI <-
      dNew$predictions * exp(1.96 *dNew$se.fit)
    dNew$lower_CI <-
      pmax(rep(yllimit, by = length(dNew$lower_CI)), dNew$lower_CI)
    dNew$upper_CI <-
      pmin(rep(yulimit, by = length(dNew$lower_CI)), dNew$upper_CI)

    if (plot_log == TRUE) {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          x = paste(from_component, "to", to_component, "\n ", units),
          y = y_label) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.2),
          labels = seq(yllimit, yulimit, by = 0.2),
          limits = c(yllimit, yulimit)
        )
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(
          x = paste(from_component, "to", to_component, "\n ", units),
          y = y_label) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }















  if (type == "linear" && (terms == FALSE)) {
    message("Note that the confidence intervals on this plot include uncertainty driven by other, non-compositional variables.")
    predictions <- predict(model, newdata = new_data, type = "response",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]

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
                        mapping = ggplot2::aes(x = axis_vals, y = fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n ", units),
             y = y_label) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.1),
          labels = seq(yllimit, yulimit, by = 0.1)
        ) +
        ggplot2::geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n " , units),
             y = y_label) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }











  if (type == "linear" && (terms)) {
    predictions <- predict(model, newdata = new_data, type = "terms", terms = transf_labels, interval = "confidence",
                           se.fit = TRUE)
    dNew <- data.frame(new_data, predictions)
    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")

    dNew$fit <- eval(parse(text = sum_for_args))
    dNew$axis_vals <-  dNew[, to_component] - comp_mean(dataset, comp_labels, rounded_zeroes = TRUE, det_limit = det_limit, units = units)[[to_component]]



    m <- (model.matrix(model)[, transf_labels])
    middle_matrix <- solve(t(m) %*% m)
    x <- data.matrix(new_data[, transf_labels])
    # t_x <- t(as.matrix(new_data[, transf_labels], rownames= TRUE)[2:nrow(new_data),])
    # t_x <- data.matrix(t_x)
    # print(head(t_x))

    in_sqrt_1 <- ( x %*%middle_matrix )
    t_x <- as.matrix(t(x))
    in_sqrt_true <- c()
    for (i in 1:nrow(in_sqrt_1)){

      in_sqrt_true <- c(in_sqrt_true, (in_sqrt_1[i, ] %*% data.matrix(t_x)[, i]))
    }

    value <- sqrt(data.matrix(in_sqrt_true))
    mse <- mean(model$residuals^2, na.rm = TRUE)
    sigma_est <- sqrt(mse)

    scaling <- sigma_est * value

    t_value <- qt(0.975, df = (nrow(m) - 1- length(transf_labels)))[[1]]




















    for (label in transf_labels){
      dNew$lower_CI <- dNew$fit - t_value*scaling
      dNew$upper_CI <- dNew$fit + t_value*scaling
    }
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
                        mapping = ggplot2::aes(x = axis_vals, y = fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n ", units),
             y = y_label) +
        ggplot2::scale_y_continuous(
          trans = log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.1),
          labels = seq(yllimit, yulimit, by = 0.1)
        ) +
        ggplot2::geom_vline(xintercept = 0)
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(data = dNew,
                        mapping = ggplot2::aes(x = axis_vals, y = fit)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ), color = "grey") +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::labs(x = paste(from_component, "to", to_component, "\n", units),
             y = y_label) +
        ggplot2::geom_vline(xintercept = 0)
    }
  }

 print("Please note that plotting may take some time.")
 if (terms == FALSE){
     short_form <- gsub( ".*~", "",as.character(formula(model)))
     print(paste("Covariate values were fixed at: "))
     variables <- strsplit(short_form[3], " + ", fixed = TRUE)[[1]]
    for (variable in variables[!(variables %in% transf_labels)]){
         print(paste(variable, ":", fixed_values[1, variable]))
      }
     }
  print("Compositional variables not varied in the visualisation were fixed at:")
  for (variable in comp_labels){
    print(paste(variable, ":", signif(fixed_values[1, variable], 2), units))
  }
  return(plot_of_this)
 }


