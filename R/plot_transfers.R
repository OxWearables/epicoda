#' plot_transfers: Plots model predictions.
#'
#' Plots model predictions for transfers between the given parts.
#'
#' @param from_part Should be an element of \code{comp_labels}.
#' @param to_part Should be an element of \code{comp_labels}. Should have compositional mean less than \code{from_part}.
#' @param model
#' @param dataset Should be dataset used to develop \code{model}. Used to set reasonable values to display predictions for based on range of the data.
#' @param fixed_values If desired, fixed_values for variables in \code{dataset} which aren't in \code{comp_labels}. These will be used when making predictions if \code{terms = FALSE}.
#' @param transformation_type Should match transformation used in \code{transform_comp} when developing models.
#' @param comparison_part If used, should match transformation used in \code{transform_comp} when developing models.
#' @param part_1 If used, should match transformation used in \code{transform_comp} when developing models.
#' @param comp_labels
#' @param yllimit Upper limit to show on y-axis on plot.
#' @param yulimit Lower limit to show on y-axis on plot.
#' @param y_label Label for y-axis on plot.
#' @param plot_log If this is \code{TRUE}, the y-axis will be log-transformed.
#' @param lower_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param upper_quantile See \code{vary_time_of_interest} and \code{make_new_data}
#' @param units What are the units of the compositional variables? E.g. for activity data "hr/day". Currently all non-activity exposure variables should be specified as unitless until support for alternatives units is added.
#' @param terms Are predictions for terms,or are they absolute?
#' @return Plot with balance of two parts plotted as exposure/ independent variable.
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
                           rounded_zeroes = FALSE,
                           det_limit = NULL,
                           terms = TRUE,
                           granularity = 10000) {
  if (is.null(transformation_type)) {
    stop(
      "transformation_type must be specified and must match the transformation used in transform_comp earlier (which defaults to \"ilr\")"
    )
  }


  # Set theme for plotting
  theme_for_plots <-
    ggplot2::theme(
      line = ggplot2::element_line(size = 1.5),
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
  type <- "unassigned"
  if (class(model)[1] == "lm") {
    type <- "linear"
  }
  if ((class(model)[1] == "glm") &&
      (family(model)[[1]] == "binomial")) {
    type <- "logistic"
  }
  if ((class(model)[1]  == "coxph")) {
    type <- "cox"
  }
  if (type == "unassigned") {
    stop("model is not a recognised type of model.")
  }



  # We make sure there will be a y_label
  if ((is.null(y_label)) & (terms) & (type == "linear")) {
    y_label <- "Model-predicted difference in outcome"
  }
  if ((is.null(y_label)) & !(terms) & (type == "logistic")) {
    y_label <- "Model-predicted probability"
  }
  if ((is.null(y_label)) & (terms) & (type == "logistic")) {
    y_label <- "Model-predicted OR"
  }
  if ((is.null(y_label)) &  (type == "cox")) {
    y_label <- "Model-predicted HR"
  }
  if ((is.null(y_label)) & (terms == FALSE)) {
    y_label <- "Model-predicted outcome"
  }


# We calculate the compositional mean so we can use it in future calculations
  cm <-
    comp_mean(
      dataset,
      comp_labels,
      rounded_zeroes = FALSE,
      det_limit = det_limit,
      units = units,
      specified_units = specified_units
    )
  cmdf <- data.frame(cm)
  cm_transf_df <- transform_comp(cmdf, comp_labels,
                                 transformation_type = transformation_type,
                                 part_1 = part_1,
                                 comparison_part = comparison_part,
                                 rounded_zeroes = FALSE)

# We assign some fixed_values to use in plotting

  if (!(is.null(fixed_values))) {
    if (!is.null(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels])) {
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
        rounded_zeroes = FALSE,
        det_limit = det_limit,
        units = units,
        specified_units = specified_units
      )
  }

  transf_fixed_vals <- transform_comp(
    fixed_values[, colnames(fixed_values)[!(colnames(fixed_values) %in% transf_labels)]],
    comp_labels,
    transformation_type = transformation_type,
    part_1 = part_1,
    comparison_part = comparison_part,
    rounded_zeroes = FALSE
  )

  # We make some new data for predictions
  new_data <-
    make_new_data(
      from_part,
      to_part,
      fixed_values,
      dataset_ready,
      units = units,
      comp_labels = comp_labels,
      lower_quantile = 0.05,
      upper_quantile = 0.95,
      granularity = granularity
    )

  new_data <-
    transform_comp(
      new_data,
      comp_labels,
      transformation_type = transformation_type,
      part_1 = part_1,
      comparison_part = comparison_part,
      rounded_zeroes = FALSE
    )

  # We begin the plotting
  if (type == "logistic" && (terms == FALSE)) {
    message(
      "Note that the confidence intervals on this plot include uncertainty driven by other, non-compositional variables. To look at compositional variables only, use terms = TRUE"
    )
    predictions <- predict(model,
                           newdata = new_data,
                           type = "link",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-
      dNew[, to_part] - comp_mean(
        dataset,
        comp_labels,
        rounded_zeroes = FALSE,
        det_limit = det_limit,
        units = units
      )[[to_part]]
    dNew$normalised_predictions <- model$family$linkinv(dNew$fit)

    dNew$lower_CI <-
      model$family$linkinv(dNew$fit - 1.96 * dNew$se.fit)
    dNew$upper_CI <-
      model$family$linkinv(dNew$fit + 1.96 * dNew$se.fit)

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
        ggplot2::ggplot(
          data = dNew,
          mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)
        ) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
       ) +
        ggplot2::scale_y_continuous(trans = scales::log_trans(),
                                    limits = c(yllimit, yulimit)) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
       theme_for_plots
    }
    else {
      plot_of_this <-
        ggplot2::ggplot(
          data = dNew,
          mapping = ggplot2::aes(x = axis_vals, y = normalised_predictions)
        ) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
    }
  }






  if (type == "logistic" && (terms)) {
    predictions <-
      predict(
        model,
        newdata = new_data,
        type = "terms",
        terms = transf_labels,
        se.fit = TRUE
      )

    acm <- predict(model,
                   newdata = transf_fixed_vals,
                   type = "terms",
                   terms = transf_labels)


    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-
      dNew[, to_part] - comp_mean(
        dataset,
        comp_labels,
        rounded_zeroes = FALSE,
        det_limit = det_limit,
        units = units
      )[[to_part]]


    vector_for_args <-
      paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")


    dNew$log_odds_change <- eval(parse(text = sum_for_args)) - sum(acm)
    dNew$fit <- exp(dNew$log_odds_change)


    middle_matrix <- vcov(model)[transf_labels, transf_labels]
    x <- data.matrix(new_data[, transf_labels] - rep(cm_transf_df[, transf_labels], by = nrow(new_data)))
    in_sqrt_1 <- (x %*% middle_matrix)
    t_x <- as.matrix(t(x))
    in_sqrt_true <- c()
    for (i in 1:nrow(in_sqrt_1)) {
      in_sqrt_true <-
        c(in_sqrt_true, (in_sqrt_1[i,] %*% data.matrix(t_x)[, i]))
    }

    value <- sqrt(data.matrix(in_sqrt_true))

    t_value <-
      qt(0.975, df = (nrow(model.matrix(model)) -1 - length(transf_labels)))[[1]]


    alpha_lower <- dNew$log_odds_change - t_value * value
    alpha_upper <- dNew$log_odds_change + t_value * value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)

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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.1),
          labels = seq(yllimit, yulimit, by = 0.1)
        ) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
    }
  }








  if (type == "cox" && (terms)) {
    predictions <- predict(
      model,
      newdata = new_data,
      type = "terms",
      se.fit = TRUE,
      terms = transf_labels
    )

    acm <- predict(model,
                   newdata = transf_fixed_vals,
                   type = "terms",
                   terms = transf_labels)


    dNew <- data.frame(new_data, predictions)


    dNew$axis_vals <-  dNew[, to_part] - comp_mean(dataset, comp_labels, rounded_zeroes = FALSE,
                                                        det_limit = det_limit, units = units)[[to_part]]

    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")



    dNew$log_hazard_change <- eval(parse(text = sum_for_args)) - sum(acm)
    dNew$fit <- exp(dNew$log_hazard_change)

    middle_matrix <- vcov(model)[transf_labels, transf_labels]
    x <- data.matrix(new_data[, transf_labels] - rep(cm_transf_df[, transf_labels], by = nrow(new_data)))
    in_sqrt_1 <- (x %*% middle_matrix)
    t_x <- as.matrix(t(x))
    in_sqrt_true <- c()
    for (i in 1:nrow(in_sqrt_1)) {
      in_sqrt_true <-
        c(in_sqrt_true, (in_sqrt_1[i,] %*% data.matrix(t_x)[, i]))
    }

    value <- sqrt(data.matrix(in_sqrt_true))

    t_value <-
      qt(0.975, df = (nrow(model.matrix(model)) -1 - length(transf_labels)))[[1]]



    alpha_lower <- dNew$log_hazard_change - t_value*value
    alpha_upper <- dNew$log_hazard_change + t_value*value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)



    # dNew <- data.frame(new_data, predictions)
    # dNew$axis_vals <-
    #   dNew[, to_part] - comp_mean(
    #     dataset,
    #     comp_labels,
    #     rounded_zeroes = FALSE,
    #     det_limit = det_limit,
    #     units = units
    #   )[[to_part]]
    #
    # vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    # sum_for_args <- paste0(vector_for_args, collapse = "+")
    #
    # vector_for_se <- paste("dNew$se.fit.", transf_labels, sep = "")
    # sum_for_se <- paste0(vector_for_se, collapse = "^2 +")
    # dNew$predictions <- exp(eval(parse(text = sum_for_args)))
    # dNew$fit <- dNew$predictions
    # dNew$lower_CI <-
    #   dNew$predictions * exp(-1.96 * sqrt(eval(parse(text = sum_for_se))))
    # dNew$upper_CI <-
    #   dNew$predictions * exp(1.96 * sqrt(eval(parse(text = sum_for_se))))

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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.2),
          labels = seq(yllimit, yulimit, by = 0.2),
          limits = c(yllimit, yulimit)
        )+
        theme_for_plots
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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
    }
  }





  if (type == "cox" && !(terms)) {
    predictions <- predict(model,
                           newdata = new_data,
                           type = "risk",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    acm <- predict(model,
                   newdata = transf_fixed_vals, type = "risk")
    dNew$axis_vals <-
      dNew[, to_part] - comp_mean(
        dataset,
        comp_labels,
        rounded_zeroes = FALSE,
        det_limit = det_limit,
        units = units
      )[[to_part]]

    dNew$predictions <- dNew$fit / acm

    dNew$lower_CI <-
      dNew$predictions * exp(-1.96 * dNew$se.fit) / acm
    dNew$upper_CI <-
      dNew$predictions * exp(1.96 * dNew$se.fit) / acm

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
                        mapping = ggplot2::aes(x = axis_vals, y = predictions)) +
        ggplot2::ylim(yllimit, yulimit) +
        ggplot2::geom_errorbar(ggplot2::aes(
          x = axis_vals,
          ymin = lower_CI,
          ymax = upper_CI
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::geom_hline(yintercept = 1, size = 1) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.2),
          labels = seq(yllimit, yulimit, by = 0.2),
          limits = c(yllimit, yulimit)
        ) +
        theme_for_plots
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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        geom_hline(yintercept = 1) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
    }
  }









  if (type == "linear" && (terms == FALSE)) {
    message(
      "Note that the confidence intervals on this plot include uncertainty driven by other, non-compositional variables."
    )
    predictions <-
      predict(model,
              newdata = new_data,
              type = "response",
              se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$axis_vals <-
      dNew[, to_part] - comp_mean(
        dataset,
        comp_labels,
        rounded_zeroes = FALSE,
        det_limit = det_limit,
        units = units
      )[[to_part]]

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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(
          x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
          y = y_label
        ) +
        ggplot2::scale_y_continuous(
          trans = scales::log_trans(),
          breaks = seq(yllimit, yulimit, by = 0.1),
          labels = seq(yllimit, yulimit, by = 0.1)
        ) +
        ggplot2::geom_vline(xintercept = 0,
                            size = 1,
                            size = 1) +
        theme_for_plots
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
        ),
        color = "grey") +
        ggplot2::geom_point(size= 2) +
        ggplot2::labs(x = paste("More", from_part, "\U2194", "More", to_part, "\n " , units),
                      y = y_label) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        theme_for_plots
    }
  }










    if (type == "linear" && (terms)) {
      print(head(new_data))
      predictions <-
        predict(
          model,
          newdata = new_data,
          type = "terms",
          terms = transf_labels,
          se.fit = TRUE
        )



      acm <- predict(model,
                     newdata = transf_fixed_vals,
                     type = "terms",
                     terms = transf_labels)



      dNew <- data.frame(new_data, predictions)
      vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
      sum_for_args <- paste0(vector_for_args, collapse = "+")



      dNew$main <- eval(parse(text = sum_for_args))
      dNew$mean_vals <- rep(sum(acm), by = nrow(dNew))
      print(head(dNew$main))
      print(head(dNew$mean_vals))

      dNew$fit <- dNew$main - dNew$mean_vals

      print(head(dNew$fit))
      dNew$axis_vals <-
        dNew[, to_part] - comp_mean(
          dataset,
          comp_labels,
          rounded_zeroes = FALSE,
          det_limit = det_limit,
          units = units
        )[[to_part]]



      middle_matrix <- vcov(model)[transf_labels, transf_labels]
      x <- data.matrix(new_data[, transf_labels] - rep(cm_transf_df[, transf_labels], by = nrow(new_data)))
      in_sqrt_1 <- (x %*% middle_matrix)
      t_x <- as.matrix(t(x))
      in_sqrt_true <- c()
      for (i in 1:nrow(in_sqrt_1)) {
        in_sqrt_true <-
          c(in_sqrt_true, (in_sqrt_1[i,] %*% data.matrix(t_x)[, i]))
      }

      value <- sqrt(data.matrix(in_sqrt_true))

      t_value <-
        qt(0.975, df = (nrow(model.matrix(model)) -1 - length(transf_labels)))[[1]]

      dNew$lower_CI <- dNew$fit - t_value * value
      dNew$upper_CI <- dNew$fit + t_value * value

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
          ),
          color = "grey") +
          ggplot2::geom_point(size= 2) +
          ggplot2::labs(
            x = paste(
              "More",
              from_part,
              "\U2194",
              "More",
              to_part,
              "\n " ,
              units
            ),
            y = y_label
          ) +
          ggplot2::scale_y_continuous(
            trans = scales::log_trans(),
            breaks = seq(yllimit, yulimit, by = 0.1),
            labels = seq(yllimit, yulimit, by = 0.1)
          ) +
          ggplot2::geom_vline(xintercept = 0, size = 1) +
          theme_for_plots
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
          ),
          color = "grey") +
          ggplot2::geom_point(size= 2) +
          ggplot2::labs(x = paste(from_part, "to", to_part, "\n", units),
                        y = y_label) +
          ggplot2::geom_vline(xintercept = 0, size = 1) +
          theme_for_plots
      }
    }




    print("Please note that plotting may take some time.")
    if (terms == FALSE) {
      short_form <- gsub(".*~", "", as.character(formula(model)))
      print(paste("Covariate values were fixed at: "))
      variables <- strsplit(short_form[3], " + ", fixed = TRUE)[[1]]
      for (variable in variables[!(variables %in% transf_labels)]) {
        print(paste(variable, ":", fixed_values[1, variable]))
      }
    }
    print("Compositional variables not varied in the visualisation were fixed at:")
    for (variable in comp_labels) {
      print(paste(variable, ":", signif(fixed_values[1, variable], 2), units))
    }


    return(plot_of_this)
  }
