#' Predict fit and confidence interval
#'
#' Principally intended as input to forest_plot_examples and plot_transfers.
#'
#' @param model Model to use in generating predictions.
#' @param dataset  Should be dataset used to develop \code{model}.
#' @param new_data Data for predictions.
#' @param fixed_values If \code{terms = FALSE}, this is used as giving the fixed values of the non-compositional covariates at which to calculate the prediction. If it is not set, it can be automatically generated.
#' @inheritParams transform_comp
#' @inheritParams process_units
#' @param terms Are predictions for terms,or are they absolute?
#' @return Plot with balance of two parts plotted as exposure/ independent variable.
#' @examples
predict_fit_and_ci <- function(model,
                           dataset,
                           new_data,
                           fixed_values = NULL,
                           transformation_type = NULL,
                           comparison_part = NULL,
                           part_1 = NULL,
                           comp_labels,
                           units = "unitless",
                           specified_units = NULL,
                           rounded_zeroes = NULL,
                           det_limit = NULL,
                           terms = TRUE) {
  if (is.null(transformation_type)) {
    stop(
      "transformation_type must be specified and must match the transformation used in transform_comp earlier (which defaults to \"ilr\")"
    )
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

  # We calculate the compositional mean so we can use it in future calculations
  cm <- comp_mean(
      dataset,
      comp_labels,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit,
      units = units,
      specified_units = specified_units
    )
  cmdf <- data.frame(cm)
  cm_transf_df <- transform_comp(cmdf, comp_labels,
                                 transformation_type = transformation_type,
                                 part_1 = part_1,
                                 comparison_part = comparison_part,
                                 rounded_zeroes = rounded_zeroes, det_limit = det_limit)



  # We assign some fixed_values to use in predicting

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
        dataset_ready,
        comp_labels,
        rounded_zeroes = rounded_zeroes,
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
    rounded_zeroes = rounded_zeroes,
    det_limit = det_limit,
    units = units,
    specified_units = specified_units
  )


  for (colname in colnames(fixed_values)){
    if (!(colname %in% colnames(new_data)) & !(colname %in% transf_labels)){
      new_data[, colname]<- rep(fixed_values[1, colname], by = nrow(new_data))
    }
  }

  new_data <- transform_comp(data = new_data, comp_labels = comp_labels, transformation_type = transformation_type, rounded_zeroes = rounded_zeroes, det_limit = det_limit, comparison_part = comparison_part, part_1 = part_1)

  # We begin the plotting
  if (type == "logistic" && (terms == FALSE)) {
    message(
      "Note that the confidence intervals on these predictions include uncertainty driven by other, non-compositional variables. To look at compositional variables only, use terms = TRUE"
    )
    predictions <- stats::predict(model,
                           newdata = new_data,
                           type = "link",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)
    dNew$normalised_predictions <- model$family$linkinv(dNew$fit)

    dNew$lower_CI <-
      model$family$linkinv(dNew$fit - 1.96 * dNew$se.fit)
    dNew$upper_CI <-
      model$family$linkinv(dNew$fit + 1.96 * dNew$se.fit)


  }






  if (type == "logistic" && (terms)) {
    predictions <-
      stats::predict(
        model,
        newdata = new_data,
        type = "terms",
        terms = transf_labels,
        se.fit = TRUE
      )

    acm <- stats::predict(model,
                   newdata = transf_fixed_vals,
                   type = "terms",
                   terms = transf_labels)


    dNew <- data.frame(new_data, predictions)

    vector_for_args <-
      paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")


    dNew$log_odds_change <- eval(parse(text = sum_for_args)) - sum(acm)
    dNew$fit <- exp(dNew$log_odds_change)


    middle_matrix <- stats::vcov(model)[transf_labels, transf_labels]
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
      stats::qt(0.975, df = (nrow(stats::model.matrix(model)) -1 - length(transf_labels)))[[1]]


    alpha_lower <- dNew$log_odds_change - t_value * value
    alpha_upper <- dNew$log_odds_change + t_value * value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)

  }








  if (type == "cox" && (terms)) {
    predictions <- stats::predict(
      model,
      newdata = new_data,
      type = "terms",
      se.fit = TRUE,
      terms = transf_labels
    )

    acm <- stats::predict(model,
                   newdata = transf_fixed_vals,
                   type = "terms",
                   terms = transf_labels)


    dNew <- data.frame(new_data, predictions)

    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")



    dNew$log_hazard_change <- eval(parse(text = sum_for_args)) - sum(acm)
    dNew$fit <- exp(dNew$log_hazard_change)

    middle_matrix <- stats::vcov(model)[transf_labels, transf_labels]
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
      stats::qt(0.975, df = (nrow(stats::model.matrix(model)) -1 - length(transf_labels)))[[1]]



    alpha_lower <- dNew$log_hazard_change - t_value*value
    alpha_upper <- dNew$log_hazard_change + t_value*value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)
  }





  if (type == "cox" && !(terms)) {
    predictions <- stats::predict(model,
                           newdata = new_data,
                           type = "risk",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    acm <- stats::predict(model,
                   newdata = transf_fixed_vals, type = "risk")

    dNew$predictions <- dNew$fit / acm

    dNew$lower_CI <-
      dNew$fit * exp(-1.96 * dNew$se.fit) / acm
    dNew$upper_CI <-
      dNew$fit * exp(1.96 * dNew$se.fit) / acm
  }









  if (type == "linear" && (terms == FALSE)) {
    message(
      "Note that the confidence intervals on this plot include uncertainty driven by other, non-compositional variables."
    )
    predictions <-
      stats::predict(model,
              newdata = new_data,
              type = "response",
              se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)


    dNew$lower_CI <- dNew$fit - 1.96 * dNew$se.fit
    dNew$upper_CI <- dNew$fit + 1.96 * dNew$se.fit


  }










  if (type == "linear" && (terms)) {
    predictions <-
      stats::predict(
        model,
        newdata = new_data,
        type = "terms",
        terms = transf_labels,
        se.fit = TRUE
      )



    acm <- stats::predict(model,
                   newdata = transf_fixed_vals,
                   type = "terms",
                   terms = transf_labels)



    dNew <- data.frame(new_data, predictions)
    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")



    dNew$main <- eval(parse(text = sum_for_args))
    dNew$mean_vals <- rep(sum(acm), by = nrow(dNew))

    dNew$fit <- dNew$main - dNew$mean_vals





    middle_matrix <- stats::vcov(model)[transf_labels, transf_labels]
    x <- data.matrix(new_data[, transf_labels] - rep(cm_transf_df[, transf_labels], by = nrow(new_data)))
    # print(head(as.data.frame(rep(cm_transf_df[, transf_labels], by = nrow(new_data)), new_data[ , c("agegroup", "sex")])))
    #
    #
    # dNew$fit <-
    #   stats::predict(
    #     model,
    #     newdata = cbind(new_data[, transf_labels] - as.data.frame(rep(cm_transf_df[, transf_labels], by = nrow(new_data))), new_data[ , c("agegroup", "sex")]),
    #     type = "terms",
    #     terms = transf_labels,
    #     se.fit = TRUE
    #   )
    in_sqrt_1 <- (x %*% middle_matrix)
    t_x <- as.matrix(t(x))
    in_sqrt_true <- c()
    for (i in 1:nrow(in_sqrt_1)) {
      in_sqrt_true <-
        c(in_sqrt_true, (in_sqrt_1[i,] %*% data.matrix(t_x)[, i]))
    }

    value <- sqrt(data.matrix(in_sqrt_true))

    t_value <-
      stats::qt(0.975, df = (nrow(stats::model.matrix(model)) -1 - length(transf_labels)))[[1]]

    dNew$lower_CI <- dNew$fit - t_value * value
    dNew$upper_CI <- dNew$fit + t_value * value
 }


  if (terms == FALSE) {
    short_form <- gsub(".*~", "", as.character(stats::formula(model)))
    print(paste("Covariate values were fixed at: "))
    variables <- strsplit(short_form[3], " + ", fixed = TRUE)[[1]]
    for (variable in variables[!(variables %in% transf_labels)]) {
      print(paste(variable, ":", fixed_values[1, variable]))
    }
  }



  return(dNew)
}
