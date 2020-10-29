#' Predict fit and confidence interval
#'
#' Principally intended as input to forest_plot_examples and plot_transfers.
#'
#' @param model Model to use in generating predictions.
#' @param dataset  Should be dataset used to develop \code{model}.
#' @param det_limit Detection limit if zeroes are to be imputed. This must be set if \code{rounded_zeroes} is \code{TRUE} and should be the
#' minimum measurable value in the compositional columns of data. It should be on the same scale as the (input) compositional columns in \code{dataset} (NB it doesn't need to match \code{new_data}).
#' @param new_data Data for predictions.
#' @param terms Are estimates for differences in outcome associated with differences in compositional variables? If \code{terms = TRUE} all estimates and plots will be for difference in outcome associated with differences in the compositional variables. If \code{terms = FALSE}, \code{fixed_values} is used to set the values of the non-compositional covariates, and outputs are predictions for the outcome based on these values of the non-compositional covariates and the given value of the compositional variables (and confidence intervals include uncertainty due to all variables in the model, not just the compositional variables). If the model uses splines (from \code{comp_spline_model}), \code{terms} must be set to \code{FALSE}.
#' @param fixed_values If \code{terms = FALSE}, this is used as giving the fixed values of the non-compositional covariates at which to calculate the prediction. If it is not set, it can be automatically generated.
#' @inheritParams transform_comp
#' @inheritParams process_units
#' @param cm Can be set with compositional mean to speed up calculation. As it is easy to make mistakes using this, this should not be set manually and should only be passed from other functions.

#' @return Plot with balance of two parts plotted as exposure/ independent variable.
#' @export
#' @examples
#' lm_outcome <- comp_model(type = "linear",
#' outcome = "linear_outcome",
#' covariates = c("agegroup", "sex"),
#' data = simdata,
#' comp_labels = c("partA", "partB", "partC", "partD", "partE"))
#'
#' old_comp <- comp_mean(simdata,
#' comp_labels = c("partA", "partB", "partC", "partD", "partE"))
#' new_comp <-
#' change_composition(
#'  composition = old_comp,
#'  main_part = "partB",
#'  main_change = +0.5,
#'  comp_labels = c("partA", "partB", "partC", "partD", "partE")
#')
#'
#' predict_fit_and_ci(model = lm_outcome,
#' dataset = simdata,
#' new_data = new_comp,
#' comp_labels = c("partA", "partB", "partC", "partD", "partE"))
predict_fit_and_ci <- function(model,
                           dataset,
                           new_data,
                           comp_labels,
                           terms = TRUE,
                           fixed_values = NULL,
                           transformation_type = "ilr",
                           comparison_part = NULL,
                           part_1 = NULL,
                           units = "unitless",
                           specified_units = NULL,
                           rounded_zeroes = TRUE,
                           det_limit = NULL,
                           cm = NULL) {
  if (is.null(transformation_type)) {
    stop(
      "transformation_type must be specified and must match the transformation used in transform_comp earlier (which defaults to \"ilr\")"
    )
  }


  # We set units
  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]

  # We normalise
  det_limit <- rescale_det_limit(data = dataset, comp_labels = comp_labels, det_limit)
  dataset <- normalise_comp(dataset, comp_labels = comp_labels)
  new_data <- normalise_comp(new_data, comp_labels = comp_labels)

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
  if (is.null(cm)){
      cm <- comp_mean(
      dataset,
      comp_labels,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit,
      units = "unitless"
    )
  }
  cm_transf_df <- suppressMessages(transform_comp(cm, comp_labels,
                                 transformation_type = transformation_type,
                                 part_1 = part_1,
                                 comparison_part = comparison_part,
                                 rounded_zeroes = FALSE))



  # We assign some fixed_values to use in predicting
  if (!(is.null(fixed_values))) {
    if (length(colnames(fixed_values)[colnames(fixed_values) %in% comp_labels]) > 0) {
      message(
        "fixed_values will be updated to have compositional parts fixed at the compositional mean. For technical and pragmatic reasons, use of a different reference for the compositional parts is not currently possible."
      )
      fixed_values <- fixed_values[, colnames(fixed_values)[!(colnames(fixed_values) %in% comp_labels)]]
    }
    fixed_values <- cbind(fixed_values, cm)
  }
  if (is.null(fixed_values)) {
    fixed_values <-
      generate_fixed_values(
        dataset_ready,
        comp_labels,
        rounded_zeroes = rounded_zeroes,
        det_limit = det_limit
      )
    fixed_values <- cbind(fixed_values, cm)
  }

  transf_fixed_vals <- suppressMessages(transform_comp(
    fixed_values[, colnames(fixed_values)[!(colnames(fixed_values) %in% transf_labels)]],
    comp_labels,
    transformation_type = transformation_type,
    part_1 = part_1,
    comparison_part = comparison_part,
    rounded_zeroes = FALSE
  ))

  for (colname in colnames(fixed_values)){
    if (!(colname %in% colnames(new_data)) & !(colname %in% transf_labels)){
      new_data[, colname]<- rep(fixed_values[1, colname], by = nrow(new_data))
    }
  }

  new_data <- transform_comp(data = new_data, comp_labels = comp_labels, transformation_type = transformation_type, rounded_zeroes = FALSE, comparison_part = comparison_part, part_1 = part_1)

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

    dNew <- data.frame(new_data, predictions)

    vector_for_args <-
      paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")


    dNew$log_odds_change <- eval(parse(text = sum_for_args))
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
      terms = transf_labels, reference = "sample"
    )

    dNew <- data.frame(new_data, predictions)

    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")


    dNew$log_hazard_change <- eval(parse(text = sum_for_args))
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
                           type = "lp",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    dNew$fit <- exp(dNew$fit)

    dNew$lower_CI <-
      dNew$fit *exp(-(1.96 * dNew$se.fit))
    dNew$upper_CI <-
      dNew$fit* exp( +(1.96 * dNew$se.fit))
  }




  if (type == "linear" && !(terms)) {
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

    dNew <- data.frame(new_data, predictions)
    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")




    dNew$main <- eval(parse(text = sum_for_args))

    dNew$fit <- dNew$main




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

    dNew$lower_CI <- dNew$fit - t_value * value
    dNew$upper_CI <- dNew$fit + t_value * value
 }

  dNew <- rescale_comp(dNew, comp_labels = comp_labels, comp_sum = comp_sum)
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
