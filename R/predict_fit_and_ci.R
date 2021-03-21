#' Predict fit and confidence interval
#'
#' Principally intended as input to forest_plot_examples and plot_transfers.
#'
#' Note that confidence intervals use the t-distribution with the appropriate degrees of freedom for linear and logistic regression,
#' and the z-distribution for Cox regression, to match the apparent behaviour of \code{summary()} for these model objects. As long as there are a reasonable
#' number of samples (at least 30, say) the difference between the two is negligible.
#'
#' @param model Model to use for predictions.
#' @param new_data Data for predictions.
#' @param terms Are estimates for differences in outcome associated with differences in compositional variables? If \code{terms = TRUE} all estimates and plots will be for difference in outcome associated with differences in the compositional variables. If \code{terms = FALSE}, \code{fixed_values} is used to set the values of the non-compositional covariates, and outputs are predictions for the outcome based on these values of the non-compositional covariates and the given value of the compositional variables (and confidence intervals include uncertainty due to all variables in the model, not just the compositional variables).
#' @param fixed_values If \code{terms = FALSE}, this gives the fixed values of the non-compositional covariates at which to calculate the prediction. It is generated automatically if not set.
#' @inheritParams transform_comp
#' @inheritParams process_units
#' @return Plot with balance of two parts plotted as exposure/ independent variable.
#' @export
#' @examples
#' lm_outcome <- comp_model(type = "linear",
#' outcome = "BMI",
#' data = simdata,
#' covariates = c("agegroup", "sex"),
#' comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
#'
#' old_comp <- comp_mean(simdata,
#' comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
#' new_comp <-
#' change_composition(
#'  composition = old_comp,
#'  main_part = "moderate",
#'  main_change = +0.5,
#'  comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep")
#')
#'
#' predict_fit_and_ci(model = lm_outcome,
#' new_data = new_comp,
#' comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"))
predict_fit_and_ci <- function(model,
                           new_data,
                           comp_labels,
                           terms = TRUE,
                           fixed_values = NULL,
                           part_1 = NULL,
                           units = "unitless",
                           specified_units = NULL) {

  # We set units
  comp_sum <- as.numeric(process_units(units, specified_units)[2])
  units <- process_units(units, specified_units)[1]

  # We assign some internal parameters
  type <- process_model_type(model)

  # We normalise
  new_data <- normalise_comp(new_data, comp_labels = comp_labels)

  # We label what the transformed cols will be
  if (!is.null(part_1)) {
      comp_labels <- alter_order_comp_labels(comp_labels, part_1)
    }

  transf_labels <-
    transf_labels(comp_labels,
                  transformation_type = "ilr",
                  part_1 = part_1)

  # We back calculate the dataset used to derive the model
  dataset <- stats::model.frame(model)
      ## We verify that the correct column names are present
      if (!(all(transf_labels  %in% colnames(dataset)[grepl("ilr", colnames(dataset))]))){
        stop("Specified comp_labels do not match those used to develop the model (e.g. different order?)")
      }
      if (!(all(colnames(dataset)[grepl("ilr", colnames(dataset))] %in% transf_labels))){
        stop("Specified comp_labels do not match those used to develop the model (e.g. missing labels?)")
      }
  comp_cols <- ilr_trans_inv(dataset[, transf_labels])
  colnames(comp_cols) <- comp_labels
  dataset <- cbind(dataset, comp_cols)
  if (type == "cox"){
    strata_list <- colnames(dataset)[grepl("strata(",colnames(dataset) )]
    for (name in strata_list){
      plain <- gsub("strata(", "", name)
      plain <- gsub(")", "", name)
      dataset[, plain] <- dataset[, name]
    }
  }
  dataset_ready <-
    dataset[,!(colnames(dataset) %in% c(transf_labels, "survival_object"))]

  # We find the reference values
  mm <- stats::model.frame(model)[, transf_labels]
  cm_transf_df <- apply(mm, 2, mean)
  cm_transf_df <- as.data.frame(t(cm_transf_df))
  cm <- ilr_trans_inv(cm_transf_df)
  colnames(cm) <- comp_labels

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
        comp_labels
      )
    fixed_values <- cbind(fixed_values, cm)
  }

  transf_fixed_vals <- suppressMessages(transform_comp(
    fixed_values[, colnames(fixed_values)[!(colnames(fixed_values) %in% transf_labels)]],
    comp_labels,
    transformation_type = "ilr",
    part_1 = part_1,
    rounded_zeroes = FALSE
  ))

  for (colname in colnames(fixed_values)){
    if (!(colname %in% colnames(new_data)) & !(colname %in% transf_labels)){
      new_data[, colname]<- rep(fixed_values[1, colname], by = nrow(new_data))
    }
  }

  new_data <- suppressMessages(transform_comp(data = new_data, comp_labels = comp_labels, transformation_type = "ilr", rounded_zeroes = FALSE, part_1 = part_1))

  # Message about meaning of the 'terms' argument
  if (terms == FALSE){
    message(
      "Note that the confidence intervals on these predictions include uncertainty driven by other, non-compositional variables. To look at compositional variables only, use terms = TRUE"
    )
  }

  # We begin the plotting
  if ((type == "logistic") & !(terms)) {
    predictions <- stats::predict(model,
                           newdata = new_data,
                           type = "link",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    t_value <-
      stats::qt(0.975, df = stats::df.residual(model))[[1]]

    dNew$lower_CI <-
      model$family$linkinv(dNew$fit - t_value * dNew$se.fit)
    dNew$upper_CI <-
      model$family$linkinv(dNew$fit + t_value * dNew$se.fit)
    dNew$fit <- model$family$linkinv(dNew$fit)

  }

  if ((type == "logistic") & (terms)) {
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

    t_x <- data.matrix(as.matrix(t(x)))
    in_sqrt_true <- diag((x %*% middle_matrix) %*% t_x)
    value <- sqrt(data.matrix(in_sqrt_true))

    t_value <-
      stats::qt(0.975, df = stats::df.residual(model))[[1]]

    alpha_lower <- dNew$log_odds_change - t_value * value
    alpha_upper <- dNew$log_odds_change + t_value * value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)
  }



  if ((type == "cox") & (terms)) {
    predictions <- stats::predict(
      model,
      newdata = new_data,
      type = "terms",
      se.fit = TRUE,
      terms = transf_labels,
      reference = "sample"
    )

    dNew <- data.frame(new_data, predictions)

    vector_for_args <-   paste("dNew$fit.", transf_labels, sep = "")
    sum_for_args <- paste0(vector_for_args, collapse = "+")


    dNew$log_hazard_change <- eval(parse(text = sum_for_args))
    dNew$fit <- exp(dNew$log_hazard_change)

    middle_matrix <- stats::vcov(model)[transf_labels, transf_labels]
    x <- data.matrix(new_data[, transf_labels] - rep(cm_transf_df[, transf_labels], by = nrow(new_data)))
    t_x <- data.matrix(as.matrix(t(x)))

    in_sqrt_true <- diag((x %*% middle_matrix) %*% t_x)
    value <- sqrt(data.matrix(in_sqrt_true))

    z_value <- stats::qnorm(0.975)

    alpha_lower <- dNew$log_hazard_change - z_value*value
    alpha_upper <- dNew$log_hazard_change + z_value*value

    dNew$lower_CI <- exp(alpha_lower)
    dNew$upper_CI <- exp(alpha_upper)
  }



  if ((type == "cox") & !(terms)) {
    predictions <- stats::predict(model,
                           newdata = new_data,
                           type = "lp",
                           se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    dNew$fit <- exp(dNew$fit)

    z_value <- stats::qnorm(0.975)

    dNew$lower_CI <-
      dNew$fit *exp(-(z_value * dNew$se.fit))
    dNew$upper_CI <-
      dNew$fit* exp( +(z_value * dNew$se.fit))
  }




  if ((type == "linear") & !(terms)) {
    predictions <-
      stats::predict(model,
              newdata = new_data,
              type = "response",
              se.fit = TRUE)

    dNew <- data.frame(new_data, predictions)

    t_value <-
      stats::qt(0.975, df = stats::df.residual(model))[[1]]


    dNew$lower_CI <- dNew$fit - t_value * dNew$se.fit
    dNew$upper_CI <- dNew$fit + t_value * dNew$se.fit


  }






  if ((type == "linear") & (terms)) {
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

    t_x <- data.matrix(as.matrix(t(x)))
    in_sqrt_true <- diag((x %*% middle_matrix) %*% t_x)
    value <- sqrt(data.matrix(in_sqrt_true))

    t_value <-
      stats::qt(0.975, df = stats::df.residual(model))[[1]]

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
