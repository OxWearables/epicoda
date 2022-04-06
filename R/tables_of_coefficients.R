#' Tabulate coefficients associated with covariates
#'
#' This pulls out a table of the coefficients of the covariates only in the model. Confidence intervals are calculated using `confint`.  The function may fail for logistic regression if the MASS package is not installed.
#'
#' @param model Model for which extracting coefficients.
#' @param scale_type Can take value "lp" (linear predictors) or "exp" (exponentiated- relevant for interpretation of logistic and Cox models).
#' @param level The level argument of the confidence intervals. Passed directly to `stats::confint`.
#' @inheritParams comp_model
#'
#' @return Table of covariates and their associated coefficients (parameter value with lower and upper confidence interval). Note that, unlike the plotting and some other default model summary functions in R, this uses confidence intervals based on stats::confint.
#' @export
tab_covariate_coefs <-
  function(model = NULL,
           scale_type = "lp",
           level = 0.95,
           comp_labels,
           part_1 = NULL) {
    transf_labels <-
      transf_labels(
        comp_labels = comp_labels,
        transformation_type = "ilr",
        part_1 = part_1
      )

    toc <- data.frame(stats::coef(model, complete = FALSE))
    colnames(toc) <- "fit"

    tocint <- stats::confint(model, level = level, complete = FALSE)

    all <- cbind(toc, tocint)
    all_red <- all[!(rownames(all) %in% transf_labels), ]

    if (scale_type == "lp") {
      all_red <- all_red
    }
    if (scale_type == "exp") {
      all_red <- exp(all_red)
    }
    if ((scale_type != "lp")&& (scale_type != "exp")) {
      stop("scale_type has unrecognised value.")
    }
    return(all_red)
  }

#' Tabulate model using ilr pivot coordinate method
#'
#' This pulls out a table of the coefficients of the models. Confidence intervals are calculated using `confint`. The function may fail for logistic regression if the MASS package is not installed.
#'
#' @inheritParams tab_covariate_coefs
#' @inheritParams comp_model
#'
#' @return Table of all model coefficients (parameter value with lower and upper confidence interval of the 95% ), using the first pivot coordinates for all compositional coefficients.
#' @examples tab_coefs(scale_type = "lp", # This argument can be "lp" or "exp" and determines whether
#' # coefficients are presented on the scale of the linear predictors ("lp")
#' # or are exponentiated ("exp"). Exponentiation gives the Odds Ratio for
#' # logistic regression models and the Hazard Ratio for Cox regression models.
#' level = 0.95,
#' type = "linear",
#' outcome = "BMI",
#' covariates = c("agegroup", "sex"),
#' data = simdata,
#' comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep" ),
#' rounded_zeroes = FALSE
#' )
#' @export
tab_coefs <-
  function(scale_type = "lp",
           level = 0.95,
           type = NULL,
           outcome = NULL,
           covariates = NULL,
           follow_up_time = NULL,
           event = NULL,
           data,
           comp_labels,
           rounded_zeroes = TRUE,
           det_limit = NULL) {
    if ((scale_type == "exp") && (type == "linear")){
      warning("It usually does not make sense to exponentiate the coefficients of a linear model.")
    }
    if ((scale_type == "lp") && (type == "logistic")){
      warning("Coefficients are on the scale of the linear predictors. For ORs, consider setting scale_type as exp.")
    }
    if ((scale_type == "lp") && (type == "cox")){
      warning("Coefficients are on the scale of the linear predictors. For HRs, consider setting scale_type as exp.")
    }
    start_model <- comp_model(
      type = type,
      outcome = outcome,
      covariates = covariates,
      follow_up_time = follow_up_time,
      event = event,
      data  = data,
      comp_labels = comp_labels,
      rounded_zeroes = rounded_zeroes,
      det_limit = det_limit
    )

    master <- tab_covariate_coefs(
      model = start_model,
      scale_type = scale_type,
      level = level,
      comp_labels = comp_labels
    )
    for (part in comp_labels) {
      current_model <- comp_model(
        type = type,
        outcome = outcome,
        covariates = covariates,
        follow_up_time = follow_up_time,
        event = event,
        data  = data,
        comp_labels = comp_labels,
        rounded_zeroes = rounded_zeroes,
        det_limit = det_limit,
        part_1 = part
      )
      transf_labels <-
        transf_labels(
          comp_labels = comp_labels,
          transformation_type = "ilr",
          part_1 = part
        )
      tl_needed <- transf_labels[1]

      toc <- data.frame(stats::coef(current_model, complete = FALSE))
      colnames(toc) <- "fit"
      tocint <-
        stats::confint(current_model, level = level, complete = FALSE)
      all <- cbind(toc, tocint)
      all_red <- all[tl_needed, ]
      rownames(all_red) <-  paste("First pivot coordinate:", part, "vs All other parts")
        if (scale_type == "lp") {
          all_red <- all_red
        }
      if (scale_type == "exp") {
        all_red <- exp(all_red)
      }
      if ((scale_type != "lp") && (scale_type != "exp")) {
        stop("scale_type has unrecognised value.")
      }
      master <- rbind(master, all_red)


    }
  return(master)


  }
