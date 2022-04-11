#' @title
#' Statistical models with compositional exposure variables
#'
#' @description
#' This is a wrapper for `lm()`, `glm()` and `survival::coxph()` which performs
#' the compositional transformation before generating the model.
#'
#' @param type Type of statistical model to use. It should be one of `"linear"`,
#' `"logistic"`, `"cox"`.
#' @param outcome Column name of outcome variable in data. If `type` is
#' `"linear"`, this should be a continuous variable.  If `type` is `"logistic"`,
#' this should be a binary outcome. If `type` is `"cox"`, if this is set it
#' should be a `Surv` object from package _survival_ (if it is set, the function
#' defaults to attempt to use it, even if `follow_up_time` and `event` are set).
#' If this is left as `NULL`, `follow_up_time` and `event` can be set instead.
#' @param covariates Character vector of column names of covariates to adjust
#' models for. As this is used as a character vector, special arguments to the
#' standard models (like `strata(variable)` for a Cox model) can be used here.
#' @param follow_up_time Only used if `type` is `"cox"` and `outcome` is `NULL`.
#' Follow-up time.
#' @param event Only used if `type` is `"cox"` and `outcome` is `NULL`.Binary
#' variable indicating whether or not an event was observed.
#' @param data Dataset to use for modelling.
#' @inheritParams transform_comp
#'
#' @return
#' Model using a compositional exposure variable (`lm`, `glm` or `coxph` object
#' as appropriate).
#' 
#' @importFrom stats as.formula glm lm
#' @importFrom survival coxph Surv
#' 
#' @examples
#' comp_model(
#'   type = "linear",
#'   outcome = "BMI",
#'   covariates = c("agegroup", "sex"),
#'   comp_labels = c("vigorous", "moderate", "light", "sedentary", "sleep"),
#'   data = simdata,
#'   det_limit = 0.00119
#' )

#' @export
comp_model <- function(
  type = NULL,
  outcome = NULL,
  covariates = NULL,
  comp_labels,
  data,
  follow_up_time = NULL,
  event = NULL,
  rounded_zeroes = TRUE,
  det_limit = NULL,
  part_1 = NULL
) {

  # Check information specified is correct
  type <- match.arg(type, c("cox", "linear", "logistic"))

  # Transform data
  data_ready <- transform_comp(
    data = data,
    comp_labels = comp_labels,
    transformation_type = "ilr",
    rounded_zeroes = rounded_zeroes,
    det_limit = det_limit,
    part_1 = part_1
  )

  # Create  strings to specify exposure variables in models
  transf_vec <- transf_labels(
    comp_labels = comp_labels,
    transformation_type = "ilr",
    part_1 = part_1
  )
  transf_sum <- vector_to_sum(transf_vec)
  cov_sum <- vector_to_sum(covariates)

  # Perform modelling by type
  if (type == "linear") {
    
    # Linear Regression
    if (is.null(outcome)) {
      stop('`outcome` must be set.')
    }
    if (is.null(covariates)) {
      model <- lm(
        formula = as.formula(paste(outcome, "~", transf_sum)),
        data = data_ready
      )
    } else {
      model <- lm(
        formula = as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
        data = data_ready
      )
    }

    } else if (type == "logistic") {
      
      # Logistic Regression
      if (is.null(outcome)) {
        stop('`outcome` must be set.')
      }
      if (is.null(covariates)) {
        model <- glm(
          fomula = as.formula(paste(outcome, "~", transf_sum)),
          data = data_ready,
          family = "binomial"
        )
      } else {
        model <- glm(
          as.formula(paste(outcome, "~", cov_sum, "+", transf_sum)),
          data = data_ready,
          family = "binomial"
        )
      }

    } else {
      
      # Cox-PH
      if (is.null(outcome)) {
        if (is.null(follow_up_time) || is.null(event)){
          stop('`follow_up_time` and `event`, or `outcome`, must be set.')
        }
        survival_object <- Surv(data_ready[, follow_up_time], data_ready[, event])
      }
      if (!(is.null(outcome))) {
        survival_object <- outcome
      }
      if (is.null(covariates)) {
        model <- coxph(
          fomula = as.formula(paste("survival_object", "~",  transf_sum)),
          data = data_ready,
          model = TRUE
        )
      }
      if (!is.null(covariates)) {
        model <- coxph(
          formula = as.formula(paste("survival_object", "~", cov_sum, "+", transf_sum)),
          data = data_ready,
          model = TRUE
        )
      }
    }

    # Return model
    return(model)

}
